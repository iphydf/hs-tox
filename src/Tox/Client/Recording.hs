{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | What changed at the home, as events for its devices: the account's and
-- friends' facts, friends, groups, invites, messages and what became of
-- them, and how far the chats were read.
module Tox.Client.Recording
  ( Recorded (..)
  , record
  , renamedIn
  , eventOf
  , liveOf
  , writeAll
  ) where

import           Control.Monad                    (forM_, unless, when)
import           Control.Monad.Trans.State.Strict (State, execState, gets,
                                                   modify', state)
import qualified Data.ByteString                  as BS
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe, isJust, isNothing,
                                                   listToMaybe)
import qualified Data.Set                         as Set
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import           Data.Time                        (UTCTime)
import qualified Tox.Application.Tox              as Tox
import qualified Tox.Sync.Account                 as Sync
import qualified Tox.Sync.Event                   as Sync
import qualified Tox.Sync.Fact                    as Sync
import qualified Tox.Sync.Id                      as Sync
import qualified Tox.Sync.Setting                 as Sync
import qualified Tox.Sync.Store                   as Sync

import           Tox.Client
import qualified Tox.Client.Home                  as Home
import           Tox.Client.Home                  (Home)
import           Tox.Client.Linked                (toMillis)
import           Tox.Client.Synced
import           Tox.Client.Types

data Recorded = Recorded
  { recordedHome    :: Home
  , recordedNew     :: [(Sync.EventId, BS.ByteString)]
    -- ^ Events for the devices: those written and those taken.
  , recordedRenames :: Map MessageId MessageId
    -- ^ New messages and group entries take their event's ID.
  , recordedLive    :: [(Sync.PublicKey, [BS.ByteString])]
    -- ^ Live facts for the devices in session, if they changed.
  , recordedBlobs   :: [BS.ByteString]
    -- ^ Avatars to store, so devices can fetch them.
  , recordedHolds   :: [(ContactId, MessageId, FilePath)]
    -- ^ Files that ended here, to store as blobs and end with.
  }

-- | What the recording pass has done so far.
data Pass = Pass
  { passHome    :: Home
  , passNew     :: [(Sync.EventId, BS.ByteString)]
    -- ^ Newest first.
  , passRenames :: Map MessageId MessageId
  , passHolds   :: [(ContactId, MessageId, FilePath)]
  }

-- | A new event, after everything we have of its graph.
write :: UTCTime -> Sync.GraphId -> Sync.Content -> State Pass Sync.EventId
write time g c = state $ \p ->
  let (i, bytes, h) = Home.write g time c (passHome p)
  in (i, p { passHome = h, passNew = (i, bytes) : passNew p })

-- | Our new events at once, after everything we have of their graphs.
writeAll :: UTCTime -> [(Sync.GraphId, Sync.Content)] -> Home -> ([(Sync.EventId, BS.ByteString)], Home)
writeAll time contents h = (reverse (passNew p), passHome p)
  where p = execState (mapM_ (uncurry (write time)) contents) (Pass h [] Map.empty [])

-- | The event of a message or group entry: the message's own ID, or for one
-- from before we were the home, the event recorded for it.
eventOf :: Home -> MessageId -> Maybe Sync.EventId
eventOf h (MessageId bytes)
  | Sync.member (Sync.EventId bytes) (Home.homeStore h) = Just (Sync.EventId bytes)
  | otherwise = Map.lookup bytes (Home.homeEarlier h)

-- | Messages and entries under the IDs they took.
renamedIn :: Map MessageId MessageId -> Client -> Client
renamedIn renames c = c
  { clientContacts = (\contact -> contact { contactMessages = [m { messageId = renamed (messageId m) } | m <- contactMessages contact] })
      <$> clientContacts c
  , clientCall = (\call -> call { callEntry = renamed <$> callEntry call }) <$> clientCall c
  , clientGroups = (\g -> g { groupEntries = [e { entryId = renamed (entryId e) } | e <- groupEntries g] }) <$> clientGroups c }
  where renamed i = Map.findWithDefault i i renames

-- | What the devices in session see as it happens.
liveOf :: Client -> Sync.Facts
liveOf client = foldr (uncurry Sync.setFact) Sync.noFacts $
  (Sync.AccountSubject, Sync.ConnectionValue (connection (clientConnection client)))
  -- Devices take calls through us.
  : (Sync.AccountSubject, Sync.CallRelay True)
  -- Ringing until here or a device takes it.
  : [ (Sync.FriendSubject pk, Sync.CallValue (if callPhase c == Ringing then Sync.Ringing (callPeerVideo c) else Sync.Answered))
    | Just c <- [clientCall client], Just pk <- [contactKey (callContact c)] ]
  ++ concat [ [(s, Sync.ConnectionValue Sync.Udp), (s, Sync.Status (toEnum (fromEnum (contactStatus c))))]
             ++ [(s, Sync.Typing True) | Set.member cid (clientTyping client)]
           | (cid, c) <- Map.toList (clientContacts client), contactOnline c, Just pk <- [contactKey cid]
           , let s = Sync.FriendSubject pk ]
  ++ concat [ [(s, Sync.Present True), (s, Sync.Name (memberName m))]
              ++ [(s, Sync.Status (toEnum (fromEnum st))) | Just st <- [Map.lookup n (groupStatuses g)]]
              ++ [(s, Sync.RoleValue (syncRole r)) | Just r <- [Map.lookup n (groupRoles g)]]
            | (gid, g) <- Map.toList (clientGroups client), groupPhase g == InGroup, Just c <- [chatIdOf gid g]
            , (n, m) <- Map.toList (groupMembers g), Just pk <- [keyOf (memberKey m)]
            , let s = Sync.PeerSubject c pk ]
  where
    connection = \case
      Offline -> Sync.NoConnection
      ViaTcp  -> Sync.Tcp
      ViaUdp  -> Sync.Udp

-- | Writes what changed from the old client to the new as events, after the
-- ones a device sent that we took. With 'paging', messages we didn't have
-- came from the store, so they aren't new.
record :: Bool -> [(Sync.EventId, BS.ByteString)] -> Home -> Client -> Client -> Recorded
record paging taken h0 old client = Recorded
  { recordedHome = home { Home.homeBackfill = False, Home.homeFull = False, Home.homeJoining = [] }
  , recordedNew = new
  , recordedRenames = passRenames done
  , recordedLive = live
  , recordedBlobs =
      [ png | (i, _) <- new, Just e <- [Sync.lookupEvent i (Home.homeStore (passHome done))]
      , Sync.FactContent _ (Sync.Avatar (Just (Sync.BlobId blob))) <- [Sync.eventContent e]
      , Just png <- [Map.lookup blob (clientAvatars client)] ]
  , recordedHolds = reverse (passHolds done)
  }
  where
    done = execState pass (Pass h0 [] Map.empty [])
    new = taken ++ reverse (passNew done)
    (live, home) = fromMaybe ([], passHome done) (Home.liveChanged (liveOf (renamedIn (passRenames done) client)) (passHome done))
    pass = do
      mapM_ (uncurry (write clock)) (facts h0)
      forM_ friends $ \(cid, pk, contact) ->
        when (full || Map.lookup cid (clientContacts old) /= Just contact) $
          mapM_ (message cid pk contact) (contactMessages contact)
      forM_ groups $ \(gid, c, g) ->
        when (changedGroup gid g) $ mapM_ (entry (Sync.chatGraph c) gid) (groupEntries g)
      invites
      reads' <- gets (readWrites . passHome)
      mapM_ (uncurry (write clock)) reads'

    account = Home.homeAccount h0
    accountG = Sync.accountGraph account
    state' = Home.homeState
    clock = Home.homeClock h0
    full = Home.homeFull h0
    profile = clientProfile client
    friends = [ (cid, pk, contact) | (cid, contact) <- Map.toList (clientContacts client), Just pk <- [contactKey cid]
              , not (Home.isDevice pk h0), pk /= account ]
    -- Joined ones, and those we left or were turned away from.
    groups = [(gid, c, g) | (gid, g) <- Map.toList (clientGroups client), Just c <- [chatIdOf gid g]]
    changedGroup gid g = full || Map.lookup gid (clientGroups old) /= Just g
    has s v h = v `elem` Sync.factsOf s (Sync.accountFacts (state' h))

    -- Facts of the account, its friends and groups, the friends, and
    -- settings.
    facts h = accountWrites h ++ friendWrites h ++ groupWrites h
    -- None is only recorded for a subject that had one.
    avatarWrite s hash h = case (Sync.BlobId <$> hash, [b | Sync.Avatar b <- Sync.factsOf s (Sync.accountFacts (state' h))]) of
      (Nothing, [])          -> []
      (Nothing, Nothing : _) -> []
      (blob, _) | has s (Sync.Avatar blob) h -> []
                | otherwise -> [(accountG, Sync.FactContent s (Sync.Avatar blob))]
    accountWrites h =
      [ (accountG, Sync.FactContent Sync.AccountSubject v)
      | v <- [ Sync.Name (profileNickname profile), Sync.StatusMessage (profileStatus profile)
             , Sync.Status (toEnum (fromEnum (profileAway profile))) ]
             ++ [Sync.Address a | Just a <- [unhex' (profileToxId profile)], BS.length a == 38]
      , not (has Sync.AccountSubject v h) ]
      ++ avatarWrite Sync.AccountSubject (clientAvatar client) h
    friendWrites h =
      [ (accountG, c)
      | (_, pk, contact) <- friends
      , c <- [Sync.WantFriend pk Nothing Nothing | not (maybe False Sync.friendWanted (Map.lookup pk (Sync.accountFriends (state' h))))]
          ++ [ Sync.FactContent (Sync.FriendSubject pk) v
             | v <- [Sync.Name n | Just n <- [contactName contact]]
                    ++ [Sync.StatusMessage (contactStatusMessage contact) | not (Text.null (contactStatusMessage contact)) || hasStatusMessage pk h]
             , not (has (Sync.FriendSubject pk) v h) ]
          ++ map snd (avatarWrite (Sync.FriendSubject pk) (contactAvatar contact) h)
          ++ friendSettingWrites pk contact h ]
      ++ [ (accountG, Sync.FriendRequest pk asked)
         | (cid, asked) <- Map.toList (clientRequests client), Just pk <- [contactKey cid]
         , maybe True (\f -> not (Sync.friendWanted f) && Sync.friendRequest f /= Just asked) (Map.lookup pk (Sync.accountFriends (state' h))) ]
      -- Gone here: deleted, or a request declined.
      ++ [ (accountG, Sync.UnwantFriend pk)
         | (pk, f) <- Map.toList (Sync.accountFriends (state' h))
         , Sync.friendWanted f || isJust (Sync.friendRequest f)
         , let cid = contactIdOf pk
         , Map.notMember cid (clientContacts client), Map.notMember cid (clientRequests client)
         , not (Home.isDevice pk h), pk /= account ]
    hasStatusMessage pk h = not (null [() | Sync.StatusMessage _ <- Sync.factsOf (Sync.FriendSubject pk) (Sync.accountFacts (state' h))])
    settingOf kind g h = Sync.keptValue <$> (Map.lookup kind =<< Map.lookup (Sync.ConversationScope g) (Sync.accountSettings (state' h)))
    muteSetting mentioned = \case
      MutedUntil t -> Sync.Mute (Just (fromIntegral (toMillis t))) mentioned
      MutedAlways  -> Sync.Mute Nothing mentioned
    setting g kind = maybe (Sync.ClearSetting (Sync.ConversationScope g) (fromIntegral kind)) (Sync.SettingContent (Sync.ConversationScope g) . Sync.kept)
    friendSettingWrites pk contact h =
      let g = Sync.friendGraph account pk
          mute = muteSetting True <$> contactMute contact
          pictures = Sync.PictureFormat <$> (syncPictures =<< contactPictures contact)
          -- Friend chats have no mentions.
          until' = \case
            Just (Sync.Mute t _) -> Just t
            _                    -> Nothing
      in [ setting g muteKind mute | until' mute /= until' (settingOf muteKind g h) ]
         ++ [ setting g picturesKind pictures | pictures /= settingOf picturesKind g h ]
    groupWrites h =
      [ (accountG, Sync.FactContent (Sync.ChatSubject c) v)
      | (_, c, g) <- groups
      , let joined = groupPhase g `elem` [Joining, InGroup]
      , v <- [Sync.Joined joined | joined || has (Sync.ChatSubject c) (Sync.Joined True) h]
             ++ [Sync.Name (groupName g) | not (Text.null (groupName g))]
             ++ [Sync.Topic (groupTopic g) | groupKind g == NgcGroup, not (Text.null (groupTopic g))]
             ++ [Sync.RoleValue (syncRole r) | Just r <- [groupSelfRole g]]
             ++ concat [ [ Sync.PrivacyValue (if private then Sync.Private else Sync.Public), Sync.Password (Text.decodeUtf8Lenient password)
                         , Sync.PeerLimit (fromIntegral limit), Sync.TopicLock locked, Sync.VoiceStateValue (syncVoice voice) ]
                       | Just (Tox.GroupSettings private password limit locked voice) <- [groupSettings g] ]
      , not (has (Sync.ChatSubject c) v h) ]
      -- Public groups devices asked to join: we're joining, or Tox refused.
      ++ [ (accountG, Sync.ChatJoined i (Sync.GroupChat chatId <$ Map.lookup (GroupId (hexOf chatId)) (clientGroups client)))
         | (i, chatId) <- Home.homeJoining h ]
      -- Deleted here.
      ++ [ (accountG, Sync.FactContent (Sync.ChatSubject c) (Sync.Joined False))
         | c <- Sync.joinedChats (state' h), Map.notMember (chatGroupId c) (clientGroups client) ]
      ++ concat [ groupSettingWrites c g h | (_, c, g) <- groups ]
    groupSettingWrites c g h =
      let graph = Sync.chatGraph c
          mute = muteSetting (groupMentions g) <$> groupMute g
          pictures = Sync.PictureFormat <$> (syncPictures =<< groupPictures g)
          bot = Map.lookup c (Sync.accountRelayBots (state' h))
          wanted = keyOf =<< groupRelayBot g
      in [ (accountG, setting graph muteKind mute) | mute /= settingOf muteKind graph h ]
         ++ [ (accountG, setting graph picturesKind pictures) | pictures /= settingOf picturesKind graph h ]
         ++ [ (accountG, Sync.RelayBot c pk True) | Just pk <- [wanted], wanted /= bot ]
         ++ [ (accountG, Sync.RelayBot c pk False) | Just pk <- [bot], isNothing wanted ]

    -- Each message we have, and what became of it.
    message cid pk contact m = do
      h <- gets passHome
      let g = Sync.friendGraph account pk
          isNew = Set.notMember (messageId m) (oldMessages cid) && not paging
          content = contentOf contact m
      found <- case eventOf h (messageId m) of
        Just i -> pure (Just i)
        Nothing
          | isNew || Home.homeBackfill h -> Just <$> written isNew (messageId m) g (messageTime m) content
          | changing m, Just i <- matching h g (messageTime m) content -> Just i <$ earlier (messageId m) i
          | otherwise -> pure Nothing
      mapM_ (answers cid g m) found
    oldMessages cid = maybe Set.empty (Set.fromList . map messageId . contactMessages) (Map.lookup cid (clientContacts old))
    -- A new one takes the event's ID. One from before we were the home keeps
    -- its own.
    written isNew mid g time content = do
      i@(Sync.EventId bytes) <- write time g content
      if isNew then modify' (\p -> p { passRenames = Map.insert mid (MessageId bytes) (passRenames p) })
      else earlier mid i
      pure i
    earlier (MessageId bytes) i = modify' $ \p ->
      p { passHome = (passHome p) { Home.homeEarlier = Map.insert bytes i (Home.homeEarlier (passHome p)) } }
    -- What answers our message, file or call, once each.
    answers cid g m i = do
      h <- gets passHome
      let plain = messageMine m && isNothing (messageFile m) && isNothing (messageCall m)
          callOver = maybe True ((`notElem` [Just (messageId m), Just (asMessage i)]) . callEntry) (clientCall client)
          -- Theirs, ended unanswered without being missed.
          declined r = isNothing (recordDuration r) && not (recordMissed r) && not (messageMine m)
      forM_ ([Sync.Sent i | plain, messageDelivery m /= Pending] ++ [Sync.Delivered i | plain, messageDelivery m == Delivered]
             ++ [Sync.FileEnded i False Nothing | Just a <- [messageFile m], attachmentState a == Cancelled]
             ++ [ Sync.CallEnded i ((\d -> round (d * 1000)) <$> recordDuration r) (declined r)
                | Just r <- [messageCall m], callOver ]) $ \c ->
        unless (Home.marked c h) (() <$ write clock g c)
      h' <- gets passHome
      forM_ (messageFile m) $ \a ->
        when (attachmentState a == Complete && not (Home.marked (Sync.FileEnded i True Nothing) h') && Set.notMember i (Home.homeHolding h')) $
          modify' $ \p -> p { passHome = (passHome p) { Home.homeHolding = Set.insert i (Home.homeHolding (passHome p)) }
                            , passHolds = (cid, messageId m, attachmentPath a) : passHolds p }
    -- What may still be answered: ours not delivered, files and calls.
    changing m = isJust (messageFile m) || isJust (messageCall m) || (messageMine m && messageDelivery m /= Delivered)
    contentOf contact m = case (messageFile m, messageCall m) of
      (Just a, _) -> Sync.File (sender contact m) (attachmentId a) (messageText m) (attachmentSize a) (not (messageMine m)) Nothing
      (_, Just r) -> Sync.CallStarted (sender contact m) (not (messageMine m)) (recordVideo r)
      _ | messageMine m -> Sync.Send (sender contact m) (rawOf m) (syncKind (messageKind m))
        | otherwise -> Sync.Received Nothing (sender contact m) (rawOf m) (syncKind (messageKind m))
    sender contact m = if messageMine m then profileNickname profile else fromMaybe "" (contactName contact)
    rawOf m = fromMaybe (Text.encodeUtf8 (messageText m)) (messageRaw m)

    -- Each group entry we have, and whether ours went.
    entry graph gid e = do
      h <- gets passHome
      let isNew = Set.notMember (entryId e) (oldEntries gid) && not paging
      found <- case (eventOf h (entryId e), entryContent e) of
        (Just i, _) -> pure (Just i)
        (Nothing, Just content)
          | isNew || Home.homeBackfill h -> Just <$> written isNew (entryId e) graph (entryTime e) content
          | ours e, not (went e), Just i <- matching h graph (entryTime e) content -> Just i <$ earlier (entryId e) i
        _ -> pure Nothing
      forM_ found $ \i -> do
        h' <- gets passHome
        when (ours e && went e && not (Home.marked (Sync.Sent i) h')) $ () <$ write clock graph (Sync.Sent i)
    oldEntries gid = maybe Set.empty (Set.fromList . map entryId . groupEntries) (Map.lookup gid (clientGroups old))
    ours e = case entryWhat e of
      Said Nothing _ _ _ _ -> True
      PrivateTo {}         -> True
      _                    -> False
    went e = case entryWhat e of
      Said _ _ _ d _    -> d /= Pending
      PrivateTo _ _ _ d -> d /= Pending
      _                 -> False
    entryContent e = case entryWhat e of
      Said (Just m) kind text _ _ -> Just (Sync.Received (keyOf (memberKey m)) (memberName m) (Text.encodeUtf8 text) (syncKind kind))
      Said Nothing kind text _ _ -> Just (Sync.Send (profileNickname profile) (Text.encodeUtf8 text) (syncKind kind))
      PrivateFrom m kind text | Just pk <- keyOf (memberKey m) -> Just (Sync.ReceivedPrivately pk (memberName m) (Text.encodeUtf8 text) (syncKind kind))
      PrivateTo m kind text _ | Just pk <- keyOf (memberKey m) -> Just (Sync.SendPrivately pk (profileNickname profile) (Text.encodeUtf8 text) (syncKind kind))
      Arrived m | Just pk <- keyOf (memberKey m) -> Just (Sync.PeerJoined pk (memberName m))
      Departed m | Just pk <- keyOf (memberKey m) -> Just (Sync.PeerLeft pk (memberName m))
      TopicSet who topic -> Just (Sync.TopicSet (keyOf . memberKey =<< who) (nameOf who) topic)
      Moderated by whom how ->
        Just (Sync.Moderated (keyOf . memberKey =<< by) (nameOf by) (keyOf . memberKey =<< whom) (nameOf whom) (role how))
      what -> Sync.Notice <$> noticeText what
    -- Nothing is us.
    nameOf = maybe (groupNick profile) memberName
    role = \case
      Tox.ModKick      -> Nothing
      Tox.ModModerator -> Just Sync.Moderator
      Tox.ModUser      -> Just Sync.User
      Tox.ModObserver  -> Just Sync.Observer

    -- Invites as they come and go here.
    invites = do
      h <- gets passHome
      let here = [ (gid, pk, v) | ((cid, gid), v) <- Map.toList (clientInvites client), Just pk <- [contactKey cid] ]
      forM_ [ v' | v'@(_, pk, v) <- here, isNothing (inviteEvent h pk v) ] $ \(_, pk, v) ->
        write clock accountG (Sync.ChatInvite pk (if inviteKind v == NgcGroup then Sync.GroupKind else Sync.ConferenceKind) (inviteName v) (inviteData v))
      h' <- gets passHome
      let status i = Sync.inviteStatus <$> Map.lookup i (Sync.accountInvites (state' h'))
          stillHere i = any (\(_, pk, v) -> inviteEvent h' pk v == Just i) here
          known = Map.union (Home.homeInvited h') (Map.fromList [ (i, gid) | (gid, pk, v) <- here, Just i <- [inviteEvent h' pk v] ])
          outcome i gid = case Map.lookup gid (clientGroups client) of
            Just g | groupPhase g `elem` [Joining, InGroup], Just c <- chatIdOf gid g ->
              [Sync.InviteJoined i (Just c) False | status i /= Just (Sync.JoinedChat c)]
            Just g | Refused why <- groupPhase g, status i /= Just (Sync.Failed (why == Tox.JoinFailBadPassword)) ->
              [Sync.InviteJoined i Nothing (why == Tox.JoinFailBadPassword)]
            _ | not (stillHere i), status i == Just Sync.Pending -> [Sync.AnswerInvite i False Nothing]
              | otherwise -> []
          answered = [ Sync.AnswerInvite i True (Just password)
                     | (_, pk, v) <- here, Just password <- [inviteAnswered v], Just i <- [inviteEvent h' pk v], status i == Just Sync.Pending ]
          -- Kept until we're in the group or it turned us away.
          open i gid = case groupPhase <$> Map.lookup gid (clientGroups client) of
            Just InGroup     -> False
            Just (Refused _) -> False
            Nothing          -> stillHere i
            _                -> True
      mapM_ (write clock accountG) (answered ++ concat [outcome i gid | (i, gid) <- Map.toList known])
      modify' $ \p -> p { passHome = (passHome p) { Home.homeInvited = Map.filterWithKey open known } }
    inviteEvent h pk v = listToMaybe
      [ i | (i, w) <- Map.toList (Sync.accountInvites (state' h)), Sync.inviteFrom w == pk, Sync.inviteData w == inviteData v ]

    -- Read here up to the newest of theirs.
    readWrites h =
      [ (accountG, Sync.ReadUpTo g latest rank)
      | (g, unread) <- [ (Sync.friendGraph account pk, contactUnread contact)
                       | (cid, pk, contact) <- friends, full || Map.lookup cid (clientContacts old) /= Just contact ]
                    ++ [ (Sync.chatGraph c, groupUnread g) | (gid, c, g) <- groups, changedGroup gid g ]
      , unread == 0
      , Just (rank, latest) <- [Map.lookup g (Home.homeLatest h)]
      , maybe True ((< rank) . fst) (Map.lookup g (Sync.accountRead (state' h))) ]

    -- An event of a message from before we were the home: same content
    -- (whatever the names), at the same time give or take a second.
    matching h g time content =
      let claimed = Set.fromList (Map.elems (Home.homeEarlier h))
      in listToMaybe
           [ i | (i, e) <- Sync.byRank g (Home.homeStore h), Set.notMember i claimed
           , unnamed (Sync.eventContent e) == unnamed content
           , abs (Sync.eventTime e - toMillis time) <= 1000 ]
    unnamed = \case
      Sync.File _ fid name size incoming blob -> Sync.File "" fid name size incoming blob
      Sync.CallStarted _ incoming video -> Sync.CallStarted "" incoming video
      Sync.Send _ bytes kind -> Sync.Send "" bytes kind
      Sync.Received from _ bytes kind -> Sync.Received from "" bytes kind
      Sync.ReceivedPrivately from _ bytes kind -> Sync.ReceivedPrivately from "" bytes kind
      Sync.SendPrivately to _ bytes kind -> Sync.SendPrivately to "" bytes kind
      c -> c
    syncKind = \case
      Tox.MessageAction -> Sync.Action
      _ -> Sync.Normal
    asMessage (Sync.EventId bytes) = MessageId bytes
