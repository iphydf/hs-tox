{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | How events change a client, and the commands for Tox.
module Tox.Client.Update
  ( Event (..)
  , Refusal (..)
  , Chat (..)
  , Viewing (..)
  , Command (..)
  , update
  , drafted
  , memberByKey
  , replied
  , forwardable
  , interrupted
  ) where

import           Control.Applicative       ((<|>))
import           Control.Monad             (mfilter)
import qualified Crypto.Hash.SHA256        as SHA256
import           Data.Bits                 (shiftR)
import qualified Data.ByteString           as BS
import           Data.Containers.ListUtils (nubOrdOn)
import           Data.List                 (find, insertBy, nub)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe, isJust, isNothing,
                                            listToMaybe, mapMaybe)
import           Data.Ord                  (comparing)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Time                 (UTCTime, diffUTCTime)
import           Data.Time.Clock.POSIX     (utcTimeToPOSIXSeconds)
import           Data.Word                 (Word32, Word64)
import qualified Tox.Application.Tox       as Tox
import qualified Tox.Share.History         as History
import qualified Tox.Share.Info            as Info
import           Tox.Share.Info            (Info)
import           Tox.Share.Packet          (sha1Info)
import qualified Tox.Sync.Account          as Sync
import qualified Tox.Sync.Event            as Sync
import qualified Tox.Sync.Fact             as Sync
import qualified Tox.Sync.Id               as Sync
import qualified Tox.Sync.Protocol         as Protocol
import qualified Tox.Sync.Replica          as Replica
import qualified Tox.Sync.Setting          as Sync
import qualified Tox.Sync.Store            as Sync

import           Tox.Client
import qualified Tox.Client.Debug          as Debug
import qualified Tox.Client.Home           as Home
import           Tox.Client.Home           (Home)
import qualified Tox.Client.Linked         as Linked
import           Tox.Client.Linked         (Linked)
import           Tox.Client.Locations      (locationText)
import           Tox.Client.Messages       (parts)
import qualified Tox.Client.Recording      as Recording
import           Tox.Client.Synced
import           Tox.Client.Types

-- | What Tox reports, and what the user wants done.
data Event
  = Started Text
    -- ^ Tox is running, with this Tox ID.
  | Connected Connection
  | FriendOnline ContactId Bool
  | FriendNamed ContactId Text
  | FriendStatusMessage ContactId Text
  | FriendStatus ContactId Tox.UserStatus
  | FriendTyping ContactId Bool
  | FriendAvatar ContactId (Maybe BS.ByteString)
    -- ^ PNG, or Nothing if they removed theirs.
  | Requested ContactId Text
  | AddFailed Text (Maybe Tox.AddFriendError)
    -- ^ Tox refused to add a Tox ID, Nothing if it isn't one.
  | Received ContactId UTCTime Tox.MessageType BS.ByteString
  | Delivering ContactId MessageId Delivery
  | Confirmed ContactId MessageId Int UTCTime
    -- ^ How many parts of our message the friend has.
  | FileStarted ContactId MessageId FileRef
    -- ^ Tox offered the friend our file.
  | FileOffered ContactId FileRef UTCTime Text Word64 FilePath BS.ByteString
    -- ^ Name, size, path to save it to, and file ID.
  | FileMoved ContactId FileRef Word64
    -- ^ Bytes so far.
  | FileEnded ContactId FileRef Bool
    -- ^ Whether the whole file got through.
  | CallInvited ContactId UTCTime Bool
    -- ^ With video or not.
  | CallPeer ContactId UTCTime Bool
    -- ^ The friend answered our call, or turned video on or off.
  | CallEnded ContactId UTCTime
  | CallGlared ContactId UTCTime
    -- ^ Tox failed the call because both ends called at once.
  | CallElsewhere UTCTime
    -- ^ Another profile has a call (we only do 1 at a time).
  | Tick UTCTime
    -- ^ Each second during calls and on the debug screen.
  | Invited ContactId GroupId GroupKind Text BS.ByteString
    -- ^ Group name and invite data.
  | GroupCreated GroupId GroupKind Text UTCTime
    -- ^ We founded it.
  | GroupConnected GroupId UTCTime
    -- ^ We have the group state from peers.
  | GroupRefused GroupId Tox.JoinFail
  | MembersListed GroupId UTCTime [(Tox.PeerId, Member)]
    -- ^ Everyone in a conference now, except us.
  | MemberJoined GroupId UTCTime Tox.PeerId Member
  | MemberLeft GroupId UTCTime Tox.PeerId Tox.ExitType
  | MemberNamed GroupId Tox.PeerId Text
  | MemberStatus GroupId Tox.PeerId Tox.UserStatus
  | RolesKnown GroupId (Maybe Tox.Role) [(Tox.PeerId, Tox.Role)]
    -- ^ Ours and the members'.
  | SettingsKnown GroupId Tox.GroupSettings
  | GroupReceived GroupId UTCTime Tox.PeerId Tox.MessageType Text (Maybe Word32)
    -- ^ With its Tox message ID (conferences have none).
  | PrivateReceived GroupId UTCTime Tox.PeerId Tox.MessageType Text
  | GroupSent GroupId MessageId (Maybe Word32)
    -- ^ With the Tox message ID of its first part (conferences have none).
  | GroupFileShared GroupId UTCTime Tox.PeerId Word32 BS.ByteString
    -- ^ A member shared a file: the Tox message ID and the file's ID.
  | GroupFileLearned GroupId BS.ByteString Info
    -- ^ Its info: name, size and chunk hashes.
  | DownloadGroupFile GroupId BS.ByteString
  | PauseGroupFile GroupId BS.ByteString
  | GroupFileMoved GroupId BS.ByteString FilePath Word64 Double
    -- ^ Where it goes, how much of it came, and how fast it comes.
  | GroupFileEnded GroupId BS.ByteString Bool
    -- ^ Whether all of it came.
  | GroupHistoryWanted GroupId Tox.PeerId History.Range
  | GroupRecalled GroupId UTCTime Tox.PeerId (Map Text Text) [Recalled]
    -- ^ The messages a member sent for the history we asked it for, with
    -- the names we know for their senders' keys.
  | WroteGroupFile GroupId UTCTime FilePath Text Word64
    -- ^ Our file to share in the group: path, name and size.
  | GroupFileOffered GroupId MessageId (Maybe (Info, Word32))
    -- ^ Our file's info and the Tox message ID we announced it with, or
    -- 'Nothing' if it couldn't be read.
  | GroupSaved GroupId BS.ByteString
  | GroupNamed GroupId Text
  | TopicChanged GroupId UTCTime (Maybe Tox.PeerId) Text
    -- ^ Peer, if Tox knows who.
  | TopicSetByUs GroupId UTCTime Text
  | TopicAllowed GroupId Bool
  | GroupModerated GroupId UTCTime (Maybe Tox.PeerId) (Maybe Tox.PeerId) Tox.Moderation
    -- ^ Who did it to whom. Nothing means us.
  | SyncReceived ContactId UTCTime BS.ByteString
    -- ^ Only a linked account's home sends these.
  | FileShared ContactId UTCTime Text Word64 BS.ByteString FilePath
    -- ^ Our file for a linked account's friend, stored as a blob for the
    -- home: name, size, blob ID and path.
  | BlobArrived BS.ByteString FilePath (Maybe BS.ByteString)
    -- ^ From a linked account's home, saved at the path. Bytes too if it's
    -- small enough to be an avatar.
  | BlobMissing BS.ByteString
    -- ^ Came out of order, cut short, or doesn't match its ID.
  | EarlierMessages ContactId [Message]
    -- ^ From the store, those before the oldest we have. None if there are
    -- no more.
  | EarlierEntries GroupId [GroupEntry]
  | CloseChat Chat
    -- ^ The user left it, so it keeps only its newest.
  | ToxRefused Refusal
  | InviteSent GroupId ContactId
  | Measured Debug.Report
    -- ^ As asked with 'MeasureDebug'.
  | ChangeProfile Text Text
    -- ^ Nickname and status message.
  | SetAway Tox.UserStatus
  | SetAvatar BS.ByteString
    -- ^ Our new avatar as a PNG.
  | RemoveAvatar
  | SetAutoAccept Word64
  | SetTypingIndicators Bool
  | Typing ContactId UTCTime
    -- ^ The user typed in the friend's chat.
  | SetDevice DeviceSettings
  | AddFriend Text Text
    -- ^ Tox ID and message.
  | AcceptRequest ContactId
  | RejectRequest ContactId
  | DeleteContact ContactId Bool
    -- ^ Keep the chat or not.
  | MuteChat ContactId (Maybe UTCTime)
    -- ^ Until a time, or forever.
  | UnmuteChat ContactId
  | SendPictures ContactId (Maybe Text)
  | SendGroupPictures GroupId (Maybe Text)
    -- ^ Picture format to send them, by its key.
  | MarkRead Chat
  | SetDraft Chat Text
  | Stage ContactId Staged
  | Unstage ContactId Int
    -- ^ By its index in the staged list.
  | ReplyTo Chat MessageId
  | CancelReply Chat
  | Send Chat UTCTime
    -- ^ Its draft, with what's staged.
  | SendPrivately GroupId Text UTCTime
    -- ^ The group's draft, to the member with this key.
  | Forward Chat [MessageId] ContactId UTCTime
    -- ^ Messages of a chat to a friend.
  | Download ContactId MessageId
  | Cancel ContactId MessageId
  | SendAgain ContactId MessageId
    -- ^ Our file that was cancelled, from the start.
  | StartCall ContactId UTCTime Bool
    -- ^ With or without the camera.
  | AnswerCall UTCTime Bool
  | HangUp UTCTime
  | ToggleMute
  | ToggleCamera
  | AnswerInvite ContactId GroupId (Maybe Text)
    -- ^ Join with this password, or decline.
  | Found Founding Text
    -- ^ A new group with this name.
  | JoinPublic GroupId Text
    -- ^ With its password.
  | InviteFriend GroupId ContactId
  | LeaveGroup GroupId
  | DeleteGroup GroupId
  | MuteGroup GroupId (Maybe UTCTime)
  | UnmuteGroup GroupId
  | ToggleMentions GroupId
  | SetTopic GroupId Text
  | Moderate GroupId Tox.PeerId MemberAction
  | ChangeSettings GroupId Tox.GroupSettings
  | ChooseRelayBot GroupId (Maybe Text)
    -- ^ Member key of the relay bot, or none.
  | Pair Text UTCTime
    -- ^ A new device may pair with this code (it makes us the home).
  | Unpair Text
    -- ^ The code stops working.
  | RemoveDevice ContactId
  | FileHeld ContactId MessageId BS.ByteString
    -- ^ The file, stored as a blob for the account's devices.
  | Wrote ContactId MessageId UTCTime Tox.MessageType BS.ByteString
    -- ^ A linked device's message to the friend.
  | WroteFile ContactId MessageId UTCTime FilePath Text Word64
    -- ^ A linked device's file for the friend: where its blob is, name and
    -- size.
  | WroteGroup GroupId MessageId UTCTime Tox.MessageType BS.ByteString
    -- ^ A linked device's message to the group.
  | WrotePrivately GroupId MessageId UTCTime Text Tox.MessageType BS.ByteString
    -- ^ A linked device's message to the member with this key alone.
  deriving (Eq, Show)

data Chat = FriendChat ContactId | GroupChat GroupId
  deriving (Eq, Show)

-- | What Tox didn't do.
data Refusal
  = TopicRefused GroupId
  | FoundingRefused Founding
  | InviteRefused GroupId ContactId
  | ModerationRefused GroupId Tox.PeerId Tox.ModError
  | SettingRefused GroupId
  deriving (Eq, Show)

-- | What the app shows, for unread counts and read marks, and how much of
-- each chat it keeps in memory.
data Viewing = Viewing
  { viewingChat  :: Maybe Chat
    -- ^ Open in the app. It keeps all it has (the user may be reading back).
  , viewingShown :: Bool
    -- ^ The app is in front.
  , viewingEnd   :: Bool
    -- ^ Scrolled to its newest messages.
  , viewingPage  :: Int
    -- ^ Other chats keep this many of their newest, and those still in use.
  } deriving (Eq, Show)

update :: Viewing -> Event -> Client -> (Client, [Command])
update viewing event client = case (clientLinked client, clientHome client) of
  (Just l, _) -> linkedUpdate l viewing event client
  (Nothing, Just h) -> homeUpdate h viewing event client
  (Nothing, Nothing) -> case event of
    -- The first device makes us the home.
    Pair {} | Just account <- keyOf (Text.take 64 (profileToxId (clientProfile client))) ->
      homeUpdate (Home.newHome account Sync.emptyStore) viewing event client
    _ -> ownUpdate viewing event client

-- | Whether the chat has a draft or staged items to send.
drafted :: Chat -> Client -> Bool
drafted chat client = case chat of
  FriendChat cid -> written (Map.lookup cid (clientDrafts client)) || not (null (Map.findWithDefault [] cid (clientStaged client)))
  GroupChat gid  -> written (Map.lookup gid (clientGroupDrafts client))
  where written = maybe False (not . Text.null . Text.strip)


forwardable :: Message -> Bool
forwardable m = isNothing (messageCall m)
  && maybe True (\a -> messageMine m || attachmentState a == Complete) (messageFile m)

-- | What the chat's draft replies to, quoted to go in front of it.
replied :: Client -> Chat -> Maybe Text
replied client chat = quoted <$> case chat of
  FriendChat cid -> said cid =<< Map.lookup cid (clientReplies client)
  GroupChat gid  -> groupSaid gid =<< Map.lookup gid (clientGroupReplies client)
  where
    quoted = Text.intercalate "\n" . map ("> " <>) . Text.lines
    said cid i = shownText . messageText <$> (mfilter (isNothing . messageCall) . find ((== i) . messageId) . contactMessages
      =<< Map.lookup cid (clientContacts client))
    -- Never what was said privately: the answer may go to everyone.
    groupSaid gid i = case entryWhat <$> (find ((== i) . entryId) . groupEntries =<< Map.lookup gid (clientGroups client)) of
      Just (Said _ _ body _ _) -> Just (shownText body)
      _                        -> Nothing

-- | The draft, after what it replies to.
replying :: Client -> Chat -> Text -> Text
replying client chat draft = maybe draft (<> "\n" <> draft) (replied client chat)

-- | For Tox to do.
-- | A group message for history sync: when, the sender's key (Nothing for
-- ours), its Tox message ID and what it said.
type Recalled = (UTCTime, Maybe Text, Word32, History.Body)

data Command
  = SetProfile Profile
  | SetGroupStatus GroupId Tox.UserStatus
  | Befriend ContactId
    -- ^ Without a request: to accept theirs, or to add a friend back.
  | Unfriend ContactId
  | SendAvatar ContactId (Maybe BS.ByteString)
    -- ^ Our avatar as a PNG, or Nothing if we have none.
  | RequestFriend Text Text
    -- ^ Tox ID and message.
  | SendMessage ContactId MessageId Text Int
    -- ^ With how many of its parts the friend confirmed.
  | SendFile ContactId MessageId FilePath Text Word64 (Maybe BS.ByteString)
  | AcceptFile ContactId FileRef FilePath
    -- ^ Resumes after what's already in the file.
  | CancelFile ContactId FileRef
  | ForgetFile FilePath
    -- ^ Delete our copy of a file that was unstaged.
  | CallFriend ContactId Bool
    -- ^ With or without the camera, as for the other call commands.
  | AnswerFriend ContactId Bool
  | HangUpCall ContactId
  | CameraCall ContactId Bool
  | JoinGroup ContactId GroupId BS.ByteString Text Text
    -- ^ With the friend's invite, our name in the group and a password.
  | JoinConference ContactId GroupId BS.ByteString
  | RestoreGroup GroupKind GroupId BS.ByteString
  | SendGroupMessage GroupId MessageId Text
  | QuitGroup GroupId
  | SetGroupTopic GroupId Text
  | FoundGroup Founding Text Text
    -- ^ Group name and our name in it.
  | JoinPublicGroup GroupId Text Text
    -- ^ Our name in it and its password.
  | InviteToGroup GroupId ContactId
  | SendPrivate GroupId MessageId Tox.PeerId Text
  | ModerateMember GroupId Tox.PeerId MemberAction
  | SetGroupSettings GroupId Tox.GroupSettings
  | SendSync ContactId [BS.ByteString]
    -- ^ For a linked account's home.
  | FetchBlob ContactId BS.ByteString [BS.ByteString] (Maybe Text)
    -- ^ Get a blob from the home with these packets, unless we have it
    -- already (a file under its name, an avatar with the blobs).
  | ServeBlob ContactId BS.ByteString [BS.ByteString]
    -- ^ Send the home a blob it asked for, or these packets if we don't
    -- have it.
  | StoreBlob BS.ByteString
    -- ^ So the home can ask for it.
  | ShareFile ContactId FilePath Text Word64
    -- ^ Store a file as a blob, for the home to send to a linked account's
    -- friend.
  | FetchGroupFile GroupId Info
  | PauseGroupDownload GroupId BS.ByteString
  | LearnGroupFile GroupId Tox.PeerId BS.ByteString
    -- ^ Get the info of a file the member has.
  | ShareGroupFile GroupId MessageId FilePath Text
  | SeedGroupFiles GroupId [(Info, FilePath)]
    -- ^ The group's files we have whole, for members to fetch from us.
  | AskHistory GroupId Tox.PeerId
  | AnswerHistory GroupId Tox.PeerId History.Range [Recalled]
  | SetTyping ContactId Bool
  | HoldFile ContactId MessageId FilePath
    -- ^ Store a file as a blob for the account's devices.
  | RelayCall ContactId ContactId Bool
    -- ^ Call the account's device, with video or not, and pass the call
    -- with the friend to it and back.
  | MeasureDebug
  deriving (Eq, Show)


-- | For profiles whose Tox runs on this device.
ownUpdate :: Viewing -> Event -> Client -> (Client, [Command])
ownUpdate viewing event client = case event of
  -- Only a linked account's home sends these.
  SyncReceived {} -> only client
  BlobArrived {} -> only client
  BlobMissing {} -> only client
  FileShared {} -> only client
  -- For the home of linked devices.
  Pair {} -> only client
  Unpair _ -> only client
  RemoveDevice _ -> only client
  FileHeld {} -> only client
  Wrote cid i time kind bytes | Just contact <- Map.lookup cid (clientContacts client) ->
    let m = (ours (Text.decodeUtf8Lenient bytes) time Nothing i) { messageKind = kind }
    in ( withContact cid (grown cid contact { contactMessages = contactMessages contact ++ [m] })
       , [SendMessage cid i (messageText m) 0 | contactOnline contact] )
  Wrote {} -> only client
  WroteFile cid i time path name size | Just contact <- Map.lookup cid (clientContacts client) ->
    ( withContact cid (grown cid contact { contactMessages = contactMessages contact ++ [ours name time (Just (Attachment size path 0 Waiting Nothing)) i] })
    , [SendFile cid i path name size Nothing | contactOnline contact] )
  WroteFile {} -> only client
  WroteGroup gid i time kind bytes | Just g <- Map.lookup gid (clientGroups client), groupPhase g `elem` [Joining, InGroup] ->
    let text = Text.decodeUtf8Lenient bytes
    in ( withGroup gid (grownGroup gid g { groupEntries = groupEntries g ++ [GroupEntry time (Said Nothing kind text Pending Nothing) i] })
       , [SendGroupMessage gid i text | groupPhase g == InGroup] )
  WroteGroup {} -> only client
  -- Never to everyone if the member is gone.
  WrotePrivately gid i time key kind bytes | Just g <- Map.lookup gid (clientGroups client), Just (n, m) <- memberByKey key g ->
    let text = Text.decodeUtf8Lenient bytes
    in ( withGroup gid (grownGroup gid g { groupEntries = groupEntries g ++ [GroupEntry time (PrivateTo m kind text Pending) i] })
       , [SendPrivate gid i n text] )
  WrotePrivately {} -> only client
  -- For the user to see.
  ToxRefused _ -> only client
  InviteSent {} -> only client
  Measured _ -> only client
  SetAvatar png ->
    let hash = SHA256.hash png
    in ( pruned client { clientAvatar = Just hash, clientAvatars = Map.insert hash png (clientAvatars client) }
       , [SendAvatar cid (Just png) | (cid, c) <- Map.toList (clientContacts client), contactOnline c] )
  RemoveAvatar ->
    ( pruned client { clientAvatar = Nothing }
    , [SendAvatar cid Nothing | (cid, c) <- Map.toList (clientContacts client), contactOnline c] )
  FriendAvatar cid png -> only $ pruned $
    (adjusted cid (\c -> c { contactAvatar = SHA256.hash <$> png }))
      { clientAvatars = maybe id (\p -> Map.insert (SHA256.hash p) p) png (clientAvatars client) }
  SetAutoAccept size -> only client { clientAccount = (clientAccount client) { settingAutoAccept = size } }
  SetTypingIndicators on ->
    let set = client { clientAccount = (clientAccount client) { settingTyping = on } }
    in if on then only set else foldr stopTyping (only set { clientTyping = Set.empty }) (Map.keys (clientTypingTo client))
  Typing cid time
    | settingTyping (clientAccount client)
    , maybe False contactOnline (Map.lookup cid (clientContacts client)) ->
        ( client { clientTypingTo = Map.insert cid time (clientTypingTo client) }
        , [SetTyping cid True | Map.notMember cid (clientTypingTo client)] )
    | otherwise -> only client
  FriendTyping cid typing
    | typing && settingTyping (clientAccount client) -> only client { clientTyping = Set.insert cid (clientTyping client) }
    | otherwise -> only client { clientTyping = Set.delete cid (clientTyping client) }
  SetDevice device -> only client { clientDevice = device }
  SetDraft (FriendChat cid) draft ->
    let drafted' = client { clientDrafts = Map.insert cid draft (clientDrafts client) }
    in if Text.null (Text.strip draft) then stopTyping cid (only drafted') else only drafted'
  SetDraft (GroupChat gid) draft -> only client { clientGroupDrafts = Map.insert gid draft (clientGroupDrafts client) }
  MarkRead (FriendChat cid) -> only $ adjusted cid (\c -> c { contactUnread = 0 })
  MarkRead (GroupChat gid) -> only $ adjustedGroup gid (\g -> g { groupUnread = 0 })
  EarlierMessages cid page -> only $ adjusted cid $ \c ->
    c { contactMessages = inFront messageId page (contactMessages c), contactEarlier = not (null page) }
  EarlierEntries gid page -> only $ adjustedGroup gid $ \g ->
    g { groupEntries = inFront entryId page (groupEntries g), groupEarlier = not (null page) }
  CloseChat (FriendChat cid) -> stopTyping cid (only (adjusted cid (trimmed cid)))
  CloseChat (GroupChat gid) -> only $ adjustedGroup gid (trimmedGroup gid)
  Send (GroupChat gid) time -> groupSend gid Nothing time
  SendPrivately gid key time -> groupSend gid (Just key) time
  -- Staged items first, then the text.
  Send (FriendChat cid) time
    | Map.member cid (clientTypingTo client) ->
        let (sent, commands) = ownUpdate viewing event client { clientTypingTo = Map.delete cid (clientTypingTo client) }
        in (sent, SetTyping cid False : commands)
    | Just contact <- Map.lookup cid (clientContacts client)
    , let staged = Map.findWithDefault [] cid (clientStaged client)
          draft = maybe "" Text.strip (Map.lookup cid (clientDrafts client))
    , not (null staged) || not (Text.null draft) ->
        let message = replying client (FriendChat cid) draft
            picked = \case
              StagedFile path name size fileId -> ours name time (Just (Attachment size path 0 Waiting (Just fileId)))
              StagedPlace latitude longitude   -> ours (locationText latitude longitude) time Nothing
            sent = zipWith ($) (map picked staged ++ [ours message time Nothing | not (Text.null draft)]) newIds
        in
        ( using (length sent) $ (withContact cid (grown cid contact { contactMessages = contactMessages contact ++ sent }))
            { clientDrafts = Map.delete cid (clientDrafts client)
            , clientStaged = Map.delete cid (clientStaged client)
            , clientReplies = if Text.null draft then clientReplies client else Map.delete cid (clientReplies client) }
        , [ case messageFile m of
              Just a  -> SendFile cid (messageId m) (attachmentPath a) (messageText m) (attachmentSize a) (attachmentId a)
              Nothing -> SendMessage cid (messageId m) (messageText m) 0
          | contactOnline contact, m <- sent ]
        )
    | otherwise -> only client
  ChangeProfile nickname status ->
    let applied = (clientProfile client) { profileNickname = Text.strip nickname, profileStatus = Text.strip status }
    in (client { clientProfile = applied }, [SetProfile applied])
  SetAway away ->
    let chosen = (clientProfile client) { profileAway = away } in (client { clientProfile = chosen }, [SetProfile chosen])
  AddFriend address message ->
    let cid = ContactId (Text.toUpper (Text.take 64 address))
        request = (Text.toUpper address, Text.strip message)
        requesting c = c { contactRequest = Just request }
    in ( client
           { clientContacts = Map.alter (Just . requesting . fromMaybe (known cid)) cid (clientContacts client)
           , clientFormer = Map.delete cid (clientFormer client)
           , clientRequests = Map.delete cid (clientRequests client)
           , clientDeclined = Map.delete cid (clientDeclined client)
           }
       , [uncurry RequestFriend request]
       )
  AddFailed address _ ->
    let cid = ContactId (Text.toUpper (Text.take 64 address))
        forget c
          | null (contactMessages c) = Nothing
          | otherwise = Just c { contactRequest = Nothing }
    in only client { clientContacts = Map.update forget cid (clientContacts client) }
  AcceptRequest cid
    | Map.member cid (clientRequests client) ->
        ( (withContact cid (known cid))
            { clientRequests = Map.delete cid (clientRequests client)
            , clientFormer = Map.delete cid (clientFormer client)
            , clientDeclined = Map.delete cid (clientDeclined client) }
        , [Befriend cid]
        )
    | otherwise -> only client
  RejectRequest cid -> only client
    { clientRequests = Map.delete cid (clientRequests client)
    , clientDeclined = maybe id (Map.insert cid) (Map.lookup cid (clientRequests client)) (clientDeclined client)
    }
  Started address ->
    ( client { clientProfile = (clientProfile client) { profileToxId = address }
             -- Tox only knows these again once it hears from peers.
             , clientGroups = (\g -> g { groupPhase = if groupPhase g == InGroup then Joining else groupPhase g
                                       , groupMembers = Map.empty, groupSince = Nothing, groupRoles = Map.empty
                                       , groupSelfRole = Nothing, groupSettings = Nothing }) <$> clientGroups client }
    , SetProfile (clientProfile client)
        : [ maybe (Befriend cid) (uncurry RequestFriend) (contactRequest c)
          | (cid, c) <- Map.toList (clientContacts client) ]
        -- After the friends (they may help find the peers).
        ++ [ RestoreGroup (groupKind g) gid saved
           | (gid, g) <- Map.toList (clientGroups client), groupPhase g `elem` [Joining, InGroup]
           , Just saved <- [groupSaved g] ]
    )
  Connected connection -> only client { clientConnection = connection }
  FriendOnline cid online -> case Map.lookup cid (clientContacts client) of
    Just contact ->
      -- Tox ends a friend's transfers when they go offline. We offer ours
      -- again when they're back.
      let (ended, going) = Map.partitionWithKey (\(c, _) _ -> c == cid && not online) (clientTransfers client)
          cut = foldr (\i -> adjustMessage cid i interrupted) client { clientTransfers = going } (Map.elems ended)
          -- Tox also says online when they move between TCP and UDP.
          returned = online && not (contactOnline contact)
          met c = c { contactOnline = online, contactRequest = if online then Nothing else contactRequest c
                    , contactStatus = if online then contactStatus c else Tox.Online }
          -- Drop invites we answered, now the friend is back to get the answer.
          spent (c, _) invite = c /= cid || not returned || isNothing (inviteAnswered invite)
      in ( cut { clientContacts = Map.adjust met cid (clientContacts cut)
               , clientInvites = Map.filterWithKey spent (clientInvites cut)
               -- Typing ends with them gone, both ways.
               , clientTyping = if online then clientTyping cut else Set.delete cid (clientTyping cut)
               , clientTypingTo = if online then clientTypingTo cut else Map.delete cid (clientTypingTo cut) }
         , [ SendMessage cid (messageId m) (messageText m) (messageParts m)
           | returned
           , m <- contactMessages contact
           , messageMine m, messageDelivery m /= Delivered, Nothing <- [messageFile m] ]
           ++
           [ SendAvatar cid (Just png) | returned, Just png <- [(`Map.lookup` clientAvatars client) =<< clientAvatar client] ]
           ++
           [ SendFile cid (messageId m) (attachmentPath a) (messageText m) (attachmentSize a) (attachmentId a)
           | returned
           , m <- contactMessages contact
           , messageMine m, Just a <- [messageFile m], attachmentState a == Waiting ]
           -- Invites we accepted while the friend was away (Tox can only
           -- answer them through the friend).
           ++
           [ joinCommand cid gid invite password
           | returned
           , ((c, gid), invite) <- Map.toList (clientInvites client), c == cid
           , Just password <- [inviteAnswered invite] ]
         )
    Nothing -> only client
  FriendNamed cid name -> only $ adjusted cid (\c -> c { contactName = Just name })
  FriendStatusMessage cid status -> only $ adjusted cid (\c -> c { contactStatusMessage = status })
  FriendStatus cid status -> only $ adjusted cid (\c -> c { contactStatus = status })
  DeleteContact cid keep -> case Map.lookup cid (clientContacts client) of
    Just contact ->
      let calling = [c | Just c <- [clientCall client], callContact c == cid]
      in ( client
             { clientContacts = Map.delete cid (clientContacts client)
             , clientFormer = if keep
                 then Map.insert cid contact { contactOnline = False, contactUnread = 0, contactRequest = Nothing }
                        (clientFormer client)
                 else clientFormer client
             , clientDrafts = Map.delete cid (clientDrafts client)
             , clientStaged = Map.delete cid (clientStaged client)
             , clientInvites = Map.filterWithKey (\(c, _) _ -> c /= cid) (clientInvites client)
             , clientTransfers = Map.filterWithKey (\(c, _) _ -> c /= cid) (clientTransfers client)
             , clientCall = if null calling then clientCall client else Nothing
             }
         , [HangUpCall cid | not (null calling)] ++ [Unfriend cid]
           ++ [ForgetFile path | StagedFile path _ _ _ <- Map.findWithDefault [] cid (clientStaged client)] )
    Nothing -> only client
  MuteChat cid until' -> only $ adjusted cid (\c -> c { contactMute = Just (maybe MutedAlways MutedUntil until') })
  UnmuteChat cid -> only $ adjusted cid (\c -> c { contactMute = Nothing })
  Received cid time kind bytes -> only $ using 1 $ adjusted cid $ \c -> grown cid c
    { contactMessages = contactMessages c
        ++ [Message False (Text.decodeUtf8Lenient bytes) time Delivered Nothing Nothing 0 Nothing kind (Just bytes) newId]
    , contactUnread = contactUnread c + (if reading (FriendChat cid) then 0 else 1)
    }
  Delivering cid i delivery -> only $ adjustMessage cid i (\m ->
    if messageDelivery m /= Delivered then m { messageDelivery = delivery } else m) client
  Confirmed cid i n time -> only $ adjustMessage cid i (\m ->
    let confirmed = max n (messageParts m)
        whole = confirmed >= length (parts (messageText m))
    in m { messageParts = confirmed
         , messageDelivery = if whole then Delivered else messageDelivery m
         , messageDelivered = if whole && isNothing (messageDelivered m) then Just time else messageDelivered m
         }) client
  ReplyTo chat i ->
    let replying' = case chat of
          FriendChat cid -> client { clientReplies = Map.insert cid i (clientReplies client) }
          GroupChat gid  -> client { clientGroupReplies = Map.insert gid i (clientGroupReplies client) }
    -- Only to what has words: not to calls or notices.
    in only (if isJust (replied replying' chat) then replying' else client)
  CancelReply (FriendChat cid) -> only client { clientReplies = Map.delete cid (clientReplies client) }
  CancelReply (GroupChat gid) -> only client { clientGroupReplies = Map.delete gid (clientGroupReplies client) }
  Forward source is target time -> case Map.lookup target (clientContacts client) of
    Just contact ->
      -- In the order of their chat. A group's files stay behind.
      let sent = case source of
            FriendChat cid -> [ (shownText (messageText m), messageFile m)
                              | m <- maybe [] contactMessages (Map.lookup cid (clientContacts client)), messageId m `elem` is, forwardable m ]
            GroupChat gid -> [ (text, Nothing)
                             | e <- maybe [] groupEntries (Map.lookup gid (clientGroups client)), entryId e `elem` is, Just text <- [entryText e] ]
          copies = zipWith copy sent newIds
          copy (text, file) = ours text time (forwardedFile <$> file)
          forwardedFile a = a { attachmentState = Waiting, attachmentDone = 0, attachmentId = Nothing }
      in ( using (length copies) (withContact target (grown target contact { contactMessages = contactMessages contact ++ copies }))
         , [ case messageFile m of
               Nothing -> SendMessage target (messageId m) (messageText m) 0
               Just a  -> SendFile target (messageId m) (attachmentPath a) (messageText m) (attachmentSize a) (attachmentId a)
           | contactOnline contact, m <- copies ] )
    Nothing -> only client
  Requested cid message
    | Map.member cid (clientContacts client) -> (client, [Befriend cid])
    | Map.lookup cid (clientDeclined client) == Just message -> only client
    | otherwise -> only client { clientRequests = Map.insert cid message (clientRequests client) }
  Stage cid item -> only client { clientStaged = Map.insertWith (flip (++)) cid [item] (clientStaged client) }
  Unstage cid k ->
    let staged = Map.findWithDefault [] cid (clientStaged client)
    in ( client { clientStaged = Map.filter (not . null) (Map.insert cid (take k staged ++ drop (k + 1) staged) (clientStaged client)) }
       , [ForgetFile path | StagedFile path _ _ _ <- take 1 (drop k staged)] )
  FileStarted cid i ref -> only client { clientTransfers = Map.insert (cid, ref) i (clientTransfers client) }
  FileOffered cid ref time name size path fileId -> case Map.lookup cid (clientContacts client) of
    -- Resume a file of theirs that was cut off, in the same message.
    Just contact
      | (i, a) : _ <- [ (messageId m, a) | m <- contactMessages contact, not (messageMine m)
                               , Just a <- [messageFile m], attachmentId a == Just fileId
                               , attachmentSize a == size, attachmentState a `elem` [Waiting, Interrupted] ] ->
          let taken = attachmentState a == Interrupted || size <= settingAutoAccept (clientAccount client)
          in ( (adjustFile cid i (\b -> b { attachmentState = if taken then Moving else Waiting }) client)
                 { clientTransfers = Map.insert (cid, ref) i (clientTransfers client) }
             , [AcceptFile cid ref (attachmentPath a) | taken] )
    Just contact ->
      let taken = size <= settingAutoAccept (clientAccount client)
          file = Attachment size path 0 (if taken then Moving else Waiting) (Just fileId)
      in ( (using 1 $ withContact cid $ grown cid contact
             { contactMessages = contactMessages contact ++ [Message False name time Delivered (Just file) Nothing 0 Nothing Tox.MessageNormal Nothing newId]
             , contactUnread = contactUnread contact + (if reading (FriendChat cid) then 0 else 1)
             }) { clientTransfers = Map.insert (cid, ref) newId (clientTransfers client) }
         , [AcceptFile cid ref path | taken]
         )
    Nothing -> (client, [CancelFile cid ref])
  Download cid i
    | Just ref <- liveRef cid i ->
        ( adjustFile cid i (\a -> a { attachmentState = Moving }) client
        , [AcceptFile cid ref (attachmentPath a) | Just a <- [fileOf cid i]] )
    | otherwise -> only client
  Cancel cid i ->
    ( adjustFile cid i (\a -> a { attachmentState = Cancelled })
        client { clientTransfers = maybe id (\ref -> Map.delete (cid, ref)) (liveRef cid i) (clientTransfers client) }
    , [CancelFile cid ref | Just ref <- [liveRef cid i]] )
  SendAgain cid i
    | Just m <- messageOf cid i, messageMine m
    , Just a <- messageFile m, attachmentState a == Cancelled ->
        ( adjustFile cid i (\b -> b { attachmentState = Waiting, attachmentDone = 0 }) client
        , [ SendFile cid i (attachmentPath a) (messageText m) (attachmentSize a) (attachmentId a)
          | maybe False contactOnline (Map.lookup cid (clientContacts client)) ] )
    | otherwise -> only client
  FileMoved cid ref done -> only $ case Map.lookup (cid, ref) (clientTransfers client) of
    Just i  -> adjustFile cid i (\a -> a { attachmentDone = done, attachmentState = Moving }) client
    Nothing -> client
  FileEnded cid ref whole -> only $ case Map.lookup (cid, ref) (clientTransfers client) of
    Just i -> adjustFile cid i
      (\a -> if whole then a { attachmentDone = attachmentSize a, attachmentState = Complete }
             else a { attachmentState = Cancelled })
      client { clientTransfers = Map.delete (cid, ref) (clientTransfers client) }
    Nothing -> client
  StartCall cid time camera
    | Nothing <- clientCall client
    , Just contact <- Map.lookup cid (clientContacts client)
    , contactOnline contact ->
        (begun (newCall cid Calling time camera) { callCamera = camera }, [CallFriend cid camera])
    | otherwise -> only client
  AnswerCall time camera -> case clientCall client of
    Just c | callPhase c == Ringing ->
      ( client { clientCall = Just c { callPhase = Talking time, callCamera = camera } }
      , [AnswerFriend (callContact c) camera] )
    _ -> only client
  HangUp time -> case clientCall client of
    Just c  -> ((recorded time False c) { clientCall = Nothing }, [HangUpCall (callContact c)])
    Nothing -> only client
  -- Their call counts as missed, and we don't place ours.
  CallElsewhere time -> case clientCall client of
    Just c  -> ((recorded time (callPhase c == Ringing) c) { clientCall = Nothing }, [HangUpCall (callContact c)])
    Nothing -> only client
  ToggleMute -> only client { clientCall = (\c -> c { callMuted = not (callMuted c) }) <$> clientCall client }
  -- Tox only lets us change this once the call is going.
  ToggleCamera -> case clientCall client of
    Just c | Talking _ <- callPhase c ->
      let camera = not (callCamera c)
      in (client { clientCall = Just c { callCamera = camera } }, [CameraCall (callContact c) camera])
    _ -> only client
  -- Their call came in before ours went out, or it's the call again after
  -- a glare. Either way, answer it.
  CallInvited cid time video -> case clientCall client of
    Just c | callContact c == cid, callPhase c == Calling, callGlare c /= Settling ->
      ( client { clientCall = Just c { callPhase = Talking time, callPeerVideo = video, callGlare = Settling } }
      , [AnswerFriend cid (callCamera c)] )
    Nothing | Map.member cid (clientContacts client) ->
      only (begun (newCall cid Ringing time video) { callPeerVideo = video })
    _ -> (recorded time True (newCall cid Ringing time video), [HangUpCall cid])
  CallPeer cid time video -> only client
    { clientCall = case clientCall client of
        Just c | callContact c == cid -> Just c
          { callPeerVideo = video
          , callPhase = if callPhase c == Calling then Talking time else callPhase c
          }
        other -> other
    }
  CallEnded cid time -> only $ case clientCall client of
    Just c | callContact c == cid -> (recorded time (callPhase c == Ringing) c) { clientCall = Nothing }
    _ -> client
  CallGlared cid time -> case clientCall client of
    Just c | callContact c == cid, callPhase c == Calling, callGlare c == Clear ->
      only client { clientCall = Just c { callGlare = Glared time } }
    _ -> ownUpdate viewing (CallEnded cid time) client
  Tick time
    | let paused = Map.keys (Map.filter ((>= typingPause) . diffUTCTime time) (clientTypingTo client))
    , not (null paused) ->
        let (ticked, commands) = ownUpdate viewing event client { clientTypingTo = foldr Map.delete (clientTypingTo client) paused }
        in (ticked, map (`SetTyping` False) paused ++ commands)
  Tick time -> case clientCall client of
    Just c
      | Glared since <- callGlare c
      , let again = Text.toUpper (Text.take 64 (profileToxId (clientProfile client))) > Text.toUpper key
            ContactId key = callContact c
      , diffUTCTime time since >= (if again then glareDelay else glareWait) ->
          if again
            then (client { clientCall = Just c { callGlare = Settling } }, [CallFriend (callContact c) (callCamera c)])
            else only (recorded time False c) { clientCall = Nothing }
    Just c
      | callPhase c `elem` [Calling, Ringing]
      , diffUTCTime time (callStarted c) >= ringTimeout ->
          ((recorded time (callPhase c == Ringing) c) { clientCall = Nothing }, [HangUpCall (callContact c)])
    _ -> only client
  SendPictures cid way -> only (adjusted cid (\c -> c { contactPictures = way }))
  SendGroupPictures gid way -> only (adjustedGroup gid (\g -> g { groupPictures = way }))
  Invited cid gid kind name bytes
    | Map.member cid (clientContacts client)
    , maybe True ((`notElem` [Joining, InGroup]) . groupPhase) (Map.lookup gid (clientGroups client)) ->
        only client { clientInvites = Map.insert (cid, gid) (Invite kind name bytes Nothing) (clientInvites client) }
    | otherwise -> only client
  AnswerInvite cid gid answer -> case Map.lookup (cid, gid) (clientInvites client) of
    Nothing -> only client
    Just invite -> case answer of
      Nothing -> only client { clientInvites = Map.delete (cid, gid) (clientInvites client) }
      Just password ->
        let online = maybe False contactOnline (Map.lookup cid (clientContacts client))
            -- Keep the chat of a group we left before.
            before = Map.lookup gid (clientGroups client)
            joining = (fromMaybe (newGroup (inviteName invite) (inviteKind invite) Joining) before)
              { groupPhase = Joining, groupMembers = Map.empty, groupSaved = Nothing }
        in ( client
               { clientInvites = if online then Map.delete (cid, gid) (clientInvites client)
                                 else Map.insert (cid, gid) invite { inviteAnswered = Just password } (clientInvites client)
               , clientGroups = Map.insert gid joining (clientGroups client) }
           , [joinCommand cid gid invite password | online] )
  GroupConnected gid time -> case Map.lookup gid (clientGroups client) of
    Just g | groupPhase g == Joining ->
      ( withGroup gid g { groupPhase = InGroup, groupSince = Just time }
      , [SetGroupStatus gid away | let away = profileAway (clientProfile client), away /= Tox.Online]
        ++ [SeedGroupFiles gid whole | let whole = wholeFiles g, not (null whole)]
        -- Messages written while we weren't in it.
        ++ [ SendGroupMessage gid i text | GroupEntry _ (Said Nothing _ text Pending _) i <- groupEntries g ] )
    _ -> only client
  GroupRefused gid why -> case Map.lookup gid (clientGroups client) of
    Just g | groupPhase g `elem` [Joining, InGroup] ->
      -- Quit, or Tox keeps the group around for nothing.
      (withGroup gid g { groupPhase = Refused why, groupMembers = Map.empty, groupSaved = Nothing }, [QuitGroup gid])
    _ -> only client
  -- As totato does, we ask each member that joins for the history it has.
  MemberJoined gid time n member ->
    ( noting gid (\g -> (named member g { groupMembers = Map.insert n member (groupMembers g) }, [GroupEntry time (Arrived member) | arrived g time]))
    , [AskHistory gid n | Just g <- [Map.lookup gid (clientGroups client)], groupKind g == NgcGroup] )
  MemberLeft gid time n how -> only $ noting gid $ \g ->
    ( g { groupMembers = Map.delete n (groupMembers g), groupStatuses = Map.delete n (groupStatuses g) }
    , [ GroupEntry time (Departed member)
      | groupPhase g == InGroup, how `notElem` [Tox.ExitSelfDisconnected, Tox.ExitKicked]
      , Just member <- [Map.lookup n (groupMembers g)] ] )
  MembersListed gid time listed -> case Map.lookup gid (clientGroups client) of
    Just g ->
      let keys = map memberKey . Map.elems
          now = Map.fromList listed
          came = [m | m <- Map.elems now, memberKey m `notElem` keys (groupMembers g)]
          went = [m | m <- Map.elems (groupMembers g), memberKey m `notElem` keys now]
      in ( noting gid (const
             ( g { groupMembers = now }
             -- Silent for the first list after getting in (they were
             -- already there).
             , [ GroupEntry time e
               | arrived g time, not (Map.null (groupMembers g)), e <- map Arrived came ++ map Departed went ] ))
         -- Messages that went to nobody.
         , [ SendGroupMessage gid i text
           | groupPhase g == InGroup, not (Map.null now)
           , GroupEntry _ (Said Nothing _ text Pending _) i <- groupEntries g ] )
    Nothing -> only client
  MemberNamed gid n name -> only $ adjustedGroup gid $ \g ->
    let renamed = Map.adjust (\m -> m { memberName = name }) n (groupMembers g)
    in maybe id named (Map.lookup n renamed) g { groupMembers = renamed }
  GroupReceived gid time n kind text mid -> only $ noting gid $ \g ->
    ( g { groupUnread = groupUnread g + (if reading (GroupChat gid) then 0 else 1) }
    , [GroupEntry time (Said (Just (memberOf g n)) kind text Delivered mid)] )
  GroupFileShared gid time n mid fid -> only $ noting gid $ \g ->
    ( g { groupUnread = groupUnread g + (if reading (GroupChat gid) then 0 else 1) }
    , [GroupEntry time (SharedFile (Just (memberOf g n)) (GroupFile fid Nothing Nothing 0) (Just mid))] )
  GroupFileLearned gid fid inf -> only $ groupFile gid fid $ \f ->
    f { groupFileInfo = Just inf, groupFileData = Just (Attachment (Info.size inf) "" 0 Waiting (Just fid)) }
  DownloadGroupFile gid fid -> case [(inf, a) | GroupEntry _ (SharedFile _ (GroupFile f (Just inf) (Just a) _) _) _ <- maybe [] groupEntries (Map.lookup gid (clientGroups client)), f == fid] of
    (inf, a) : _ | attachmentState a `elem` [Waiting, Interrupted] ->
      (groupFile gid fid (\f -> f { groupFileData = Just a { attachmentState = Moving } }), [FetchGroupFile gid inf])
    _ -> only client
  PauseGroupFile gid fid
    | [()] <- [() | GroupEntry _ (SharedFile _ (GroupFile f _ (Just a) _) _) _ <- maybe [] groupEntries (Map.lookup gid (clientGroups client)), f == fid, attachmentState a == Moving] ->
        (groupFile gid fid (\f -> f { groupFileData = (\a -> a { attachmentState = Interrupted }) <$> groupFileData f, groupFileRate = 0 }), [PauseGroupDownload gid fid])
    | otherwise -> only client
  GroupFileMoved gid fid path done rate -> only $ groupFile gid fid $ \f ->
    f { groupFileData = (\a -> a { attachmentPath = path, attachmentDone = done, attachmentState = Moving }) <$> groupFileData f, groupFileRate = rate }
  GroupFileEnded gid fid whole -> only $ groupFile gid fid $ \f ->
    f { groupFileData = (\a -> a { attachmentState = if whole then Complete else Interrupted }) <$> groupFileData f, groupFileRate = 0 }
  WroteGroupFile gid time path name size | Just g <- Map.lookup gid (clientGroups client), groupPhase g == InGroup ->
    let file = GroupFile BS.empty Nothing (Just (Attachment size path 0 Moving Nothing)) 0
    in ( using 1 (withGroup gid (grownGroup gid g { groupEntries = groupEntries g ++ [GroupEntry time (SharedFile Nothing file Nothing) newId] }))
       , [ShareGroupFile gid newId path name] )
  WroteGroupFile {} -> only client
  GroupFileOffered gid i offered -> only $ adjustedGroup gid $ \g -> g
    { groupEntries = [ case e of
                         GroupEntry t (SharedFile Nothing file _) j | j == i -> GroupEntry t (uncurry (SharedFile Nothing) (ready file)) j
                         _ -> e
                     | e <- groupEntries g ] }
    where
      ready file = case offered of
        Just (inf, mid) -> (file { groupFileId = Info.hash inf, groupFileInfo = Just inf, groupFileData = whole inf <$> groupFileData file }, Just mid)
        Nothing         -> (file { groupFileData = (\a -> a { attachmentState = Cancelled }) <$> groupFileData file }, Nothing)
      whole inf a = a { attachmentDone = attachmentSize a, attachmentState = Complete, attachmentId = Just (Info.hash inf) }
  GroupRecalled gid now n names rs | Just g <- Map.lookup gid (clientGroups client) ->
    let had = Set.fromList ([(memberKey <$> who, mid) | GroupEntry _ (Said who _ _ _ (Just mid)) _ <- groupEntries g]
                              <> [(memberKey <$> who, mid) | GroupEntry _ (SharedFile who _ (Just mid)) _ <- groupEntries g])
        -- As totato does: nothing from more than a minute ahead.
        fresh = nubOrdOn (\(_, who, mid, _) -> (who, mid))
          [r | r@(t, who, mid, _) <- rs, diffUTCTime t now <= 60, Set.notMember (who, mid) had]
        -- Senders may have left: then the chat or the store may know them.
        member key = Member key . fromMaybe "" $
          (memberName . snd <$> memberByKey key g) <|> Map.lookup key names
            <|> listToMaybe [memberName m | e <- reverse (groupEntries g), m <- entryMembers (entryWhat e), memberKey m == key, not (Text.null (memberName m))]
        happenings = [ (t, h) | (t, who, mid, body) <- fresh, Just h <- [happening (member <$> who) mid body] ]
        happening who mid = \case
          History.Text action text -> Just (Said who (if action then Tox.MessageAction else Tox.MessageNormal) text (maybe Sent (const Delivered) who) (Just mid))
          History.Shared kind fid | kind == sha1Info -> Just (SharedFile who (GroupFile fid Nothing Nothing 0) (Just mid))
          History.Shared {} -> Nothing
        added = zipWith (\(t, h) i -> GroupEntry t h i) happenings newIds
        -- After those of the same time, so history keeps its order.
        placed = foldl (flip (insertBy (comparing entryTime <> \_ _ -> GT))) (groupEntries g) added
    in ( using (length added) (withGroup gid (grownGroup gid g { groupEntries = placed }))
       , [LearnGroupFile gid n fid | GroupEntry _ (SharedFile _ file _) _ <- added, let fid = groupFileId file] )
  GroupRecalled {} -> only client
  GroupHistoryWanted gid n range -> case Map.lookup gid (clientGroups client) of
    Just g ->
      let asker = memberKey (memberOf g n)
          -- As totato does: nothing from before the asker first joined.
          since = case [t | GroupEntry t (Arrived m) _ <- groupEntries g, memberKey m == asker] of
            [] -> Nothing
            ts -> Just (minimum ts)
          within t = maybe True (<= t) since && inRange range t
      in (client, [AnswerHistory gid n range [r | e <- reverse (groupEntries g), within (entryTime e), Just r <- [recalled e]]])
    Nothing -> only client
  GroupSent gid i mid -> only $ adjustedGroup gid $ \g -> g
    { groupEntries = [ case e of
                         GroupEntry t (Said Nothing kind text Pending _) j | j == i -> GroupEntry t (Said Nothing kind text Sent mid) j
                         GroupEntry t (PrivateTo who kind text Pending) j | j == i -> GroupEntry t (PrivateTo who kind text Sent) j
                         _ -> e
                     | e <- groupEntries g ] }
  PrivateReceived gid time n kind text -> only $ noting gid $ \g ->
    ( g { groupUnread = groupUnread g + (if reading (GroupChat gid) then 0 else 1) }
    , [GroupEntry time (PrivateFrom (memberOf g n) kind text)] )
  RolesKnown gid self theirs -> only $ adjustedGroup gid $ \g -> g
    { groupSelfRole = self, groupRoles = Map.fromList theirs }
  MemberStatus gid n status -> only $ adjustedGroup gid $ \g -> g
    { groupStatuses = if status == Tox.Online then Map.delete n (groupStatuses g) else Map.insert n status (groupStatuses g) }
  GroupModerated gid time setter target moderation -> case Map.lookup gid (clientGroups client) of
    Just g ->
      let notice = GroupEntry time (Moderated (memberOf g <$> setter) (memberOf g <$> target) moderation)
          kicked = isNothing target && moderation == Tox.ModKick
      in if kicked
           -- Quit, or Tox keeps the group around for nothing.
           then ( noting gid (const (g { groupPhase = Kicked, groupMembers = Map.empty, groupSaved = Nothing }, [notice]))
                , [QuitGroup gid] )
           else only (noting gid (const (g, [notice])))
    Nothing -> only client
  SettingsKnown gid settings -> only $ adjustedGroup gid (\g -> g { groupSettings = Just settings })
  Moderate gid n permission -> (client, [ModerateMember gid n permission])
  ChangeSettings gid settings -> (client, [SetGroupSettings gid settings])
  ChooseRelayBot gid bot -> only $ adjustedGroup gid (\g -> g { groupRelayBot = bot })
  ToggleMentions gid -> only $ adjustedGroup gid (\g -> g { groupMentions = not (groupMentions g) })
  TopicChanged gid time setter topic -> only $ noting gid $ \g ->
    ( g { groupTopic = topic }
    -- Silent for the topic Tox gets while joining.
    , [ GroupEntry time (TopicSet (Just (maybe (Member "" "") (memberOf g) setter)) topic)
      | arrived g time, topic /= groupTopic g ] )
  TopicSetByUs gid time topic -> only $ noting gid $ \g -> (g { groupTopic = topic }, [GroupEntry time (TopicSet Nothing topic)])
  TopicAllowed gid allowed -> only $ adjustedGroup gid (\g -> g { groupMayTopic = allowed })
  SetTopic gid topic
    | Just g <- Map.lookup gid (clientGroups client), groupPhase g == InGroup, groupMayTopic g ->
        (client, [SetGroupTopic gid (Text.strip topic)])
    | otherwise -> only client
  Found founding name -> (client, [FoundGroup founding (Text.strip name) (groupNick (clientProfile client))])
  GroupCreated gid kind name time ->
    only (withGroup gid (newGroup name kind InGroup) { groupSince = Just time, groupSelfRole = Just Tox.Founder })
  JoinPublic gid password ->
    let joining = maybe True ((`notElem` [Joining, InGroup]) . groupPhase) (Map.lookup gid (clientGroups client))
        fresh = newGroup "" NgcGroup Joining
    in ( client { clientGroups = if joining
                    then Map.insert gid (maybe fresh (\g -> g { groupPhase = Joining, groupSaved = Nothing })
                                           (Map.lookup gid (clientGroups client))) (clientGroups client)
                    else clientGroups client }
       , [JoinPublicGroup gid (groupNick (clientProfile client)) password | joining] )
  InviteFriend gid cid -> (client, [InviteToGroup gid cid])
  GroupSaved gid saved -> only $ adjustedGroup gid $ \g ->
    if groupPhase g `elem` [Joining, InGroup] then g { groupSaved = Just saved } else g
  GroupNamed gid name -> only $ adjustedGroup gid $ \g -> g { groupName = name }
  LeaveGroup gid -> case Map.lookup gid (clientGroups client) of
    Just g | groupPhase g `elem` [Joining, InGroup] ->
      (withGroup gid g { groupPhase = Gone, groupMembers = Map.empty, groupSaved = Nothing }, [QuitGroup gid])
    _ -> only client
  DeleteGroup gid -> case Map.lookup gid (clientGroups client) of
    Just g ->
      ( client { clientGroups = Map.delete gid (clientGroups client)
               , clientGroupDrafts = Map.delete gid (clientGroupDrafts client) }
      , [QuitGroup gid | groupPhase g `elem` [Joining, InGroup]] )
    Nothing -> only client
  MuteGroup gid until' -> only $ adjustedGroup gid (\g -> g { groupMute = Just (maybe MutedAlways MutedUntil until') })
  UnmuteGroup gid -> only $ adjustedGroup gid (\g -> g { groupMute = Nothing })
  where
    only c = (c, [])
    -- Tells the friend we stopped typing, if we said we were.
    stopTyping cid (c, commands)
      | Map.member cid (clientTypingTo c) =
          (c { clientTypingTo = Map.delete cid (clientTypingTo c) }, commands ++ [SetTyping cid False])
      | otherwise = (c, commands)
    reading chat = viewingShown viewing && viewingChat viewing == Just chat
    newContact = Contact Nothing False [] 0 Nothing "" Tox.Online Nothing Nothing Nothing False
    ours text time file = Message True text time Pending file Nothing 0 Nothing Tox.MessageNormal Nothing
    messageOf cid i = find ((== i) . messageId) . contactMessages =<< Map.lookup cid (clientContacts client)
    known cid = fromMaybe newContact (Map.lookup cid (clientFormer client))
    entry c record = Message (callMine c) "" (callStarted c) Delivered Nothing (Just record) 0 Nothing Tox.MessageNormal Nothing
    begun c = using 1 $ (adjusted (callContact c) $ \contact -> grown (callContact c) contact
        { contactMessages = contactMessages contact ++ [entry c (CallRecord (callVideo c) Nothing False) newId] })
      { clientCall = Just c { callEntry = Just newId } }
    recorded time missed c = using (maybe 1 (const 0) (callEntry c)) $ adjusted (callContact c) $ \contact -> grown (callContact c) contact
      { contactMessages = case callEntry c of
          Just i  -> [ if messageId m == i then entry c record i else m | m <- contactMessages contact ]
          Nothing -> contactMessages contact ++ [entry c record newId]
      , contactUnread = contactUnread contact + (if missed && not (reading (FriendChat (callContact c))) then 1 else 0)
      }
      where
        record = CallRecord (callVideo c) talked missed
        talked = case callPhase c of
          Talking since -> Just (diffUTCTime time since)
          _             -> Nothing
    groupSend gid to time = case (Map.lookup gid (clientGroups client), Text.strip <$> Map.lookup gid (clientGroupDrafts client)) of
      (Just g, Just text)
        | groupPhase g `elem` [Joining, InGroup], not (Text.null text) ->
            let i = newId
                message = replying client (GroupChat gid) text
                sent written commands =
                  ( using 1 $ (withGroup gid (grownGroup gid g { groupEntries = groupEntries g ++ [GroupEntry time written i] }))
                      { clientGroupDrafts = Map.delete gid (clientGroupDrafts client)
                      , clientGroupReplies = Map.delete gid (clientGroupReplies client) }
                  , commands )
            in case to of
                 Nothing -> sent (Said Nothing Tox.MessageNormal message Pending Nothing) [SendGroupMessage gid i message | groupPhase g == InGroup]
                 Just key | Just (n, m) <- memberByKey key g -> sent (PrivateTo m Tox.MessageNormal message Pending) [SendPrivate gid i n message]
                 -- Never send a private message to everyone if the member is
                 -- gone.
                 Just _ -> only client
      _ -> only client
    withContact cid contact = client { clientContacts = Map.insert cid contact (clientContacts client) }
    withGroup gid g = client { clientGroups = Map.insert gid g (clientGroups client) }
    adjustedGroup gid f = client { clientGroups = Map.adjust f gid (clientGroups client) }
    -- Changes a group and adds entries to its chat.
    noting gid f = case Map.lookup gid (clientGroups client) of
      Just g -> let (g', added) = f g
                in using (length added) (withGroup gid (grownGroup gid g' { groupEntries = groupEntries g' ++ zipWith ($) added newIds }))
      Nothing -> client
    -- Chats not open keep their newest, and those still in use.
    grown cid c = if viewingChat viewing == Just (FriendChat cid) then c else trimmed cid c
    grownGroup gid g = if viewingChat viewing == Just (GroupChat gid) then g else trimmedGroup gid g
    trimmed cid c =
      let kept = newest (viewingPage viewing) (held cid) (contactMessages c)
      in c { contactMessages = kept, contactEarlier = contactEarlier c || length kept < length (contactMessages c) }
    trimmedGroup gid g =
      let kept = newest (viewingPage viewing) (heldEntry gid) (groupEntries g)
      in g { groupEntries = kept, groupEarlier = groupEarlier g || length kept < length (groupEntries g) }
    held cid m = inUse m || Just (messageId m) `elem` [callEntry =<< clientCall client, Map.lookup cid (clientReplies client)]
    heldEntry gid e = entryInUse e || Map.lookup gid (clientGroupReplies client) == Just (entryId e)
    -- IDs for what the event adds, in order. 'using' counts those taken.
    newIds = unused (clientIds client)
    newId = nextId (clientIds client)
    using :: Int -> Client -> Client
    using n c = c { clientIds = (clientIds client) { idsUsed = idsUsed (clientIds client) + fromIntegral n } }
    joinCommand cid gid invite password = case inviteKind invite of
      NgcGroup -> JoinGroup cid gid (inviteData invite) (groupNick (clientProfile client)) password
      _        -> JoinConference cid gid (inviteData invite)
    -- Joined after Tox met the peers already there.
    arrived g time = groupPhase g == InGroup && maybe False ((>= meeting) . diffUTCTime time) (groupSince g)
    memberOf g n = Map.findWithDefault (Member "" "") n (groupMembers g)
    groupFile gid fid f = adjustedGroup gid $ \g -> g
      { groupEntries = [ case e of
                           GroupEntry t (SharedFile who file mid) j | groupFileId file == fid -> GroupEntry t (SharedFile who (f file) mid) j
                           _ -> e
                       | e <- groupEntries g ] }
    -- Files of the group we have whole, where they are.
    wholeFiles g = [ (inf, attachmentPath a)
                   | GroupEntry _ (SharedFile _ (GroupFile _ (Just inf) (Just a) _) _) _ <- groupEntries g, attachmentState a == Complete ]
    -- History.Range is in seconds, inclusive.
    inRange r t = let s = floor (utcTimeToPOSIXSeconds t) in s >= History.oldest r && s <= History.newest r
    recalled e = case entryWhat e of
      Said who kind text _ (Just mid) -> Just (entryTime e, memberKey <$> who, mid, History.Text (kind == Tox.MessageAction) text)
      SharedFile who file (Just mid) -> Just (entryTime e, memberKey <$> who, mid, History.Shared sha1Info (groupFileId file))
      _ -> Nothing
    adjusted cid f = client { clientContacts = Map.adjust f cid (clientContacts client) }
    fileOf cid i = messageFile =<< messageOf cid i
    liveRef cid i = case [ref | ((c, ref), j) <- Map.toList (clientTransfers client), c == cid, j == i] of
      ref : _ -> Just ref
      []      -> Nothing

-- Home of linked devices

-- | For profiles whose account has devices linked here: what happens here
-- becomes events for them, and what they write is done here.
homeUpdate :: Home -> Viewing -> Event -> Client -> (Client, [Command])
homeUpdate h0 viewing event client = case event of
  Pair code _ -> settled [] (Home.pairWith (Just code) h) (client, [])
  Unpair code
    | Home.homePairing h == Just code -> settled [] (Home.pairWith Nothing h) (client, [])
  RemoveDevice cid | Just pk <- contactKey cid, Home.isDevice pk h ->
    let (new, h') = writing [(account, Sync.DeviceGone pk)] h
    in settled new (Home.sessionEnded pk h') (client, [Unfriend cid])
  -- The devices are Tox friends but not contacts, so Tox gets them here.
  Started _ -> settled [] h (fmap (++ [Befriend (contactIdOf pk) | pk <- Map.keys (Home.devices h)]) (ownUpdate viewing event client))
  FriendOnline cid online | Just pk <- contactKey cid, Home.isDevice pk h ->
    if online then settled [] (Home.sessionStarted pk h) (client, [SendSync cid Home.hello])
    else settled [] (Home.sessionEnded pk h) (client, [])
  SyncReceived cid _ packet | Just pk <- contactKey cid ->
    let (taken, h') = Home.receivePacket pk packet (Recording.liveOf client) h
    in fromDevice cid taken h'
  -- A new device with our code.
  Requested cid message | Just pk <- contactKey cid, Just h' <- Home.paired message h ->
    let (new, h'') = writing [(account, Sync.Device pk "")] h'
    in settled new h'' (client, [Befriend cid])
  BlobArrived blob path small
    | Just files <- Map.lookup (Sync.BlobId blob) (Home.homeFetching h) ->
        let h' = h { Home.homeFetching = Map.delete (Sync.BlobId blob) (Home.homeFetching h) }
        in settled [] h' (applying [ \_ -> [WroteFile cid (MessageId i) (Linked.eventTime e) path name size]
                                   | Sync.EventId i <- files, Just e <- [Sync.lookupEvent (Sync.EventId i) (Home.homeStore h)]
                                   , Sync.File _ _ name size _ _ <- [Sync.eventContent e], Just cid <- [friendOf h (Sync.eventGraph e)] ] client)
    | Home.homeAvatar h == Just (Sync.BlobId blob), Just png <- small ->
        settled [] h { Home.homeAvatar = Nothing } (ownUpdate viewing (SetAvatar png) client)
  BlobMissing blob -> missing [Sync.BlobId blob]
  FileHeld _ i blob | Just e <- Recording.eventOf h i ->
    let (new, h') = writing [(Sync.eventGraph e', Sync.FileEnded e True (Just (Sync.BlobId blob))) | Just e' <- [Sync.lookupEvent e (Home.homeStore h)]]
                      h { Home.homeHolding = Set.delete e (Home.homeHolding h) }
    in settled new h' (client, [])
  -- Older messages from the store aren't new.
  EarlierMessages {} -> homeRecorded True [] h client (ownUpdate viewing event client)
  -- What became of a device's new chat.
  GroupCreated gid _ _ _ | i : rest <- Home.homeFounding h ->
    let (c, commands) = ownUpdate viewing event client
        chat = chatIdOf gid =<< Map.lookup gid (clientGroups c)
        (new, h') = writing [(account, Sync.ChatJoined i chat)] h { Home.homeFounding = rest }
    in settled new h' (c, commands)
  ToxRefused (FoundingRefused _) | i : rest <- Home.homeFounding h ->
    let (new, h') = writing [(account, Sync.ChatJoined i Nothing)] h { Home.homeFounding = rest }
    in settled new h' (ownUpdate viewing event client)
  -- Devices don't call the account: it calls them.
  CallInvited cid _ _ | Just pk <- contactKey cid, Home.isDevice pk h -> settled [] h (client, [HangUpCall cid])
  -- The device's end of a call we relay: the friend's ends with it.
  CallEnded cid time | relayedTo cid -> settled [] h (ownUpdate viewing (HangUp time) client)
  CallGlared cid time | relayedTo cid -> settled [] h (ownUpdate viewing (HangUp time) client)
  CallPeer cid _ _ | Just pk <- contactKey cid, Home.isDevice pk h -> settled [] h (client, [])
  -- The friend answered the call a device placed: now we call the device.
  CallPeer cid _ _
    | Just c <- clientCall client, callContact c == cid, callPhase c == Calling, Just device <- callDevice c ->
        settled [] h (fmap (++ [RelayCall cid device (callCamera c)]) (ownUpdate viewing event client))
  _ -> settled [] h (ownUpdate viewing event client)
  where
    relayedTo cid = maybe False ((== Just cid) . callDevice) (clientCall client)
    -- A device answers or places the friend's call, or hangs it up. Only
    -- one call goes on at a time.
    deviceCall device (Sync.PublicKey f) action c =
      let friend = ContactId (hexOf f)
          time = Home.homeClock h
      in case (action, clientCall c) of
        (Protocol.Connect video, Just call)
          | callContact call == friend, callPhase call == Ringing, isNothing (callDevice call) ->
              ( c { clientCall = Just call { callPhase = Talking time, callCamera = video, callDevice = Just device } }
              , [AnswerFriend friend video, RelayCall friend device video] )
        (Protocol.Connect video, Nothing) ->
          let (c', commands) = ownUpdate viewing (StartCall friend time video) c
          in (c' { clientCall = (\call -> call { callDevice = Just device }) <$> clientCall c' }, commands)
        (Protocol.HangUp, Just call)
          | callContact call == friend, maybe (callPhase call == Ringing) (== device) (callDevice call) ->
              ownUpdate viewing (HangUp time) c
        _ -> (c, [])
    h = h0 { Home.homeClock = maybe (Home.homeClock h0) (max (Home.homeClock h0)) (timeOf event) }
    account = Sync.accountGraph (Home.homeAccount h)
    settled new h' result = homeRecorded False new h' client result
    -- Each after the ones before, as the user would do them.
    applying actions c = foldl (\(c', cs) act -> foldl (\(c'', cs') e -> fmap (cs' ++) (ownUpdate viewing e c'')) (c', cs) (act c')) (c, []) actions
    writing = Recording.writeAll (Home.homeClock h)
    -- What a device sent: replies, its events done here, and blobs.
    fromDevice cid taken h' =
      let (h'', actions, commands) = foldl (acting cid) (h', [], []) (Home.takenEvents taken)
          (c, done) = foldl (\(c', cs) (friend, action) -> fmap (cs ++) (deviceCall cid friend action c'))
                        (applying actions client) (Home.takenCalls taken)
          (failed, h3) = failing (Home.takenMissing taken) h''
      in homeRecorded False ([(i, bytes) | (i, _, bytes) <- Home.takenEvents taken] ++ failed) h3 client
           (c, [SendSync cid (Home.takenReplies taken) | not (null (Home.takenReplies taken))]
                 ++ [ServeBlob cid blob (Linked.noBlob (Sync.BlobId blob)) | Sync.BlobId blob <- Home.takenWanted taken]
                 ++ commands ++ done)
    missing blobs = let (failed, h') = failing blobs h in settled failed h' (client, [])
    -- Devices' files whose blobs didn't come fail, and so does an avatar.
    failing blobs h' =
      let files = concat [Map.findWithDefault [] b (Home.homeFetching h') | b <- blobs]
          (new, h'') = writing [ (Sync.eventGraph e, Sync.FileEnded i False Nothing)
                               | i <- files, Just e <- [Sync.lookupEvent i (Home.homeStore h')] ] h'
      in (new, h'' { Home.homeFetching = foldr Map.delete (Home.homeFetching h'') blobs
                   , Home.homeAvatar = mfilter (`notElem` blobs) (Home.homeAvatar h'') })
    acting cid (h', actions, commands) (i@(Sync.EventId bytes), e, _) = case Sync.eventContent e of
      Sync.Send _ said kind | Just friend <- friendOf h' g ->
        (h', actions ++ [const [Wrote friend (MessageId bytes) (Linked.eventTime e) (toxKind kind) said]], commands)
      Sync.FileAnswered file accepted | Just friend <- friendOf h' g, Just mid <- messageFor h' file ->
        (h', actions ++ [const [if accepted then Download friend mid else Cancel friend mid]], commands)
      Sync.File _ _ name _ False (Just blob@(Sync.BlobId b)) | Just _ <- friendOf h' g ->
        ( h' { Home.homeFetching = Map.insertWith (flip (++)) blob [i] (Home.homeFetching h') }
        , actions, commands ++ [FetchBlob cid b (Home.askBlob blob) (Just name)] )
      Sync.ReadUpTo g' _ rank | Just friend <- friendOf h' g', maybe True ((<= rank) . fst) (Map.lookup g' (Home.homeLatest h')) ->
        (h', actions ++ [const [MarkRead (FriendChat friend)]], commands)
      Sync.WantFriend pk message nospam -> let friend = contactIdOf pk in
        (h', actions ++ [\c -> if Map.member friend (clientRequests c) then [AcceptRequest friend]
                               else [ AddFriend (toxId (keyHex pk) (nospamText n)) (fromMaybe "" message)
                                    | Map.notMember friend (clientContacts c), Just n <- [nospam] ]], commands)
      Sync.UnwantFriend pk -> let friend = contactIdOf pk in
        (h', actions ++ [\c -> [DeleteContact friend True | Map.member friend (clientContacts c)]
                               ++ [RejectRequest friend | Map.member friend (clientRequests c)]], commands)
      Sync.Wish Sync.AccountSubject v -> case v of
        Sync.Name n -> (h', actions ++ [\c -> [ChangeProfile n (profileStatus (clientProfile c))]], commands)
        Sync.StatusMessage t -> (h', actions ++ [\c -> [ChangeProfile (profileNickname (clientProfile c)) t]], commands)
        Sync.Status st -> (h', actions ++ [const [SetAway (toEnum (fromEnum st))]], commands)
        Sync.Avatar Nothing -> (h', actions ++ [const [RemoveAvatar]], commands)
        Sync.Avatar (Just blob@(Sync.BlobId b)) ->
          (h' { Home.homeAvatar = Just blob }, actions, commands ++ [FetchBlob cid b (Home.askBlob blob) Nothing])
        _ -> (h', actions, commands)
      Sync.SettingContent (Sync.ConversationScope g') setting | Just friend <- friendOf h' g' -> case Sync.keptValue setting of
        Sync.Mute until' _ -> (h', actions ++ [const [MuteChat friend (Linked.fromMillis . fromIntegral <$> until')]], commands)
        Sync.PictureFormat way -> (h', actions ++ [const [SendPictures friend (picturesOf way)]], commands)
        _ -> (h', actions, commands)
      Sync.ClearSetting (Sync.ConversationScope g') kind | Just friend <- friendOf h' g' ->
        (h', actions ++ [const ([UnmuteChat friend | fromIntegral kind == muteKind] ++ [SendPictures friend Nothing | fromIntegral kind == picturesKind])], commands)
      Sync.DeviceGone pk -> (Home.sessionEnded pk h', actions, commands ++ [Unfriend (contactIdOf pk)])
      -- Groups and conferences.
      Sync.Send _ said kind | Just gid <- groupFor g ->
        (h', actions ++ [const [WroteGroup gid (MessageId bytes) (Linked.eventTime e) (toxKind kind) said]], commands)
      Sync.SendPrivately (Sync.PublicKey to) _ said kind | Just gid <- groupFor g ->
        (h', actions ++ [const [WrotePrivately gid (MessageId bytes) (Linked.eventTime e) (hexOf to) (toxKind kind) said]], commands)
      Sync.ReadUpTo g' _ rank | Just gid <- groupFor g', maybe True ((<= rank) . fst) (Map.lookup g' (Home.homeLatest h')) ->
        (h', actions ++ [const [MarkRead (GroupChat gid)]], commands)
      Sync.NewChat kind name privacy ->
        ( h' { Home.homeFounding = Home.homeFounding h' ++ [i] }
        , actions ++ [const [Found (case (kind, privacy) of
                                      (Sync.ConferenceKind, _) -> NewConference
                                      (_, Sync.Private)        -> PrivateGroup
                                      _                        -> PublicGroup) name]], commands )
      Sync.JoinChat chatId password ->
        ( h' { Home.homeJoining = Home.homeJoining h' ++ [(i, chatId)] }
        , actions ++ [const [JoinPublic (GroupId (hexOf chatId)) (fromMaybe "" password)]], commands )
      Sync.InviteToChat c (Sync.PublicKey friend) ->
        (h', actions ++ [const [InviteFriend (chatGroupId c) (ContactId (hexOf friend))]], commands)
      Sync.AnswerInvite invite join password | Just (cid', gid) <- inviteHere invite ->
        (h', actions ++ [const [AnswerInvite cid' gid (if join then Just (fromMaybe "" password) else Nothing)]], commands)
      Sync.RelayBot c (Sync.PublicKey bot) relays ->
        (h', actions ++ [const [ChooseRelayBot (chatGroupId c) (if relays then Just (hexOf bot) else Nothing)]], commands)
      Sync.Wish (Sync.ChatSubject c) v -> let gid = chatGroupId c in case v of
        Sync.Topic topic -> (h', actions ++ [const [SetTopic gid topic]], commands)
        Sync.Joined False -> (h', actions ++ [const [LeaveGroup gid]], commands)
        _ -> (h', actions ++ [\c' -> [ ChangeSettings gid (wished v settings)
                                     | Just settings <- [groupSettings =<< Map.lookup gid (clientGroups c')] ]], commands)
      Sync.Wish (Sync.PeerSubject c pk) v -> let gid = chatGroupId c in
        (h', actions ++ [\c' -> [ Moderate gid n act
                                | Just g' <- [Map.lookup gid (clientGroups c')], Just (n, _) <- [memberByKey (keyHex pk) g']
                                , Just act <- [case v of
                                                 Sync.Present False -> Just Kick
                                                 Sync.RoleValue Sync.Moderator -> Just MakeModerator
                                                 Sync.RoleValue Sync.User -> Just MakeUser
                                                 Sync.RoleValue Sync.Observer -> Just MakeObserver
                                                 _ -> Nothing] ]], commands)
      Sync.SettingContent (Sync.ConversationScope g') setting | Just gid <- groupFor g', Sync.Mute until' mentioned <- Sync.keptValue setting ->
        (h', actions ++ [\c' -> MuteGroup gid (Linked.fromMillis . fromIntegral <$> until')
                                : [ToggleMentions gid | maybe False ((/= mentioned) . groupMentions) (Map.lookup gid (clientGroups c'))]], commands)
      Sync.SettingContent (Sync.ConversationScope g') setting | Just gid <- groupFor g', Sync.PictureFormat way <- Sync.keptValue setting ->
        (h', actions ++ [const [SendGroupPictures gid (picturesOf way)]], commands)
      Sync.ClearSetting (Sync.ConversationScope g') kind | Just gid <- groupFor g' ->
        (h', actions ++ [const ([UnmuteGroup gid | fromIntegral kind == muteKind] ++ [SendGroupPictures gid Nothing | fromIntegral kind == picturesKind])], commands)
      _ -> (h', actions, commands)
      where g = Sync.eventGraph e
    groupFor g = listToMaybe [gid | (gid, grp) <- Map.toList (clientGroups client), Just c <- [chatIdOf gid grp], Sync.chatGraph c == g]
    -- The invite here that the account's invite event is about.
    inviteHere invite = case Map.lookup invite (Sync.accountInvites (Home.homeState h)) of
      Just v -> listToMaybe [ (cid', gid) | ((cid', gid), here) <- Map.toList (clientInvites client)
                            , contactKey cid' == Just (Sync.inviteFrom v), inviteData here == Sync.inviteData v ]
      Nothing -> Nothing
    wished v settings = case v of
      Sync.PrivacyValue p -> settings { Tox.groupPrivate = p == Sync.Private }
      Sync.PeerLimit n -> settings { Tox.groupPeerLimit = fromIntegral n }
      Sync.Password p -> settings { Tox.groupPassword = Text.encodeUtf8 p }
      Sync.TopicLock locked -> settings { Tox.groupTopicLock = locked }
      Sync.VoiceStateValue voice -> settings { Tox.groupVoice = case voice of
        Sync.VoiceEveryone   -> Tox.VoiceAll
        Sync.VoiceModerators -> Tox.VoiceModerators
        Sync.VoiceFounder    -> Tox.VoiceFounder }
      _ -> settings
    nospamText n = hexOf (BS.pack [fromIntegral (n `shiftR` s) | s <- [24, 16, 8, 0]])
    toxKind Sync.Action = Tox.MessageAction
    toxKind Sync.Normal = Tox.MessageNormal

-- | Writes what changed as events for the devices, and sends them. New
-- messages and entries take their events' IDs, also in the commands.
homeRecorded :: Bool -> [(Sync.EventId, BS.ByteString)] -> Home -> Client -> (Client, [Command]) -> (Client, [Command])
homeRecorded paging taken h old (client, commands) =
  ( Recording.renamedIn renames client { clientHome = Just home }
  , map renamedCommand commands
      ++ [SendSync (contactIdOf pk) packets | (pk, packets) <- Home.pushed (Recording.recordedNew r) home ++ Recording.recordedLive r]
      ++ map StoreBlob (Recording.recordedBlobs r)
      ++ [HoldFile cid (renamed i) path | (cid, i, path) <- Recording.recordedHolds r] )
  where
    r = Recording.record paging taken h old client
    home = Recording.recordedHome r
    renames = Recording.recordedRenames r
    renamed i = Map.findWithDefault i i renames
    renamedCommand = \case
      SendMessage cid i text n -> SendMessage cid (renamed i) text n
      SendFile cid i path name size fileId -> SendFile cid (renamed i) path name size fileId
      SendGroupMessage gid i text -> SendGroupMessage gid (renamed i) text
      SendPrivate gid i n text -> SendPrivate gid (renamed i) n text
      other -> other

-- | The message ID of an event.
messageFor :: Home -> Sync.EventId -> Maybe MessageId
messageFor h i@(Sync.EventId bytes) = case [m | (m, e) <- Map.toList (Home.homeEarlier h), e == i] of
  m : _ -> Just (MessageId m)
  []    -> Just (MessageId bytes)

-- | The friend an account's chat is with, by the chat's graph.
friendOf :: Home -> Sync.GraphId -> Maybe ContactId
friendOf h g = listToMaybe
  [ contactIdOf pk | (pk, f) <- Map.toList (Sync.accountFriends (Home.homeState h)), Sync.friendWanted f
  , Sync.friendGraph (Home.homeAccount h) pk == g ]

-- | When it happened, for events that say.
timeOf :: Event -> Maybe UTCTime
timeOf = \case
  Received _ t _ _ -> Just t
  Tick t -> Just t
  Send _ t -> Just t
  FileOffered _ _ t _ _ _ _ -> Just t
  CallInvited _ t _ -> Just t
  CallEnded _ t -> Just t
  Typing _ t -> Just t
  StartCall _ t _ -> Just t
  AnswerCall t _ -> Just t
  HangUp t -> Just t
  Forward _ _ _ t -> Just t
  Pair _ t -> Just t
  SyncReceived _ t _ -> Just t
  CallPeer _ t _ -> Just t
  CallGlared _ t -> Just t
  _ -> Nothing

-- Linked accounts

-- | For linked profiles. What the user does becomes events for the home,
-- and the chats are derived from the account's events. Our own Tox only
-- has the home as a friend.
linkedUpdate :: Linked -> Viewing -> Event -> Client -> (Client, [Command])
linkedUpdate l viewing event client = case event of
  Started _ ->
    ( linkedView l client
    , [maybe (Befriend home) (RequestFriend (linkedHomeAddress l)) (Linked.linkedPairing l)] )
  FriendOnline cid online
    | cid == home && online -> (client, [SendSync home (Linked.homeOnline l)])
    | cid == home -> linked (Linked.homeOffline l) client []
  SyncReceived cid _ packet
    | cid == home ->
        let (replies, asked, l') = Linked.receivePacket packet l
            (c, commands) = linked l' client replies
        in (c, commands ++ [ServeBlob home blob (Linked.noBlob (Sync.BlobId blob)) | Sync.BlobId blob <- asked])
  BlobMissing blob -> linked (Linked.blobMissing (Sync.BlobId blob) l) client []
  BlobArrived blob path small ->
    let here = Linked.blobHere (Sync.BlobId blob) path l
        avatars = case small of
          Just png | blob `elem` avatarBlobs l -> Map.insert blob png (clientAvatars client)
          _                                    -> clientAvatars client
    in linked here client { clientAvatars = avatars } []
  -- The home accepts theirs, then we fetch it from the home.
  Download cid i | Just (fid, file) <- linkedFileAt l i -> case Map.lookup fid (linkedEnds l) of
    Just (True, Just blob) ->
      let (l', commands) = fetchFile l blob file in (fst (linked l' client []), commands)
    Nothing | Map.notMember fid (linkedAnswers l) ->
      let (l', packets) = authored (friendGraphOf cid) now (Sync.FileAnswered fid True) l in linked l' client packets
    _ -> only client
  -- The home device has our file.
  SendAgain {} -> only client
  Cancel cid i | Just (fid, _) <- linkedFileAt l i, Map.notMember fid (linkedEnds l) ->
    let (l', packets) = authored (friendGraphOf cid) now (Sync.FileAnswered fid False) l in linked l' client packets
  -- Staged items first, then the text. Files go once stored as blobs.
  Send (FriendChat cid) time
    | let staged = Map.findWithDefault [] cid (clientStaged client)
          draft = maybe "" Text.strip (Map.lookup cid (clientDrafts client))
    , not (null staged) || not (Text.null draft) ->
        let text = replying client (FriendChat cid) draft
            g = friendGraphOf cid
            said = [locationText latitude longitude | StagedPlace latitude longitude <- staged]
              ++ [text | not (Text.null draft)]
            (l', packets) = foldl (\(l0, ps) t ->
                                     let (l1, p) = authored g time (Sync.Send (senderIn g) (Text.encodeUtf8 t) Sync.Normal) l0
                                     in (l1, ps ++ p)) (l, []) said
            (sending, commands) = linked l' client
              { clientDrafts = Map.delete cid (clientDrafts client)
              , clientStaged = Map.delete cid (clientStaged client)
              , clientReplies = if Text.null draft then clientReplies client else Map.delete cid (clientReplies client) }
              packets
        in (sending, [ShareFile cid path name size | StagedFile path name size _ <- staged] ++ commands)
  Send (GroupChat gid) time -> groupSend gid time (\g bytes -> Just (Sync.Send (senderIn g) bytes Sync.Normal))
  SendPrivately gid key time -> groupSend gid time $ \g bytes -> do
    member <- memberByKey key =<< Map.lookup gid (clientGroups client)
    pk <- keyOf (memberKey (snd member))
    pure (Sync.SendPrivately pk (senderIn g) bytes Sync.Normal)
  AddFriend address message | Just pk <- keyOf (Text.take 64 address) ->
    let nospam = nospamOf (Text.take 8 (Text.drop 64 address))
        (l', packets) = authored account now (Sync.WantFriend pk (Just (Text.strip message)) nospam) l
    in linked l' client packets
  AcceptRequest cid -> friendWish cid True
  RejectRequest cid -> friendWish cid False
  DeleteContact cid _ -> friendWish cid False
  MuteChat cid until' -> setting muteKind (friendGraphOf cid) (Just (muteOf until' True))
  UnmuteChat cid -> setting muteKind (friendGraphOf cid) Nothing
  -- Sending pictures as they are is the default, so clear the setting.
  SendPictures cid way -> setting picturesKind (friendGraphOf cid) (Sync.PictureFormat <$> (syncPictures =<< way))
  MuteGroup gid until' | Just c <- chatOf gid ->
    setting muteKind (Sync.chatGraph c) (Just (muteOf until' (maybe True groupMentions (Map.lookup gid (clientGroups client)))))
  UnmuteGroup gid | Just c <- chatOf gid -> setting muteKind (Sync.chatGraph c) Nothing
  SendGroupPictures gid way | Just c <- chatOf gid -> setting picturesKind (Sync.chatGraph c) (Sync.PictureFormat <$> (syncPictures =<< way))
  ToggleMentions gid
    | Just c <- chatOf gid, Just g <- Map.lookup gid (clientGroups client), Just m <- groupMute g ->
        setting muteKind (Sync.chatGraph c) (Just (muteOf (case m of MutedUntil t -> Just t; MutedAlways -> Nothing)
                                                 (not (groupMentions g))))
  AnswerInvite _ (GroupId invite) answer
    | Just i <- Sync.EventId <$> unhex' invite ->
        let password = mfilter (not . Text.null) answer
            (l', packets) = authored account now (Sync.AnswerInvite i (isJust answer) password) l
        in linked l' client packets
  LeaveGroup gid | Just c <- chatOf gid -> leaving c
  -- We only show chats the account is in, so deleting one leaves it.
  DeleteGroup gid | Just c <- chatOf gid -> leaving c
  ChangeProfile nickname status ->
    let (l1, p1) = authored account now (Sync.Wish Sync.AccountSubject (Sync.Name (Text.strip nickname))) l
        (l2, p2) = authored account now (Sync.Wish Sync.AccountSubject (Sync.StatusMessage (Text.strip status))) l1
    in linked l2 client (p1 ++ p2)
  SetAway away ->
    let (l', packets) = authored account now (Sync.Wish Sync.AccountSubject (Sync.Status (toEnum (fromEnum away)))) l
    in linked l' client packets
  SetTopic gid topic | Just c <- chatOf gid ->
    let (l', packets) = authored account now (Sync.Wish (Sync.ChatSubject c) (Sync.Topic (Text.strip topic))) l
    in linked l' client packets
  ChooseRelayBot gid bot | Just c <- chatOf gid ->
    case (bot, Map.lookup c (Sync.accountRelayBots (Linked.accountState l))) of
      (Just key, _) | Just pk <- keyOf key ->
        let (l', packets) = authored account now (Sync.RelayBot c pk True) l in linked l' client packets
      (Nothing, Just pk) ->
        let (l', packets) = authored account now (Sync.RelayBot c pk False) l in linked l' client packets
      _ -> only client
  -- From our own Tox, whose only friend is the home.
  FriendNamed {} -> only client
  FriendStatusMessage {} -> only client
  FriendStatus {} -> only client
  FriendAvatar {} -> only client
  Received {} -> only client
  Requested {} -> only client
  AddFailed {} -> only client
  Invited {} -> only client
  FileShared cid time name size blob path ->
    let g = friendGraphOf cid
        (l', packets) = authored g time (Sync.File (senderIn g) Nothing name size False (Just (Sync.BlobId blob))) l
    in linked (Linked.blobHere (Sync.BlobId blob) path l') client packets
  SetAvatar png ->
    let hash = SHA256.hash png
        (l', packets) = authored account now (Sync.Wish Sync.AccountSubject (Sync.Avatar (Just (Sync.BlobId hash)))) l
        (c, commands) = linked l' client { clientAvatars = Map.insert hash png (clientAvatars client) } packets
    in (c, StoreBlob png : commands)
  RemoveAvatar ->
    let (l', packets) = authored account now (Sync.Wish Sync.AccountSubject (Sync.Avatar Nothing)) l
    in linked l' client packets
  Found founding name ->
    let (kind, privacy) = case founding of
          PrivateGroup  -> (Sync.GroupKind, Sync.Private)
          PublicGroup   -> (Sync.GroupKind, Sync.Public)
          NewConference -> (Sync.ConferenceKind, Sync.Public)
        (l', packets) = authored account now (Sync.NewChat kind (Text.strip name) privacy) l
    in linked l' client packets
  JoinPublic (GroupId chat) password | Just chatId <- unhex' chat ->
    let (l', packets) = authored account now (Sync.JoinChat chatId (mfilter (not . Text.null) (Just password))) l
    in linked l' client packets
  InviteFriend gid (ContactId key) | Just c <- chatOf gid, Just friend <- keyOf key ->
    let (l', packets) = authored account now (Sync.InviteToChat c friend) l
    in linked l' client packets
  -- The home answers or places the friend's call, then calls us and relays
  -- it.
  AnswerCall time camera -> case clientCall client of
    Just c | callPhase c == Ringing ->
      viaHome (client { clientCall = Just c { callPhase = Calling, callStarted = time, callCamera = camera } }, [CallFriend (callContact c) camera])
    _ -> only client
  StartCall {} -> viaHome (ownUpdate viewing event client)
  HangUp {} -> viaHome (ownUpdate viewing event client)
  ToggleCamera -> viaHome (ownUpdate viewing event client)
  CallElsewhere {} -> viaHome (ownUpdate viewing event client)
  -- The home gives up on a ringing call, not us.
  Tick {}
    | (callPhase <$> clientCall client) == Just Ringing -> only client
    | otherwise -> viaHome (ownUpdate viewing event client)
  CallInvited cid time video | cid == home, Just friend <- relaying -> viaHome (ownUpdate viewing (CallInvited friend time video) client)
  CallPeer cid time video | cid == home, Just friend <- relaying -> viaHome (ownUpdate viewing (CallPeer friend time video) client)
  CallEnded cid time | cid == home, Just friend <- relaying -> viaHome (ownUpdate viewing (CallEnded friend time) client)
  CallGlared cid time | cid == home, Just friend <- relaying -> viaHome (ownUpdate viewing (CallEnded friend time) client)
  Moderate {} -> wishing
  ChangeSettings {} -> wishing
  -- Only for accounts whose Tox runs here.
  Typing {} -> only client
  FriendTyping {} -> only client
  _ -> readToEnd viewing l (fst (ownUpdate viewing event client)) []
  where
    only c = (c, [])
    linked = withLinked viewing
    leaving c =
      let (l', packets) = authored account now (Sync.Wish (Sync.ChatSubject c) (Sync.Joined False)) l
      in linked l' client packets
    home = ContactId (Text.toUpper (Text.take 64 (linkedHomeAddress l)))
    account = Sync.accountGraph (Linked.accountKey l)
    now = linkedClock l
    friendGraphOf (ContactId key) = Sync.friendGraph (Linked.accountKey l) (fromMaybe (Sync.PublicKey BS.empty) (keyOf key))
    chatOf gid = listToMaybe [c | c <- Sync.joinedChats (Linked.accountState l), chatGroupId c == gid]
    senderIn = linkedSender l
    groupSend gid time content
      | Just c <- chatOf gid
      , Just draft <- Text.strip <$> Map.lookup gid (clientGroupDrafts client), not (Text.null draft)
      , Just made <- content (Sync.chatGraph c) (Text.encodeUtf8 (replying client (GroupChat gid) draft)) =
          let (l', packets) = authored (Sync.chatGraph c) time made l
          in linked l' client { clientGroupDrafts = Map.delete gid (clientGroupDrafts client)
                              , clientGroupReplies = Map.delete gid (clientGroupReplies client) } packets
      | otherwise = only client
    relaying = case clientCall client of
      Just c | callVia c == Just home -> Just (callContact c)
      _                               -> Nothing
    -- Call commands go to the home, which relays the call. We ask it to
    -- place new ones.
    viaHome (c, commands) =
      let c' = c { clientCall = (\call -> call { callVia = Just home }) <$> clientCall c }
          toHome = \case
            CallFriend (ContactId key) camera | Just pk <- keyOf key -> [SendSync home (Linked.callAction pk (Protocol.Connect camera))]
            AnswerFriend _ camera -> [AnswerFriend home camera]
            HangUpCall (ContactId key) ->
              HangUpCall home : [SendSync home (Linked.callAction pk Protocol.HangUp) | Just pk <- [keyOf key]]
            CameraCall _ camera -> [CameraCall home camera]
            other -> [other]
      in (c', concatMap toHome commands)
    -- Turn Tox commands into wishes for the home.
    wishing =
      let (c, commands) = ownUpdate viewing event client
          (l', packets) = foldl (\(l0, ps) (subject', value') ->
                                   let (l1, p) = authored account now (Sync.Wish subject' value') l0 in (l1, ps ++ p))
                                (l, []) (concatMap wishesOf commands)
      in linked l' c packets
    wishesOf = \case
      ModerateMember gid n action
        | Just c <- chatOf gid
        , Just pk <- keyOf . memberKey =<< Map.lookup n . groupMembers =<< Map.lookup gid (clientGroups client) ->
            [(Sync.PeerSubject c pk, case action of
                MakeModerator -> Sync.RoleValue Sync.Moderator
                MakeUser      -> Sync.RoleValue Sync.User
                MakeObserver  -> Sync.RoleValue Sync.Observer
                Kick          -> Sync.Present False)]
      SetGroupSettings gid new
        | Just c <- chatOf gid, Just old <- groupSettings =<< Map.lookup gid (clientGroups client) ->
            let differs f = f old /= f new
            in [ (Sync.ChatSubject c, v)
               | (True, v) <-
                   [ (differs Tox.groupPrivate, Sync.PrivacyValue (if Tox.groupPrivate new then Sync.Private else Sync.Public))
                   , (differs Tox.groupPeerLimit, Sync.PeerLimit (fromIntegral (Tox.groupPeerLimit new)))
                   , (differs Tox.groupPassword, Sync.Password (Text.decodeUtf8Lenient (Tox.groupPassword new)))
                   , (differs Tox.groupTopicLock, Sync.TopicLock (Tox.groupTopicLock new))
                   , (differs Tox.groupVoice, Sync.VoiceStateValue (syncVoice (Tox.groupVoice new))) ] ]
      _ -> []
    friendWish (ContactId key) wanted = case keyOf key of
      Just pk ->
        let content = if wanted then Sync.WantFriend pk Nothing Nothing else Sync.UnwantFriend pk
            (l', packets) = authored account now content l
        in linked l' client packets
      Nothing -> only client
    -- Mute or picture format of a conversation. Keep any fields that newer
    -- clients added.
    setting kind g s' =
      let scope = Sync.ConversationScope g
          old = Map.lookup kind =<< Map.lookup scope (Sync.accountSettings (Linked.accountState l))
          content = maybe (Sync.ClearSetting scope (fromIntegral kind))
            (\new -> Sync.SettingContent scope (maybe (Sync.kept new) (Sync.withSetting new) old)) s'
          (l', packets) = authored account now content l
      in linked l' client packets
    muteOf until' = Sync.Mute (fromIntegral . Linked.toMillis <$> until')
    nospamOf t = case unhex' t of
      Just bytes | BS.length bytes == 4 -> Just (BS.foldl' (\n b -> n * 256 + fromIntegral b) 0 bytes)
      _ -> Nothing

withLinked :: Viewing -> Linked -> Client -> [BS.ByteString] -> (Client, [Command])
withLinked viewing l client = readToEnd viewing l (linkedView l client { clientLinked = Just l })

-- | Mark the open chat read to its end, as an event for the other devices.
readToEnd :: Viewing -> Linked -> Client -> [BS.ByteString] -> (Client, [Command])
readToEnd viewing l0 client0 packets = case unreadEnd of
  Just (g, i, rank) ->
    let (l', more) = authored (Sync.accountGraph (Linked.accountKey l)) (linkedClock l) (Sync.ReadUpTo g i rank) l
    in (linkedView l' client { clientLinked = Just l' }, send (packets ++ more) ++ fetches)
  Nothing -> (client, send packets ++ fetches)
  where
    (l, fetches) = fetching l0 client0
    client = client0 { clientLinked = Just l }
    home = ContactId (Text.toUpper (Text.take 64 (linkedHomeAddress l)))
    send ps = [SendSync home ps | not (null ps)]
    store = Replica.replicaStore (Linked.linkedReplica l)
    shown g = viewingShown viewing && viewingEnd viewing && case viewingChat viewing of
      Just (FriendChat (ContactId key)) -> Just g == (Sync.friendGraph (Linked.accountKey l) <$> keyOf key)
      Just (GroupChat gid) -> g `elem` [Sync.chatGraph c | c <- Sync.joinedChats (Linked.accountState l), chatGroupId c == gid]
      Nothing -> False
    unreadEnd = listToMaybe
      [ (g, i, Sync.eventRank e)
      | g <- Sync.storeGraphs store, shown g
      , (i, e) <- take 1 (reverse (Sync.byRank g store))
      , unreadIn (Linked.accountState l) g e ]

-- | Whether no device has read the event yet. Read up to a rank means
-- everything of that rank or lower.
unreadIn :: Sync.AccountState -> Sync.GraphId -> Sync.Event -> Bool
unreadIn state g e = maybe True ((< Sync.eventRank e) . fst) (Map.lookup g (Sync.accountRead state))

-- | Ask the home for avatar blobs we don't have (once per session), and for
-- accepted files.
fetching :: Linked -> Client -> (Linked, [Command])
fetching l client = foldl (\(l', commands) (blob, file) -> (<>) commands <$> fetchFile l' blob file)
  (foldl ask (l, []) missing) files
  where
    missing = [ blob | Linked.inSession l, blob <- avatarBlobs l, Map.notMember blob (clientAvatars client)
                     , Set.notMember (Sync.BlobId blob) (Linked.linkedWanted l) ]
    files = [ (blob, file) | (fid, file@(_, size)) <- linkedFiles l
                           , size <= settingAutoAccept (clientAccount client) || Map.lookup fid answers == Just True
                           , Just (True, Just blob) <- [Map.lookup fid ends] ]
      -- Ours, stored here as blobs (to find them again after a restart).
      ++ linkedOurs l
    ends = linkedEnds l
    answers = linkedAnswers l
    home = ContactId (Text.toUpper (Text.take 64 (linkedHomeAddress l)))
    ask (l', commands) blob =
      let (packets, l'') = Linked.wantBlob (Sync.BlobId blob) l' in (l'', commands ++ [FetchBlob home blob packets Nothing])

-- | Use the blob if we have it, else ask the home in a session.
fetchFile :: Linked -> Sync.BlobId -> (Text, Word64) -> (Linked, [Command])
fetchFile l blob@(Sync.BlobId bytes) (name, _)
  | Map.member blob (Linked.linkedHave l) = (l, [])
  | Linked.inSession l, Set.notMember blob (Linked.linkedWanted l) =
      let (packets, l') = Linked.wantBlob blob l in (l', [FetchBlob home bytes packets (Just name)])
  | Set.notMember blob (Linked.linkedLooked l) = (Linked.lookForBlob blob l, [FetchBlob home bytes [] (Just name)])
  | otherwise = (l, [])
  where home = ContactId (Text.toUpper (Text.take 64 (linkedHomeAddress l)))

-- | Files friends sent the account.
linkedFiles :: Linked -> [(Sync.EventId, (Text, Word64))]
linkedFiles l =
  [ (i, (name, size)) | g <- Sync.storeGraphs store, (i, e) <- Sync.byRank g store
                      , Sync.File _ _ name size True _ <- [Sync.eventContent e] ]
  where store = Replica.replicaStore (Linked.linkedReplica l)

-- | Our files sent by the account's devices.
linkedOurs :: Linked -> [(Sync.BlobId, (Text, Word64))]
linkedOurs l =
  [ (blob, (name, size)) | g <- Sync.storeGraphs store, (_, e) <- Sync.byRank g store
                         , Sync.File _ _ name size False (Just blob) <- [Sync.eventContent e] ]
  where store = Replica.replicaStore (Linked.linkedReplica l)

-- | How each file ended, if it did: OK or not, and the home's blob.
linkedEnds :: Linked -> Map Sync.EventId (Bool, Maybe Sync.BlobId)
linkedEnds l = Map.unions
  [ Map.fromList [ (i, (ok, blob)) | (_, e) <- Sync.byRank g store, Sync.FileEnded i ok blob <- [Sync.eventContent e] ]
  | g <- Sync.storeGraphs store ]
  where store = Replica.replicaStore (Linked.linkedReplica l)

-- | Latest accept or decline of each file.
linkedAnswers :: Linked -> Map Sync.EventId Bool
linkedAnswers l = Map.fromList
  [ (i, yes) | g <- Sync.storeGraphs store, (_, e) <- Sync.byRank g store, Sync.FileAnswered i yes <- [Sync.eventContent e] ]
  where store = Replica.replicaStore (Linked.linkedReplica l)

-- | A friend's file, by its message.
linkedFileAt :: Linked -> MessageId -> Maybe (Sync.EventId, (Text, Word64))
linkedFileAt l (MessageId i) = case Sync.eventContent <$> Sync.lookupEvent fid (Replica.replicaStore (Linked.linkedReplica l)) of
  Just (Sync.File _ _ name size True _) -> Just (fid, (name, size))
  _                                     -> Nothing
  where fid = Sync.EventId i

avatarBlobs :: Linked -> [BS.ByteString]
avatarBlobs l = nub [blob | (_, Sync.Avatar (Just (Sync.BlobId blob))) <- Sync.allFacts (Sync.accountFacts (Linked.accountState l))]

authored :: Sync.GraphId -> UTCTime -> Sync.Content -> Linked -> (Linked, [BS.ByteString])
authored g time content l = (l', packets)
  where (_, packets, l') = Linked.author g time content l

linkedHomeAddress :: Linked -> Text
linkedHomeAddress = Linked.linkedHome

-- | Latest event time, for our new events (there's no clock here).
linkedClock :: Linked -> UTCTime
linkedClock l = maximum (Linked.linkedSince l : [Linked.eventTime e | g <- Sync.storeGraphs store, (_, e) <- take 1 (reverse (Sync.byRank g store))])
  where store = Replica.replicaStore (Linked.linkedReplica l)

linkedSender :: Linked -> Sync.GraphId -> Text
linkedSender l g = fromMaybe ourName (listToMaybe
  [n | c <- Sync.joinedChats state, Sync.chatGraph c == g, Sync.SelfName n <- Sync.factsOf (Sync.ChatSubject c) facts])
  where
    state = Linked.accountState l
    facts = Sync.accountFacts state
    ourName = fromMaybe "" (listToMaybe [n | Sync.Name n <- Sync.factsOf Sync.AccountSubject facts])

-- | A chat's events in display order, and what later events say of
-- earlier ones.
data Thread = Thread
  { threadGraph   :: Sync.GraphId
  , threadEvents  :: [(Sync.EventId, Sync.Event)]
  , threadSent    :: Set Sync.EventId
  , threadArrived :: Map Sync.EventId UTCTime
  , threadEnded   :: Map Sync.EventId (Maybe Word64, Bool)
  }

threadIn :: Sync.GraphId -> Sync.EventStore -> Thread
threadIn g store = Thread g events
  (Set.fromList [i | Sync.Sent i <- contents])
  (Map.fromList [(i, Linked.eventTime e) | (_, e) <- events, Sync.Delivered i <- [Sync.eventContent e]])
  (Map.fromList [(i, (d, declined)) | Sync.CallEnded i d declined <- contents])
  where
    events = Sync.displayOrder g store
    contents = map (Sync.eventContent . snd) events

-- | Chats, friends, invites and profile of a linked account, from its
-- events and the home's live facts.
linkedView :: Linked -> Client -> Client
linkedView l client = client
  { clientProfile = (clientProfile client)
      { profileNickname = fromMaybe "" (listToMaybe [n | Sync.Name n <- kept Sync.AccountSubject])
      , profileStatus = fromMaybe "" (listToMaybe [t | Sync.StatusMessage t <- kept Sync.AccountSubject])
      , profileAway = maybe Tox.Online (toEnum . fromEnum)
          (listToMaybe [st | Sync.Status st <- kept Sync.AccountSubject])
      , profileToxId = fromMaybe (Text.toUpper (Linked.linkedHome l))
          (listToMaybe [hexOf a | Sync.Address a <- kept Sync.AccountSubject]) }
  , clientAvatar = avatarOf Sync.AccountSubject
  -- Ring while it rings at the home, until a device answers.
  , clientCall = case clientCall client of
      Nothing -> listToMaybe
        [ (newCall (contactIdOf pk) Ringing (linkedClock l) video) { callPeerVideo = video }
        | (Sync.FriendSubject pk, Sync.CallValue (Sync.Ringing video)) <- Sync.allFacts live ]
      Just c | callPhase c == Ringing, not (ringing (callContact c)) -> Nothing
      other -> other
  , clientContacts = Map.fromList
      -- Leave out the account's devices (they're the home's friends too). Leave
      -- us out even before the home lists us.
      [ (contactIdOf pk, contact pk) | (pk, f) <- Map.toList (Sync.accountFriends state), Sync.friendWanted f
      , pk /= Replica.replicaDevice (Linked.linkedReplica l), Map.notMember pk (Sync.accountDevices state) ]
  , clientRequests = Map.fromList
      [ (contactIdOf pk, asked) | (pk, Sync.Friend False (Just asked)) <- Map.toList (Sync.accountFriends state) ]
  , clientGroups = Map.fromList [(chatGroupId c, group c) | c <- Sync.joinedChats state]
  , clientInvites = Map.fromList
      [ ((contactIdOf (Sync.inviteFrom v), GroupId (hexOf i)), invite v)
      | (Sync.EventId i, v) <- Map.toList (Sync.accountInvites state)
      , Sync.inviteStatus v `elem` [Sync.Pending, Sync.Joining] ]
  }
  where
    state = Linked.accountState l
    facts = Sync.accountFacts state
    live = Linked.liveFacts l
    store = Replica.replicaStore (Linked.linkedReplica l)
    account = Linked.accountKey l
    kept s = Sync.factsOf s facts
    avatarOf s = listToMaybe [blob | Sync.Avatar (Just (Sync.BlobId blob)) <- kept s]
    ringing (ContactId key) = or
      [ True | Just pk <- [keyOf key], Sync.CallValue (Sync.Ringing _) <- Sync.factsOf (Sync.FriendSubject pk) live ]
    idOf (Sync.EventId i) = MessageId i
    memberKey' (Sync.PublicKey pk) = hexOf pk
    toxKind Sync.Action = Tox.MessageAction
    toxKind Sync.Normal = Tox.MessageNormal
    ends = linkedEnds l
    answers = linkedAnswers l
    delivery thread i
      | Map.member i (threadArrived thread) = Delivered
      | Set.member i (threadSent thread) = Sent
      | otherwise = Pending
    -- Theirs after the last read and since the link (earlier ones were for
    -- the other devices to notify). Only missed calls count.
    unread thread theirs = length
      [ () | (i, e) <- threadEvents thread
      , unreadIn state (threadGraph thread) e
      , Linked.eventTime e >= Linked.linkedSince l
      , theirs (i, e) ]
    theirMessage thread = maybe False (\m -> not (messageMine m) && maybe True recordMissed (messageCall m)) . message thread
    theirEntry (_, e) = case Sync.eventContent e of
      Sync.Received {}          -> True
      Sync.ReceivedPrivately {} -> True
      _                         -> False
    muteIn g = case Map.lookup muteKind =<< Map.lookup (Sync.ConversationScope g) (Sync.accountSettings state) of
      Just (Sync.Kept (Sync.Mute until' mentioned) _) ->
        Just (maybe MutedAlways (MutedUntil . Linked.fromMillis . fromIntegral) until', mentioned)
      _ -> Nothing
    picturesIn g = case Sync.keptValue <$> (Map.lookup picturesKind =<< Map.lookup (Sync.ConversationScope g) (Sync.accountSettings state)) of
      Just (Sync.PictureFormat way) -> picturesOf way
      _                             -> Nothing
    contact pk = Contact
      { contactName = listToMaybe [n | Sync.Name n <- kept subject]
      , contactOnline = or [c /= Sync.NoConnection | Sync.ConnectionValue c <- Sync.factsOf subject live]
      , contactMessages = mapMaybe (message thread) (threadEvents thread)
      , contactUnread = unread thread (theirMessage thread)
      , contactRequest = Nothing
      , contactStatusMessage = fromMaybe "" (listToMaybe [t | Sync.StatusMessage t <- kept subject])
      , contactStatus = fromMaybe Tox.Online (listToMaybe [toEnum (fromEnum st) | Sync.Status st <- Sync.factsOf subject live])
      , contactMute = fst <$> muteIn g
      , contactAvatar = avatarOf subject
      , contactPictures = picturesIn g
      , contactEarlier = False
      }
      where
        subject = Sync.FriendSubject pk
        g = Sync.friendGraph account pk
        thread = threadIn g store
    message thread (i, e) = case Sync.eventContent e of
      Sync.Received _ _ bytes kind ->
        Just (Message False (Text.decodeUtf8Lenient bytes) t Delivered Nothing Nothing 0 Nothing (toxKind kind) (Just bytes) (idOf i))
      Sync.Send _ bytes kind ->
        Just (Message True (Text.decodeUtf8Lenient bytes) t (delivery thread i) Nothing Nothing 0 (Map.lookup i (threadArrived thread)) (toxKind kind) Nothing (idOf i))
      Sync.CallStarted _ incoming video ->
        let (talked, declined) = case Map.lookup i (threadEnded thread) of
              Just (d, no) -> (fromIntegral <$> d, no)
              Nothing      -> (Nothing, False)
            missed = incoming && Map.member i (threadEnded thread) && isNothing talked && not declined
        in Just (Message (not incoming) "" t Delivered Nothing
                   (Just (CallRecord video ((/ 1000) <$> talked) missed)) 0 Nothing Tox.MessageNormal Nothing (idOf i))
      Sync.File _ _ name size incoming blob ->
        let end = Map.lookup i ends
            held = case end of
              Just (_, b) -> b <|> blob
              Nothing     -> blob
            here = (`Map.lookup` Linked.linkedHave l) =<< held
            path = fromMaybe "" here
            progress = case end of
              Just (False, _) -> Cancelled
              Just (True, _) | isJust here -> Complete
              Just (True, Just b) | Set.member b (Linked.linkedMissing l) -> Cancelled
                                  | Set.member b (Linked.linkedWanted l) -> Moving
              Just (True, _) -> if incoming then Waiting else Complete
              Nothing -> case Map.lookup i answers of
                Just True  -> Moving
                Just False -> Cancelled
                Nothing    -> if incoming then Waiting else Moving
            done = if progress == Complete then size else 0
        in Just (Message (not incoming) name t (if isJust end then Delivered else Pending)
                   (Just (Attachment size path done progress Nothing)) Nothing 0 Nothing Tox.MessageNormal Nothing (idOf i))
      _ -> Nothing
      where t = Linked.eventTime e
    group c = (newGroup name (if isGroup then NgcGroup else TextConference) InGroup)
      { groupEntries = mapMaybe (entry thread peerName) (threadEvents thread)
      , groupUnread = unread thread theirEntry
      , groupMembers = Map.fromList [(n, Member (memberKey' pk) (peerName pk)) | (n, pk) <- peers]
      , groupMute = fst <$> muteIn g
      , groupMentions = maybe True snd (muteIn g)
      , groupPictures = picturesIn g
      , groupTopic = fromMaybe "" (listToMaybe [t | Sync.Topic t <- kept subject])
      -- As Tox allows: anyone but observers, or only moderators while locked.
      , groupMayTopic = selfRole /= Just Tox.Observer && (not locked || selfRole `elem` [Just Tox.Founder, Just Tox.Moderator])
      , groupRoles = Map.fromList [(n, role) | (n, pk) <- peers, Sync.RoleValue r <- peerFacts pk, let role = toxRole r]
      , groupStatuses = Map.fromList
          [(n, status) | (n, pk) <- peers, Sync.Status st <- peerFacts pk, let status = toEnum (fromEnum st), status /= Tox.Online]
      , groupSelfRole = selfRole
      , groupSettings = Tox.GroupSettings
          <$> listToMaybe [p == Sync.Private | Sync.PrivacyValue p <- kept subject]
          <*> Just (Text.encodeUtf8 (fromMaybe "" (listToMaybe [p | Sync.Password p <- kept subject])))
          <*> listToMaybe [fromIntegral n | Sync.PeerLimit n <- kept subject]
          <*> Just locked
          <*> listToMaybe [toxVoice v | Sync.VoiceStateValue v <- kept subject]
      , groupRelayBot = memberKey' <$> Map.lookup c (Sync.accountRelayBots state)
      }
      where
        g = Sync.chatGraph c
        thread = threadIn g store
        subject = Sync.ChatSubject c
        isGroup = case c of
          Sync.GroupChat _ -> True
          _                -> False
        name = fromMaybe "" (listToMaybe [n | Sync.Name n <- kept subject])
        locked = or [b | Sync.TopicLock b <- kept subject]
        selfRole = listToMaybe [toxRole r | Sync.RoleValue r <- kept subject]
        peerFacts pk = Sync.factsOf (Sync.PeerSubject c pk) live
        -- A member who left has no facts, so use the name of their last
        -- message.
        peerName pk = fromMaybe "" (listToMaybe [n | Sync.Name n <- peerFacts pk] <|> Map.lookup pk wroteAs)
        wroteAs = Map.fromList $ flip mapMaybe (threadEvents thread) $ \(_, e) -> case Sync.eventContent e of
          Sync.Received (Just pk) said _ _   -> Just (pk, said)
          Sync.ReceivedPrivately pk said _ _ -> Just (pk, said)
          _                                  -> Nothing
        -- Our own numbers (Tox's are on the home).
        peers = zip [0 ..] [pk | (Sync.PeerSubject c' pk, Sync.Present True) <- Sync.allFacts live, c' == c]
    entry thread nameOf (i, e) = (\what -> GroupEntry (Linked.eventTime e) what (idOf i)) <$> case Sync.eventContent e of
      Sync.Received from sender bytes kind ->
        Just (Said (Just (Member (maybe "" memberKey' from) sender)) (toxKind kind) (Text.decodeUtf8Lenient bytes) Delivered Nothing)
      Sync.Send _ bytes kind -> Just (Said Nothing (toxKind kind) (Text.decodeUtf8Lenient bytes) (delivery thread i) Nothing)
      Sync.ReceivedPrivately from sender bytes kind ->
        Just (PrivateFrom (Member (memberKey' from) sender) (toxKind kind) (Text.decodeUtf8Lenient bytes))
      Sync.SendPrivately to _ bytes kind ->
        Just (PrivateTo (Member (memberKey' to) (nameOf to)) (toxKind kind) (Text.decodeUtf8Lenient bytes) (delivery thread i))
      Sync.Notice t -> Just (Noted t)
      Sync.PeerJoined pk name -> Just (Arrived (Member (memberKey' pk) name))
      Sync.PeerLeft pk name -> Just (Departed (Member (memberKey' pk) name))
      Sync.PeerRenamed pk old name -> Just (Noted (memberLabel (Member (memberKey' pk) old) <> " is now known as " <> name))
      Sync.TopicSet by name topic -> Just (TopicSet (Just (someone by name)) topic)
      Sync.TitleSet by name title -> Just (Noted (memberLabel (someone by name) <> " changed the title to " <> title))
      Sync.Moderated by name peer peerName role ->
        Moderated (Just (someone by name)) (Just (someone peer peerName)) <$> maybe (Just Tox.ModKick) moderation role
      _ -> Nothing
    someone = Member . maybe "" memberKey'
    moderation = \case
      Sync.Moderator -> Just Tox.ModModerator
      Sync.User      -> Just Tox.ModUser
      Sync.Observer  -> Just Tox.ModObserver
      Sync.Founder   -> Nothing
    toxRole = \case
      Sync.Founder   -> Tox.Founder
      Sync.Moderator -> Tox.Moderator
      Sync.User      -> Tox.User
      Sync.Observer  -> Tox.Observer
    toxVoice = \case
      Sync.VoiceEveryone   -> Tox.VoiceAll
      Sync.VoiceModerators -> Tox.VoiceModerators
      Sync.VoiceFounder    -> Tox.VoiceFounder
    invite v = Invite
      { inviteKind = if Sync.inviteKind v == Sync.GroupKind then NgcGroup else TextConference
      , inviteName = Sync.inviteName v
      , inviteData = Sync.inviteData v
      , inviteAnswered = if Sync.inviteStatus v == Sync.Joining then Just (fromMaybe "" (Sync.invitePassword v)) else Nothing
      }

-- | A page from the store in front, without any we have already.
inFront :: Ord k => (a -> k) -> [a] -> [a] -> [a]
inFront key page have = filter ((`Set.notMember` Set.fromList (map key have)) . key) page ++ have

-- | The newest, from the oldest still in use if that's earlier.
newest :: Int -> (a -> Bool) -> [a] -> [a]
newest n used xs = drop (minimum (length xs - n : [i | (i, x) <- zip [0 ..] xs, used x])) xs

-- | Gives the member's name to their entries that have none, as from
-- history before they joined.
named :: Member -> Group -> Group
named m g
  | Text.null (memberName m) = g
  | otherwise = g { groupEntries = [e { entryWhat = withName (entryWhat e) } | e <- groupEntries g] }
  where
    withName = \case
      Said who kind text d mid    -> Said (fill <$> who) kind text d mid
      PrivateFrom who kind text   -> PrivateFrom (fill who) kind text
      PrivateTo who kind text d   -> PrivateTo (fill who) kind text d
      Moderated by whom how       -> Moderated (fill <$> by) (fill <$> whom) how
      Arrived who                 -> Arrived (fill who)
      Departed who                -> Departed (fill who)
      TopicSet who topic          -> TopicSet (fill <$> who) topic
      SharedFile who file mid     -> SharedFile (fill <$> who) file mid
      Noted t                     -> Noted t
    fill who
      | memberKey who == memberKey m, Text.null (memberName who) = who { memberName = memberName m }
      | otherwise = who

entryMembers :: Happening -> [Member]
entryMembers = \case
  Said who _ _ _ _     -> maybe [] pure who
  PrivateFrom who _ _  -> [who]
  PrivateTo who _ _ _  -> [who]
  Moderated by whom _  -> maybe [] pure by <> maybe [] pure whom
  Arrived who          -> [who]
  Departed who         -> [who]
  TopicSet who _       -> maybe [] pure who
  SharedFile who _ _   -> maybe [] pure who
  Noted _              -> []

-- | Nothing if they've left.
memberByKey :: Text -> Group -> Maybe (Tox.PeerId, Member)
memberByKey key g = find ((== key) . memberKey . snd) (Map.toList (groupMembers g))

adjustMessage :: ContactId -> MessageId -> (Message -> Message) -> Client -> Client
adjustMessage cid i f client = client { clientContacts = Map.adjust contact cid (clientContacts client) }
  where
    contact c = c { contactMessages = [ if messageId m == i then f m else m | m <- contactMessages c ] }

adjustFile :: ContactId -> MessageId -> (Attachment -> Attachment) -> Client -> Client
adjustFile cid i f = adjustMessage cid i (\m -> m { messageFile = f <$> messageFile m })

-- | A transfer that Tox ended early. Both ours and theirs wait to be offered
-- again and resume, but only if they have a file ID.
interrupted :: Message -> Message
interrupted m = m { messageFile = cut <$> messageFile m }
  where
    cut a
      | attachmentState a `notElem` [Waiting, Moving] = a
      | messageMine m && (attachmentState a == Waiting || resumable) = a { attachmentState = Waiting }
      | resumable && attachmentState a == Moving = a { attachmentState = Interrupted }
      | resumable = a
      | otherwise = a { attachmentState = Cancelled }
      where resumable = isJust (attachmentId a)

pruned :: Client -> Client
pruned client = client
  { clientAvatars = Map.restrictKeys (clientAvatars client) $ Set.fromList $
      maybe id (:) (clientAvatar client)
        [ h | c <- Map.elems (clientContacts client) ++ Map.elems (clientFormer client), Just h <- [contactAvatar c] ] }

