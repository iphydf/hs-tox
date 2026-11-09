{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | A profile whose Tox runs here, as the home of the account's other devices.
-- What happens here becomes events, which the devices sync. What they write
-- becomes things for Tox to do.
module Tox.Client.Home
  ( Home (..)
  , Session (..)
  , Taken (..)
  , pairingTime
  , newHome
  , marked
  , devices
  , isDevice
  , inSession
  , write
  , pushed
  , hello
  , sessionStarted
  , sessionEnded
  , receivePacket
  , liveChanged
  , pairWith
  , paired
  , homeHolds
  , askBlob
  ) where

import qualified Data.ByteString   as BS
import           Data.List         (sortOn)
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Data.Text         (Text)
import           Data.Time         (NominalDiffTime, UTCTime)
import           Data.Word         (Word64)

import           Tox.Sync.Account  (AccountState (..), Friend (..), accountOf,
                                    applyEvent, isOpen, joinedChats)
import           Tox.Sync.Event    (Content (..), Event (..), contentTag,
                                    decodeEvent)
import           Tox.Sync.Fact     (Facts, Subject (..), Value, allFacts,
                                    isWishable, noFacts)
import qualified Tox.Sync.Frame    as Frame
import           Tox.Sync.Id
import qualified Tox.Sync.Protocol as Protocol
import           Tox.Sync.Protocol (CallAction, SessionMessage (..),
                                    decodeMessage, encodeMessage, syncsAccounts)
import qualified Tox.Sync.Session  as Session
import           Tox.Sync.Setting  (Scope (..))
import           Tox.Sync.Store    (EventStore, allHeads, create, eventBytes,
                                    headsOf, insert, lookupEvent, member,
                                    storedIds)

import           Tox.Client.Linked (fromMillis, toMillis)
import           Tox.Client.Types  (GroupId)

data Session = Session
  { sessionIncoming :: Frame.Reassembler
  , sessionOn       :: Bool
    -- ^ After the device's 'Hello'.
  , sessionWaiting  :: [BS.ByteString]
    -- ^ Its events whose parents haven't come yet.
  }

data Home = Home
  { homeAccount  :: PublicKey
  , homeStore    :: EventStore
  , homeState    :: AccountState
  , homeSession  :: Map PublicKey Session
    -- ^ Devices online now.
  , homePairing  :: Maybe Text
    -- ^ Code a new device may pair with.
  , homeMarks    :: Set (Word64, EventId)
    -- ^ Events that answer others, by their tag and the event they answer.
  , homeLatest   :: Map GraphId (Word64, EventId)
    -- ^ Rank and ID of the newest message to us in each conversation.
  , homeLive     :: Facts
    -- ^ As the devices in session have them.
  , homeEarlier  :: Map BS.ByteString EventId
    -- ^ Events of messages from before we were the home, by message ID
    -- (later ones have their event's ID).
  , homeHolding  :: Set EventId
    -- ^ Files being stored as blobs, to end them with.
  , homeFetching :: Map BlobId [EventId]
    -- ^ Devices' files to send once their blobs are here.
  , homeAvatar   :: Maybe BlobId
    -- ^ Avatar a device chose, until its blob is here.
  , homeInvited  :: Map EventId GroupId
    -- ^ Group of each invite, until we're in it.
  , homeFounding :: [EventId]
    -- ^ Devices' new chats, in order, until Tox made them.
  , homeJoining  :: [(EventId, BS.ByteString)]
    -- ^ Public groups devices asked to join, by chat ID, until answered.
  , homeBackfill :: Bool
    -- ^ Until the messages we had when we became the home are events.
  , homeFull     :: Bool
    -- ^ Until we've looked at all chats once (others only when they change).
  , homeClock    :: UTCTime
    -- ^ Latest time we know, for events that happen at no time of their own.
  }

-- | Compares heads, not events (events never change, so same heads means
-- same events).
instance Eq Home where
  a == b = homeAccount a == homeAccount b && heads a == heads b
    && Map.map sessionOn (homeSession a) == Map.map sessionOn (homeSession b)
    && homePairing a == homePairing b && homeMarks a == homeMarks b
    && homeLatest a == homeLatest b && homeLive a == homeLive b
    && homeEarlier a == homeEarlier b && homeHolding a == homeHolding b
    && homeFetching a == homeFetching b && homeAvatar a == homeAvatar b
    && homeInvited a == homeInvited b && homeFounding a == homeFounding b && homeJoining a == homeJoining b
    && homeBackfill a == homeBackfill b && homeFull a == homeFull b && homeClock a == homeClock b
    where heads = allHeads . homeStore

instance Show Home where
  show h = "Home " <> show (homeAccount h)

-- | How long a pairing code works.
pairingTime :: NominalDiffTime
pairingTime = 10 * 60

-- | Takes the events stored so far. A new home writes its messages first.
newHome :: PublicKey -> EventStore -> Home
newHome account store = Home
  { homeAccount = account
  , homeStore = store
  , homeState = accountOf (accountGraph account) store
  , homeSession = Map.empty
  , homePairing = Nothing
  , homeMarks = Set.fromList [m | i <- Set.toList (storedIds store), Just e <- [lookupEvent i store], Just m <- [markOf (eventContent e)]]
  , homeLatest = foldl (flip latest) Map.empty [(i, e) | i <- Set.toList (storedIds store), Just e <- [lookupEvent i store]]
  , homeLive = noFacts
  , homeEarlier = Map.empty
  , homeHolding = Set.empty
  , homeFetching = Map.empty
  , homeAvatar = Nothing
  , homeInvited = Map.empty
  , homeFounding = []
  , homeJoining = []
  , homeBackfill = null (storedIds store)
  , homeFull = True
  , homeClock = maximum (fromMillis 0 : [fromMillis (eventTime e) | i <- Set.toList (storedIds store), Just e <- [lookupEvent i store]])
  }

devices :: Home -> Map PublicKey Text
devices = accountDevices . homeState

isDevice :: PublicKey -> Home -> Bool
isDevice pk = Map.member pk . devices

inSession :: Home -> [PublicKey]
inSession = Map.keys . Map.filter sessionOn . homeSession

-- | A new event by us, after everything we have of its graph.
write :: GraphId -> UTCTime -> Content -> Home -> (EventId, BS.ByteString, Home)
write g time c h = (i, bytes, added [(i, g, c)] h { homeStore = store })
  where (i, bytes, store) = create g (homeAccount h) (toMillis time) c (homeStore h)

-- | What an event answers: our message sent or delivered, a file or call
-- ended.
markOf :: Content -> Maybe (Word64, EventId)
markOf c = case c of
  Sent i          -> Just (contentTag c, i)
  Delivered i     -> Just (contentTag c, i)
  FileEnded i _ _ -> Just (contentTag c, i)
  CallEnded i _ _ -> Just (contentTag c, i)
  _               -> Nothing

-- | Keeps the newest message to us in its conversation.
latest :: (EventId, Event) -> Map GraphId (Word64, EventId) -> Map GraphId (Word64, EventId)
latest (i, e) = case eventContent e of
  Received {}          -> Map.insertWith max (eventGraph e) (eventRank e, i)
  ReceivedPrivately {} -> Map.insertWith max (eventGraph e) (eventRank e, i)
  _                    -> id

-- | Whether an event of this kind answers the event.
marked :: Content -> Home -> Bool
marked c h = maybe False (`Set.member` homeMarks h) (markOf c)

-- | Account events of the highest rank apply on top. Others (a device's
-- concurrent ones) need the account folded again.
added :: [(EventId, GraphId, Content)] -> Home -> Home
added new h0
  | null mine = h
  | all (top . (\(i, _, _) -> i)) mine = h { homeState = foldl (\s (i, _, c) -> applyEvent i c s) (homeState h) mine }
  | otherwise = h { homeState = accountOf account (homeStore h) }
  where
    h = h0 { homeMarks = foldr Set.insert (homeMarks h0) [m | (_, _, c) <- new, Just m <- [markOf c]]
           , homeLatest = foldl (flip latest) (homeLatest h0) [(i, e) | (i, _, _) <- new, Just e <- [lookupEvent i (homeStore h0)]] }
    account = accountGraph (homeAccount h)
    mine = [e | e@(_, g, _) <- new, g == account]
    top i = i `elem` headsOf account (homeStore h)

-- | Packets for each device in session with new events, but not for their
-- authors.
pushed :: [(EventId, BS.ByteString)] -> Home -> [(PublicKey, [BS.ByteString])]
pushed new h =
  [ (pk, concatMap (Frame.frames . encodeMessage) (Session.batches mine))
  | pk <- inSession h
  , let mine = [bytes | (i, bytes) <- new, maybe True ((/= pk) . eventAuthor) (lookupEvent i (homeStore h))]
  , not (null mine) ]

askBlob :: BlobId -> [BS.ByteString]
askBlob = Frame.frames . encodeMessage . WantBlob

hello :: [BS.ByteString]
hello = Frame.frames (encodeMessage Protocol.hello)

sessionStarted :: PublicKey -> Home -> Home
sessionStarted pk h = h { homeSession = Map.insert pk (Session Frame.reassembler False []) (homeSession h) }

sessionEnded :: PublicKey -> Home -> Home
sessionEnded pk h = h { homeSession = Map.delete pk (homeSession h) }

-- | What a device sent, once whole.
data Taken = Taken
  { takenReplies :: [BS.ByteString]
  , takenEvents  :: [(EventId, Event, BS.ByteString)]
    -- ^ Its events we took, lowest rank first.
  , takenWanted  :: [BlobId]
    -- ^ Blobs it asks for.
  , takenMissing :: [BlobId]
    -- ^ Blobs it doesn't have.
  , takenCalls   :: [(PublicKey, CallAction)]
    -- ^ What it does with a friend's call.
  }

-- | Only devices get a session. From others we take nothing.
receivePacket :: PublicKey -> BS.ByteString -> Facts -> Home -> (Taken, Home)
receivePacket pk packet live h = case Map.lookup pk (homeSession h) of
  Just s | isDevice pk h -> case Frame.push packet (sessionIncoming s) of
    Right (Just bytes, incoming) | Right (Just message) <- decodeMessage bytes ->
      received pk message live h { homeSession = Map.insert pk s { sessionIncoming = incoming } (homeSession h) }
    Right (_, incoming) -> (nothing, h { homeSession = Map.insert pk s { sessionIncoming = incoming } (homeSession h) })
    Left (_, incoming) -> (nothing, h { homeSession = Map.insert pk s { sessionIncoming = incoming } (homeSession h) })
  _ -> (nothing, h)
  where nothing = Taken [] [] [] [] []

received :: PublicKey -> SessionMessage -> Facts -> Home -> (Taken, Home)
received pk message live h = case message of
  -- A repeated one means the device started again before Tox noticed.
  Hello _ features ->
    let on = syncsAccounts features
        again = was sessionOn
    in ( Taken (packets ([Protocol.hello | on && again] ++ [Session.summary (homeStore h) | on] ++ [Facts (factsList live) True | on])) [] [] [] []
       , session (\s -> s { sessionOn = on }) h { homeLive = live } )
  _ | not (was sessionOn) -> (Taken [] [] [] [] [], h)
  Events events -> taking pk events h
  WantBlob blob -> (Taken [] [] [blob] [] [], h)
  NoBlob blob -> (Taken [] [] [] [blob] [], h)
  Facts _ _ -> (Taken [] [] [] [] [], h)
  Call friend action -> (Taken [] [] [] [] [(friend, action)], h)
  _ -> (Taken (packets (Session.answer (homeStore h) message)) [] [] [] [], h)
  where
    was f = maybe False f (Map.lookup pk (homeSession h))
    session f h' = h' { homeSession = Map.adjust f pk (homeSession h') }

packets :: [SessionMessage] -> [BS.ByteString]
packets = concatMap (Frame.frames . encodeMessage)

factsList :: Facts -> [(Subject, Value)]
factsList = allFacts

-- | Events the device wrote, of the kinds it may, each once its parents are
-- here. We ask for the missing parents.
taking :: PublicKey -> [BS.ByteString] -> Home -> (Taken, Home)
taking pk incoming h = (Taken (packets asked) taken [] [] [], h' { homeSession = Map.adjust (\s -> s { sessionWaiting = waiting }) pk (homeSession h') })
  where
    earlier = maybe [] sessionWaiting (Map.lookup pk (homeSession h))
    decoded = sortOn (\(_, e, _) -> eventRank e)
      [ (eventIdOf bytes, e, bytes) | bytes <- earlier ++ incoming, Right e <- [decodeEvent bytes]
      , eventAuthor e == pk, not (member (eventIdOf bytes) (homeStore h)) ]
    (taken, waiting, h') = go [] [] h decoded
    go took held home = \case
      [] -> (reverse took, [b | (_, _, b) <- held], home)
      (i, e, bytes) : rest
        | not (allowed pk e home) -> go took held home rest
        | all (`member` homeStore home) (eventParents e) && (not (null (eventParents e)) || eventRank e == 0)
        , Right (_, store) <- insert bytes (homeStore home) ->
            -- Taking it may let ones held back in.
            let home' = added [(i, eventGraph e, eventContent e)] home { homeStore = store }
            in go ((i, e, bytes) : took) [] home' (sortOn (\(_, x, _) -> eventRank x) (reverse held ++ rest))
        | otherwise -> go took ((i, e, bytes) : held) home rest
    asked = Session.want (Set.toList (Set.fromList
      [ p | (_, e, _) <- [d | d@(i, _, _) <- decoded, i `notElem` [t | (t, _, _) <- taken]]
      , p <- eventParents e, not (member p (homeStore h')) ]))

-- | The kinds of events a device may write, and where.
allowed :: PublicKey -> Event -> Home -> Bool
allowed pk e h
  | eventGraph e == account = case eventContent e of
      ReadUpTo {} -> True
      MergeChat {} -> True
      RelayBot {} -> True
      SettingContent scope _ -> ours scope
      ClearSetting scope _ -> ours scope
      WantFriend friend _ _ -> notDevice friend
      UnwantFriend friend -> notDevice friend
      AnswerInvite invite _ _ -> maybe False isOpen (Map.lookup invite (accountInvites state))
      Wish s v -> isWishable s v
      Device {} -> True
      DeviceGone gone -> gone /= homeAccount h
      NewChat {} -> True
      JoinChat {} -> True
      InviteToChat c _ -> c `elem` joinedChats state
      _ -> False
  | Map.member (eventGraph e) friendChats = case eventContent e of
      Send {}                     -> True
      FileAnswered {}             -> True
      File _ _ _ _ False (Just _) -> True
      _                           -> False
  | eventGraph e `elem` map chatGraph (joinedChats state) = case eventContent e of
      Send {}          -> True
      SendPrivately {} -> True
      _                -> False
  | otherwise = False
  where
    state = homeState h
    account = accountGraph (homeAccount h)
    friendChats = Map.fromList
      [ (friendGraph (homeAccount h) friend, friend) | (friend, f) <- Map.toList (accountFriends state), friendWanted f ]
    notDevice friend = friend /= homeAccount h && not (isDevice friend h) && friend /= pk
    ours = \case
      AccountScope -> True
      ConversationScope g -> Map.member g friendChats || g `elem` map chatGraph (joinedChats state)
      UnknownScope _ -> False

-- | New live facts for the devices in session, if they changed.
liveChanged :: Facts -> Home -> Maybe ([(PublicKey, [BS.ByteString])], Home)
liveChanged live h
  | live == homeLive h = Nothing
  | otherwise = Just ([(pk, packets [Facts (factsList live) True]) | pk <- inSession h], h { homeLive = live })

-- | A new code, or none.
pairWith :: Maybe Text -> Home -> Home
pairWith code h = h { homePairing = code }

-- | Whether a friend request is from a new device with our code, which it
-- then uses up.
paired :: Text -> Home -> Maybe Home
paired message h = case homePairing h of
  Just code | message == "pair:" <> code -> Just h { homePairing = Nothing }
  _                                      -> Nothing

-- | For the debug screen.
homeHolds :: Home -> [(Text, Int)]
homeHolds h =
  [ ("home events", Set.size (storedIds store))
  , ("home event bytes", sum [maybe 0 BS.length (eventBytes i store) | i <- Set.toList (storedIds store)])
  , ("home devices", Map.size (devices h))
  , ("home sessions", length (inSession h))
  ]
  where store = homeStore h
