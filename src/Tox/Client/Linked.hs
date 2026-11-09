{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | A profile whose account runs on another device (its home). We keep a
-- replica in sync over Tox. Screens are derived from its events, and what
-- the user does becomes events.
module Tox.Client.Linked
  ( Linked (..)
  , newLinked
  , homeKey
  , accountKey
  , accountState
  , liveFacts
  , inSession
  , homeOnline
  , homeOffline
  , receivePacket
  , linkedHolds
  , wantBlob
  , lookForBlob
  , blobMissing
  , blobHere
  , noBlob
  , callAction
  , author
  , eventTime
  , fromMillis
  , toMillis
  ) where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.Either            (fromRight)
import           Data.Int               (Int64)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Data.Time              (UTCTime)
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime,
                                         utcTimeToPOSIXSeconds)

import           Tox.Sync.Account       (AccountState, accountOf)
import qualified Tox.Sync.Event         as Event
import           Tox.Sync.Event         (Content, Event)
import           Tox.Sync.Fact          (Facts)
import qualified Tox.Sync.Frame         as Frame
import           Tox.Sync.Id
import           Tox.Sync.Protocol      (CallAction, SessionMessage (..),
                                         decodeMessage, encodeMessage, hello)
import qualified Tox.Sync.Replica       as Replica
import           Tox.Sync.Store         (EventStore, allHeads, eventBytes,
                                         storedIds)

data Linked = Linked
  { linkedHome     :: Text
    -- ^ Home's Tox address in hex.
  , linkedPairing  :: Maybe Text
    -- ^ Pairing friend request, until the home answers.
  , linkedSince    :: UTCTime
    -- ^ Messages from before the link aren't new here.
  , linkedReplica  :: Replica.Replica
  , linkedAccount  :: AccountState
  , linkedIncoming :: Frame.Reassembler
  , linkedWanted   :: Set BlobId
    -- ^ Asked of the home in this session.
  , linkedMissing  :: Set BlobId
    -- ^ The home doesn't have them, or sent the wrong bytes (this session).
  , linkedLooked   :: Set BlobId
    -- ^ Looked for here since the app started.
  , linkedHave     :: Map BlobId FilePath
  , linkedTraffic  :: Map Text Int
    -- ^ Sync messages by direction and kind since the app started, to see a
    -- loop.
  }

-- | Compares heads, not events (events never change, so same heads means
-- same events).
instance Eq Linked where
  a == b = linkedHome a == linkedHome b && linkedPairing a == linkedPairing b && linkedSince a == linkedSince b
    && inSession a == inSession b && liveFacts a == liveFacts b && heads a == heads b
    && linkedWanted a == linkedWanted b && linkedMissing a == linkedMissing b && linkedLooked a == linkedLooked b && linkedHave a == linkedHave b
    where heads = allHeads . Replica.replicaStore . linkedReplica

instance Show Linked where
  show l = "Linked " <> show (linkedHome l)

newLinked :: Text -> Maybe Text -> UTCTime -> PublicKey -> EventStore -> Linked
newLinked home pairing since device store = Linked
  { linkedHome = home
  , linkedPairing = pairing
  , linkedSince = since
  , linkedReplica = replica
  , linkedAccount = accountOf (Replica.replicaAccount replica) store
  , linkedIncoming = Frame.reassembler
  , linkedWanted = Set.empty
  , linkedMissing = Set.empty
  , linkedLooked = Set.empty
  , linkedHave = Map.empty
  , linkedTraffic = Map.empty
  }
  where replica = Replica.newReplica device (homeKeyOf home) store

homeKeyOf :: Text -> PublicKey
homeKeyOf = PublicKey . fromRight BS.empty . Base16.decode . Text.encodeUtf8 . Text.take 64

-- | The home's key, which is also the account's (the home runs it on Tox).
homeKey :: Linked -> PublicKey
homeKey = homeKeyOf . linkedHome

accountKey :: Linked -> PublicKey
accountKey = homeKey

accountState :: Linked -> AccountState
accountState = linkedAccount

-- | What the home says is live now, while in session.
liveFacts :: Linked -> Facts
liveFacts = Replica.replicaLive . linkedReplica

inSession :: Linked -> Bool
inSession = Replica.replicaSession . linkedReplica

homeOnline :: Linked -> [BS.ByteString]
homeOnline _ = Frame.frames (encodeMessage hello)

homeOffline :: Linked -> Linked
homeOffline l = l { linkedReplica = Replica.offline (linkedReplica l), linkedIncoming = Frame.reassembler, linkedWanted = Set.empty
                  , linkedMissing = Set.empty }

-- | Gives the packets to answer with, and the blobs the home asks for.
receivePacket :: BS.ByteString -> Linked -> ([BS.ByteString], [BlobId], Linked)
receivePacket packet l = case Frame.push packet (linkedIncoming l) of
  Left (_, incoming) -> ([], [], l { linkedIncoming = incoming })
  Right (Nothing, incoming) -> ([], [], l { linkedIncoming = incoming })
  Right (Just bytes, incoming) -> case decodeMessage bytes of
    Right (Just (WantBlob blob)) -> ([], [blob], l { linkedIncoming = incoming })
    Right (Just (NoBlob blob)) -> ([], [], blobMissing blob l { linkedIncoming = incoming })
    Right (Just message) ->
      let (replies, replica) = Replica.received message (linkedReplica l)
          traffic = foldr (counted "out") (counted "in" message (linkedTraffic l)) replies
          l' = l { linkedIncoming = incoming, linkedReplica = replica, linkedTraffic = traffic }
          -- The home has paired us once its session starts.
          paired = case message of
            Hello {} -> l' { linkedPairing = Nothing }
            _        -> l'
      in (concatMap (Frame.frames . encodeMessage) replies, [], refreshed paired)
    _ -> ([], [], l { linkedIncoming = incoming })

-- | Its kind, without showing the rest (derived 'Show' is lazy).
counted :: Text -> SessionMessage -> Map Text Int -> Map Text Int
counted direction message = Map.insertWith (+) (direction <> " " <> Text.pack (takeWhile (/= ' ') (show message))) 1

-- | What it holds and has sent, for the debug screen.
linkedHolds :: Linked -> [(Text, Int)]
linkedHolds l =
  [ ("sync events", Set.size (storedIds store))
  , ("sync event bytes", sum [ maybe 0 BS.length (eventBytes i store) | i <- Set.toList (storedIds store) ])
  , ("sync blobs wanted", Set.size (linkedWanted l))
  , ("sync blobs missing", Set.size (linkedMissing l))
  , ("sync blobs looked for", Set.size (linkedLooked l))
  , ("sync blobs here", Map.size (linkedHave l))
  ]
  ++ [ ("sync " <> kind, n) | (kind, n) <- Map.toList (linkedTraffic l) ]
  where store = Replica.replicaStore (linkedReplica l)

blobMissing :: BlobId -> Linked -> Linked
blobMissing blob l = l { linkedMissing = Set.insert blob (linkedMissing l) }

wantBlob :: BlobId -> Linked -> ([BS.ByteString], Linked)
wantBlob blob l = (Frame.frames (encodeMessage (WantBlob blob)), l { linkedWanted = Set.insert blob (linkedWanted l) })

lookForBlob :: BlobId -> Linked -> Linked
lookForBlob blob l = l { linkedLooked = Set.insert blob (linkedLooked l) }

blobHere :: BlobId -> FilePath -> Linked -> Linked
blobHere blob path l = l { linkedHave = Map.insert blob path (linkedHave l) }

callAction :: PublicKey -> CallAction -> [BS.ByteString]
callAction friend = Frame.frames . encodeMessage . Call friend

noBlob :: BlobId -> [BS.ByteString]
noBlob = Frame.frames . encodeMessage . NoBlob

-- | A new event by us, with the packets for the home.
author :: GraphId -> UTCTime -> Content -> Linked -> (EventId, [BS.ByteString], Linked)
author graph time content l = (i, concatMap (Frame.frames . encodeMessage) pushed, refreshed l { linkedReplica = replica })
  where (i, _, pushed, replica) = Replica.author graph (toMillis time) content (linkedReplica l)

refreshed :: Linked -> Linked
refreshed l = l { linkedAccount = accountOf (Replica.replicaAccount replica) (Replica.replicaStore replica) }
  where replica = linkedReplica l

eventTime :: Event -> UTCTime
eventTime = fromMillis . Event.eventTime

fromMillis :: Int64 -> UTCTime
fromMillis ms = posixSecondsToUTCTime (fromIntegral ms / 1000)

toMillis :: UTCTime -> Int64
toMillis t = round (utcTimeToPOSIXSeconds t * 1000)
