{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | Tox behind a client: turns its events into client events, and runs the
-- client's commands.
module Tox.Client.Network
  ( Network
  , start
  , identityOf
  , run
  , looping
  , stop
  , perform
  , pullAudio
  , dueVideo
  , sendAudio
  , sendVideo
  , measure
  , constructor
  ) where

import           Control.Concurrent        (Chan, MVar, ThreadId, forkIO,
                                            modifyMVar, modifyMVar_, newChan,
                                            newMVar, readChan, readMVar,
                                            threadDelay, writeChan)
import           Control.Exception         (IOException,
                                            SomeAsyncException (..),
                                            displayException, fromException,
                                            try)
import           Control.Monad             (forM, forM_, unless, void, when)
import qualified Crypto.Saltine.Class      as Sodium
import           Crypto.Saltine.Core.Utils (randomByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base16    as Base16
import           Data.Char                 (isDigit)
import           Data.IORef                (IORef, modifyIORef', newIORef,
                                            readIORef)
import           Data.List                 (find, sortOn)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (fromMaybe, isJust, isNothing,
                                            listToMaybe)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Time                 (getCurrentTime)
import           Data.Time.Clock.POSIX     (getPOSIXTime, posixSecondsToUTCTime,
                                            utcTimeToPOSIXSeconds)
import qualified Data.Vector.Storable      as V
import           Data.Word                 (Word32, Word64, Word8)
import           GHC.Clock                 (getMonotonicTime)
import           GHC.Conc                  (labelThread, myThreadId)
import           System.Directory          (removeFile)
import           System.FilePath           (takeDirectory, (</>))
import           System.IO                 (IOMode (..), SeekMode (..),
                                            hFileSize, hSeek, withBinaryFile)
import           System.Random             (initStdGen)
import qualified Tox.Application.Tox       as Tox
import qualified Tox.AV                    as AV
import           Tox.AV                    (BitRates (..), Pcm, Picture)
import qualified Tox.AV.MSI                as MSI
import qualified Tox.AV.Playout            as Playout
import           Tox.AV.Playout            (Playout)
import qualified Tox.Client.Avatars        as Avatars
import qualified Tox.Client.Blobs          as Blobs
import qualified Tox.Client.Debug          as Debug
import qualified Tox.Client.Files          as Files
import           Tox.Client.Messages       (parts)
import           Tox.Client.Nodes          (Node (..), bootstrapFrom, usable)
import qualified Tox.Client.Outgoing       as Outgoing
import           Tox.Client.Types
import           Tox.Client.Update         (Command (..), Event (..),
                                            Refusal (..))
import qualified Tox.Core.Lanes            as Lanes
import           Tox.Core.Lanes            (Lanes)
import           Tox.Core.Log              (LogLevel (..))
import           Tox.Crypto.Core.Key       (PublicKey)
import qualified Tox.Share                 as Share
import           Tox.Share                 (Share)
import qualified Tox.Share.History         as History
import qualified Tox.Share.Info            as Info
import qualified Tox.Share.Packet          as Packet
import qualified Tox.Share.Stats           as Stats
import qualified Tox.Sync.Frame            as Frame

data Network = Network
  { networkTox         :: Tox.Tox
  , networkOutgoing    :: Outgoing.Outgoing
  , networkFiles       :: Files.Transfers
  , networkAvatars     :: Avatars.Avatars
  , networkBlobs       :: Blobs.Blobs
  , networkReceived    :: FilePath
    -- ^ Directory for friends' files.
  , networkAV          :: AV.ToxAV PublicKey
  , networkPlayouts    :: MVar (Map PublicKey Playout)
  , networkRelays      :: MVar (Map PublicKey PublicKey)
    -- ^ The other end of each call we relay between a friend and one of the
    -- account's devices, by either end.
  , networkNodes       :: [Node]
  , networkProxied     :: Bool
    -- ^ With a proxy, we mustn't look up host names outside it.
  , networkGroups      :: IORef (Map Word32 GroupId)
    -- ^ Group numbers of this Tox run.
  , networkConferences :: IORef (Map Tox.ConferenceNumber GroupId)
  , networkSync        :: MVar (Map PublicKey [BS.ByteString])
    -- ^ Sync packets per friend until Tox takes them (it refuses them while
    -- its queue is full). Held while sending, so 2 threads never send a
    -- friend's packets out of order.
  , networkShares      :: MVar (Map Word32 (Share Word32))
    -- ^ Files and history shared in each group, by group number.
  , networkGroupFiles  :: IORef (Map BS.ByteString (Info.Info, Maybe FilePath, Word64))
    -- ^ Files shared in groups that we know the info of, where we download
    -- them to, and how much came.
  , networkHashing     :: Chan (GroupId, MessageId, FilePath, Text.Text)
    -- ^ Our files to share in groups, for a thread of their own, as hashing
    -- a big one takes a while.
  , networkLanes       :: Lanes
  , networkLog         :: Text.Text -> IO ()
    -- ^ For warnings and failures.
  }

-- | Kbit/s we send in calls.
audioRate, videoRate :: Word32
audioRate = 48
videoRate = 1000

-- | Microseconds between bootstraps while offline.
bootstrapInterval :: Int
bootstrapInterval = 30000000

shuffled :: [a] -> IO [a]
shuffled xs = do
  keys <- BS.unpack <$> randomByteString (4 * length xs)
  pure . map snd . sortOn fst $ zip (chunks keys) xs
  where
    chunks bytes = case splitAt 4 bytes of
      ([], _)   -> []
      (k, rest) -> k : chunks rest

-- | Nodes per bootstrap.
bootstrapCount :: Int
bootstrapCount = 8

-- | Start Tox with our identity, or a new one.
start :: (Text.Text -> IO ()) -> Lanes -> Maybe Identity -> DeviceSettings -> FilePath -> [Node] -> IO (Either Tox.NewError Network)
start logs lanes known device received known' = Tox.new options >>= \case
  Left err -> pure (Left err)
  Right tox -> do
    forM_ known (Tox.selfSetNospam tox . identityNospam)
    outgoing <- Outgoing.newOutgoing
    files <- Files.newTransfers
    avatars <- Avatars.newAvatars
    -- By hash, so all profiles share them.
    blobs <- Blobs.newBlobs (takeDirectory received </> "blobs")
    av <- AV.new AV.Transport
      { AV.sendLossless = Tox.friendQueueLosslessPacket tox
      , AV.sendLossy = Tox.friendQueueLossyPacket tox
      }
    playouts <- newMVar Map.empty
    relays <- newMVar Map.empty
    groups <- newIORef Map.empty
    conferences <- newIORef Map.empty
    sync <- newMVar Map.empty
    shares <- newMVar Map.empty
    groupFiles <- newIORef Map.empty
    hashing <- newChan
    pure (Right (Network tox outgoing files avatars blobs received av playouts relays known' (isJust (settingProxy device)) groups conferences sync shares groupFiles hashing lanes logs))
  where
    options = Tox.defaultOptions
      { Tox.optSaveData = maybe Tox.NoSaveData Tox.SecretKey
          (Sodium.decode . identitySecretKey =<< known)
      , Tox.optLogger = logged
      -- Only TCP goes through a proxy, so turn UDP off.
      , Tox.optUdpEnabled = settingUdp device && isNothing (settingProxy device)
      , Tox.optIpv6Enabled = settingIpv6 device
      , Tox.optLocalDiscovery = settingLan device && isNothing (settingProxy device)
      , Tox.optProxy = settingProxy device
      }
    logged level message = when (level >= LevelWarn) (logs message)

-- | Bootstrap from some nodes, in a different order each time, so a few
-- nodes that are down can't take all the places.
bootstrap :: Network -> IO ()
bootstrap network = do
  chosen <- take bootstrapCount . usable . filter reachable <$> shuffled (networkNodes network)
  mapM_ (bootstrapFrom (networkTox network)) chosen
  where
    -- With a proxy, only nodes given by IP address (Tox would look up names
    -- outside the proxy).
    reachable n = not (networkProxied network) || isAddress (nodeHost n)
    isAddress host = let octets = dotted host in
      length octets == 4 && all (\p -> not (null p) && length p <= 3 && all isDigit p && read p < (256 :: Int)) octets
    dotted s = case break (== '.') s of
      (part, _ : rest) -> part : dotted rest
      (part, [])       -> [part]

-- | The loops end with Tox.
stop :: Network -> IO ()
stop = Tox.kill . networkTox

identityOf :: Network -> IO Identity
identityOf network =
  Identity (Sodium.encode (Tox.selfSecretKey tox)) <$> Tox.selfGetNospam tox
  where tox = networkTox network

run :: Network -> (Event -> IO ()) -> IO [ThreadId]
run network report = do
  Tox.selfAddress tox >>= report . Started . hex
  sequence
    -- Keep going while offline. Tox drops nodes and relays it can't reach,
    -- and may have none left after an outage.
    [ looping network "Bootstrap" $ do
        status <- Tox.selfConnectionStatus tox
        when (status == Tox.NotConnected) $ bootstrap network
        threadDelay bootstrapInterval
    , looping network "Tox events" $ Tox.nextEvent tox >>= (\e -> timed ("Tox event " <> constructor e) (translate network e)) >>= mapM_ report
    , looping network "Call events" $ AV.nextEvent (networkAV network) >>= (\e -> timed ("Call event " <> constructor e) (called network e)) >>= mapM_ report
    , looping network "Sending" $
        threadDelay Files.retryInterval >> Files.flush tox (networkFiles network) >>= mapM_ (report . fromFiles)
          >> Avatars.flush tox (networkAvatars network)
          >> Blobs.flush tox (networkBlobs network)
          >> Outgoing.flush tox (networkOutgoing network) >>= mapM_ (report . fromOutgoing)
          >> flushSync network
    , looping network "Group files" $ do
        groups <- Map.keys <$> readMVar (networkShares network)
        mapM_ (\g -> withShare network g Share.tick >>= mapM_ report) groups
        busy <- any Share.busy <$> readMVar (networkShares network)
        threadDelay (if busy then 5000 else 1000000)
    , looping network "Group file hashing" $ readChan (networkHashing network) >>= offering network >>= mapM_ report
    ]
  where
    tox = networkTox network
    timed = Lanes.timed (networkLanes network)

-- | The most data a group's lossy custom packets carry, less the 4 bytes FT1
-- takes.
shareData :: Int
shareData = 1373 - 4

-- | Runs a step of a group's sharing, and what it says to do.
withShare :: Network -> Word32 -> (Double -> Share Word32 -> (Share Word32, [Share.Output Word32])) -> IO [Event]
withShare network g step = do
  t <- getMonotonicTime
  outs <- modifyMVar (networkShares network) $ \shares -> do
    s <- maybe (Share.new t shareData <$> initStdGen) pure (Map.lookup g shares)
    let (s', outs) = step t s
    pure (Map.insert g s' shares, outs)
  concat <$> mapM (fromShare network g) outs

fromShare :: Network -> Word32 -> Share.Output Word32 -> IO [Event]
fromShare network g o = Map.lookup g <$> readIORef (networkGroups network) >>= \case
  Nothing -> pure []
  Just gid -> case o of
    Share.Send n packet -> [] <$ Tox.groupSendCustomPrivatePacket tox g n (Packet.lossless packet) (Packet.encode packet)
    Share.SendAll packet -> [] <$ Tox.groupSendCustomPacket tox g True (Packet.encode packet)
    -- We get its info to show what it is, and the rest when asked.
    -- As a message, only from members who may speak.
    Share.Shared n mid fid -> Tox.groupPeerCanSpeak tox g n >>= \case
      False -> pure []
      True -> do
        now <- getCurrentTime
        (GroupFileShared gid now n mid fid :) <$> withShare network g (const (\s -> (Share.learn n fid s, [])))
    Share.Learned fid inf -> do
      modifyIORef' (networkGroupFiles network) (Map.insertWith (\_ old -> old) fid (inf, Nothing, 0))
      pure [GroupFileLearned gid fid inf]
    Share.Chunk fid n bytes -> Map.lookup fid <$> readIORef (networkGroupFiles network) >>= \case
      Just (inf, Just path, done) -> do
        withBinaryFile path ReadWriteMode $ \h -> hSeek h AbsoluteSeek (fromIntegral n * fromIntegral (Info.chunkSize inf)) >> BS.hPut h bytes
        let done' = done + fromIntegral (BS.length bytes)
        modifyIORef' (networkGroupFiles network) (Map.insert fid (inf, Just path, done'))
        t <- getMonotonicTime
        rate <- maybe 0 (Stats.rateDown . Share.stats t fid) . Map.lookup g <$> readMVar (networkShares network)
        pure [GroupFileMoved gid fid path done' rate]
      _ -> pure []
    -- From the file we write its chunks to, which has this one.
    Share.ChunkWanted n fid i -> Map.lookup fid <$> readIORef (networkGroupFiles network) >>= \case
      Just (inf, Just path, _) -> do
        bytes <- withBinaryFile path ReadMode $ \h -> hSeek h AbsoluteSeek (fromIntegral i * fromIntegral (Info.chunkSize inf)) >> BS.hGet h (Info.chunkLength inf i)
        withShare network g (\t -> Share.sendChunk t n fid i bytes)
      _ -> pure []
    Share.Finished fid -> pure [GroupFileEnded gid fid True]
    Share.HistoryWanted n range -> pure [GroupHistoryWanted gid n range]
    Share.History n entries -> do
      self <- fmap (hex . Sodium.encode) <$> Tox.groupSelfPublicKey tox g
      now <- getCurrentTime
      let recalled (History.Entry ts key mid body) =
            (posixSecondsToUTCTime (fromIntegral ts / 10), if Just (hex key) == self then Nothing else Just (hex key), mid, body)
      pure [GroupRecalled gid now n Map.empty (map recalled entries)]
  where tox = networkTox network

groupNumber :: Network -> GroupId -> IO (Maybe Word32)
groupNumber network gid = fmap fst . find ((== gid) . snd) . Map.toList <$> readIORef (networkGroups network)

-- | Hashes our file and shares it in the group.
offering :: Network -> (GroupId, MessageId, FilePath, Text.Text) -> IO [Event]
offering network (gid, i, path, name) = groupNumber network gid >>= \case
  Nothing -> pure [GroupFileOffered gid i Nothing]
  Just g -> (try (withBinaryFile path ReadMode hashed) :: IO (Either IOException Info.Info)) >>= \case
    Left _ -> pure [GroupFileOffered gid i Nothing]
    Right inf -> do
      mid <- BS.foldr (\b n -> n * 256 + fromIntegral b) 0 <$> randomByteString 4
      modifyIORef' (networkGroupFiles network) (Map.insert (Info.hash inf) (inf, Just path, Info.size inf))
      (GroupFileOffered gid i (Just (inf, mid)) :) <$> withShare network g (const (Share.share mid inf))
  where
    hashed h = do
      size <- fromIntegral <$> hFileSize h
      let chunk = Info.chunkSizeFor size
          next = do
            bytes <- BS.hGet h (fromIntegral chunk)
            if BS.null bytes then pure [] else (Info.sha1 bytes :) <$> next
      Info.Info (Text.encodeUtf8 name) size chunk . BS.concat <$> next

-- | Its constructor's name, without showing the rest (derived 'Show' is lazy).
constructor :: Show a => a -> Text.Text
constructor = Text.pack . takeWhile (/= ' ') . show

-- | Everything the debug screen shows, but the app's model.
measure :: Network -> IO Debug.Report
measure network = do
  rt <- Debug.runtime
  ts <- Debug.threads
  app <- Lanes.lanes (networkLanes network)
  loop <- Tox.timings (networkTox network)
  sizes <- Tox.sizes (networkTox network)
  counted <- Tox.traffic (networkTox network)
  calls <- AV.waitingEvents (networkAV network)
  playouts <- readMVar (networkPlayouts network)
  inCall <- forM (Map.toList playouts) $ \(pk, p) ->
    Debug.Call (Text.take 8 (Text.decodeUtf8 (Base16.encode (Sodium.encode pk)))) (Playout.stats p)
      <$> AV.videoStats (networkAV network) pk
      <*> (fmap Text.pack <$> AV.videoEncoderName (networkAV network) pk)
  pure Debug.Report
    { Debug.reportRuntime = rt
    , Debug.reportThreads = ts
    , Debug.reportApp     = app
    , Debug.reportTox     = loop
    , Debug.reportSizes   = sizes ++ [Tox.Size "Call events not taken yet" calls Nothing]
    , Debug.reportTraffic = counted
    , Debug.reportCalls   = inCall
    }

queueSync :: Network -> PublicKey -> [BS.ByteString] -> IO ()
queueSync network pk packets = do
  modifyMVar_ (networkSync network) (pure . Map.insertWith (flip (++)) pk packets)
  flushSync network

-- | Send sync packets in order until Tox refuses one (full queue). The rest
-- go later. Drop packets of friends that are gone.
flushSync :: Network -> IO ()
flushSync network = modifyMVar_ (networkSync network) (Map.traverseMaybeWithKey send)
  where
    send _ [] = pure Nothing
    send pk (packet : rest) = Tox.friendSendLosslessPacket (networkTox network) pk packet >>= \case
      Right () -> send pk rest
      Left Tox.CustomSendQueueFull -> pure (Just (packet : rest))
      Left err -> Nothing <$ networkLog network ("Sync packets dropped: " <> Text.pack (show err))

-- | Repeat in a thread, also after failures, so one bad event doesn't stop
-- everything after it. Only ends when Tox stops or the thread is killed.
looping :: Network -> Text.Text -> IO () -> IO ThreadId
looping network name action = forkIO ((myThreadId >>= (`labelThread` Text.unpack (Text.toLower name))) >> go)
  where
    go = try action >>= \case
      Right () -> go
      Left failure
        | Just Tox.ToxStopped <- fromException failure -> networkLog network (name <> ": Tox stopped")
        -- Killed, e.g. when switching accounts.
        | Just (SomeAsyncException _) <- fromException failure -> pure ()
        | otherwise -> networkLog network (name <> ": " <> Text.pack (displayException failure)) >> go

-- | A friend went offline or was deleted. A sync session starts again on the
-- next connection.
friendGone :: Network -> PublicKey -> IO ()
friendGone network pk = do
  Files.friendGone (networkFiles network) pk
  Avatars.friendGone (networkAvatars network) pk
  Blobs.friendGone (networkBlobs network) pk
  Outgoing.friendGone (networkOutgoing network) pk
  modifyMVar_ (networkSync network) (pure . Map.delete pk)

fromFiles :: Files.Report -> Event
fromFiles = \case
  Files.Moved cid ref done   -> FileMoved cid ref done
  Files.Ended cid ref whole  -> FileEnded cid ref whole

fromAvatars :: Avatars.Report -> Event
fromAvatars (Avatars.Avatar cid png) = FriendAvatar cid png

fromOutgoing :: Outgoing.Report -> Event
fromOutgoing = \case
  Outgoing.Delivering cid i delivery -> Delivering cid i delivery
  Outgoing.Confirmed cid i n time    -> Confirmed cid i n time

fromBlobs :: Blobs.Report -> Event
fromBlobs = \case
  Blobs.Arrived blob path small -> BlobArrived blob path small
  Blobs.Missing blob            -> BlobMissing blob

translate :: Network -> Tox.Event -> IO [Event]
translate network = \case
  Tox.SelfConnectionChanged status -> do
    networkLog network ("Tox connection: " <> Text.pack (show status))
    pure [Connected (connection status)]
  Tox.FriendConnectionChanged pk status -> do
    when (status == Tox.NotConnected) $ friendGone network pk
    AV.friendConnection (networkAV network) pk (status /= Tox.NotConnected)
    pure [FriendOnline (contactId pk) (status /= Tox.NotConnected)]
  Tox.FriendName pk name -> pure [FriendNamed (contactId pk) (utf8 name)]
  Tox.FriendStatusMessage pk message -> pure [FriendStatusMessage (contactId pk) (utf8 message)]
  Tox.FriendStatus pk status -> pure [FriendStatus (contactId pk) status]
  Tox.FriendTyping pk typing -> pure [FriendTyping (contactId pk) typing]
  Tox.FriendMessage pk kind message -> do
    now <- getCurrentTime
    pure [Received (contactId pk) now kind message]
  Tox.FriendReadReceipt pk receipt -> map fromOutgoing <$> Outgoing.receipt (networkOutgoing network) pk receipt
  Tox.FriendRequest pk message -> pure [Requested (contactId pk) (utf8 message)]
  Tox.FileReceive pk n kind size fileId name
    | kind == Tox.fileKindAvatar -> map fromAvatars <$> Avatars.offered tox avatars (contactId pk) pk n size fileId
    | kind == Blobs.blobKind -> [] <$ Blobs.offered tox blobs pk n size fileId
    | otherwise -> do
        now <- getCurrentTime
        let cid = contactId pk
        path <- Files.offered tox files (networkReceived network) now cid pk n kind size (utf8 name)
        pure [FileOffered cid (Receiving n) now (utf8 name) size p fileId | Just p <- [path]]
  Tox.FileReceiveChunk pk n position bytes -> transferOf pk (Tox.Incoming n)
    (map fromAvatars <$> Avatars.received tox avatars pk n position bytes)
    (map fromBlobs <$> Blobs.received tox blobs pk n position bytes)
    (map fromFiles <$> Files.received tox files pk n position bytes)
  Tox.FileChunkRequest pk n position len -> transferOf pk (Tox.Outgoing n)
    ([] <$ Avatars.requested tox avatars pk n position len) ([] <$ Blobs.requested tox blobs pk n position len)
    (map fromFiles <$> Files.requested tox files pk n position len)
  Tox.FileControlChanged pk ref control -> transferOf pk ref
    ([] <$ when (control == Tox.FileCancel) (Avatars.cancelled avatars pk ref))
    ([] <$ when (control == Tox.FileCancel) (Blobs.cancelled blobs pk ref))
    (map fromFiles <$> Files.controlled files pk ref control)
  Tox.FriendLosslessPacket pk packet
    | BS.take 1 packet == BS.singleton Frame.packetId -> (\now -> [SyncReceived (contactId pk) now packet]) <$> getCurrentTime
    | otherwise -> [] <$ forward AV.losslessIds pk packet
  Tox.FriendLossyPacket pk packet -> [] <$ forward AV.lossyIds pk packet
  -- Invite data starts with the chat ID.
  Tox.GroupInvite pk invite name ->
    pure [Invited (contactId pk) (GroupId (hex (BS.take chatIdLength invite))) NgcGroup (utf8 name) invite]
  -- Cookie is the inviter's conference number, its type, then the ID.
  Tox.ConferenceInvite pk kind cookie ->
    pure [ Invited (contactId pk) (GroupId (hex (BS.drop 3 cookie)))
             (if kind == audioConference then AudioConference else TextConference) "" cookie ]
  Tox.ConferenceConnected g -> ofConference g $ \gid -> do
    now <- getCurrentTime
    title <- Tox.conferenceTitle tox g
    (++) ([GroupNamed gid (utf8 t) | Just t <- [title], not (BS.null t)] ++ [GroupConnected gid now])
      <$> conferencePeers g gid
  Tox.ConferenceTitle g _ title -> ofConference g $ \gid ->
    (GroupNamed gid (utf8 title) :) <$> savedConference tox g gid
  Tox.ConferencePeerName g n name -> ofConference g $ \gid -> pure [MemberNamed gid (fromIntegral n) (utf8 name)]
  Tox.ConferencePeerListChanged g -> ofConference g (conferencePeers g)
  Tox.ConferenceMessage g n kind message -> ofConference g $ \gid -> do
    now <- getCurrentTime
    pure [GroupReceived gid now (fromIntegral n) kind (utf8 message) Nothing]
  Tox.GroupSelfJoin g -> ofGroup g $ \gid -> do
    name <- Tox.groupName tox g
    topic <- Tox.groupTopic tox g
    now <- getCurrentTime
    (++ [GroupNamed gid (utf8 n) | Just n <- [name]] ++ [TopicChanged gid now Nothing (utf8 t) | Just t <- [topic]]
         ++ [GroupConnected gid now]) <$> groupStanding tox g gid
  Tox.GroupJoinFail g why -> ofGroup g $ \gid -> pure [GroupRefused gid why]
  Tox.GroupPeerJoin g n pk -> ofGroup g $ \gid -> do
    now <- getCurrentTime
    name <- maybe "" (utf8 . Tox.groupPeerName) . find ((== n) . Tox.groupPeerId) <$> Tox.groupPeers tox g
    -- Peers are part of the saved group (Tox finds them again from there).
    joined <- withShare network g Share.joined
    (MemberJoined gid now n (Member (hex (Sodium.encode pk)) name) :) . (<> joined) <$> groupStanding tox g gid
  Tox.GroupPeerExit g n how _ _ -> ofGroup g $ \gid -> do
    now <- getCurrentTime
    (MemberLeft gid now n how :) <$> withShare network g (const (Share.leave n))
  Tox.GroupCustomPacket g n bytes -> withShare network g (\t -> Share.receive t n Packet.Public bytes)
  Tox.GroupCustomPrivatePacket g n bytes -> withShare network g (\t -> Share.receive t n Packet.Private bytes)
  Tox.GroupPeerName g n name -> ofGroup g $ \gid -> pure [MemberNamed gid n (utf8 name)]
  Tox.GroupPeerStatus g n status -> ofGroup g $ \gid -> pure [MemberStatus gid n s | Just s <- [userStatus status]]
  Tox.GroupMessage g n kind message mid -> ofGroup g $ \gid -> do
    now <- getCurrentTime
    pure [GroupReceived gid now n kind (utf8 message) (Just mid)]
  -- These change the saved group.
  Tox.GroupTopic g setter topic -> ofGroup g $ \gid -> do
    now <- getCurrentTime
    (TopicChanged gid now setter (utf8 topic) :) <$> saved g gid
  Tox.GroupModeration g setter target what -> ofGroup g $ \gid -> do
    now <- getCurrentTime
    (GroupModerated gid now setter target what :) <$> groupStanding tox g gid
  Tox.GroupPrivateMessage g n kind message _ -> ofGroup g $ \gid -> do
    now <- getCurrentTime
    pure [PrivateReceived gid now n kind (utf8 message)]
  Tox.GroupPrivacy g _ -> ofGroup g (groupStanding tox g)
  Tox.GroupPeerLimit g _ -> ofGroup g (groupStanding tox g)
  Tox.GroupPassword g _ -> ofGroup g (groupStanding tox g)
  Tox.GroupTopicLock g _ -> ofGroup g (groupStanding tox g)
  Tox.GroupVoiceState g _ -> ofGroup g (groupStanding tox g)
  where
    tox = networkTox network
    files = networkFiles network
    avatars = networkAvatars network
    blobs = networkBlobs network
    transferOf pk ref forAvatar forBlob forFile = do
      avatar <- Avatars.has avatars pk ref
      blob <- Blobs.has blobs pk ref
      if avatar then forAvatar else if blob then forBlob else forFile
    connection = \case
      Tox.NotConnected -> Offline
      Tox.ConnectedTcp -> ViaTcp
      Tox.ConnectedUdp -> ViaUdp
    forward ids pk packet = forM_ (BS.uncons packet) $ \(ident, _) ->
      when (ident `elem` ids) $ Lanes.timed (networkLanes network) "Call packet in" (AV.receivePacket (networkAV network) pk packet)
    ofGroup g f = readIORef (networkGroups network) >>= maybe (pure []) f . Map.lookup g
    ofConference g f = readIORef (networkConferences network) >>= maybe (pure []) f . Map.lookup g
    conferencePeers g gid = do
      now <- getCurrentTime
      peers <- Tox.conferencePeers tox g
      let others = [ (fromIntegral (Tox.conferencePeerNumber p), Member (hex (Sodium.encode (Tox.conferencePeerPublicKey p))) (utf8 (Tox.conferencePeerName p)))
                   | p <- peers, Tox.conferencePeerPublicKey p /= Tox.selfPublicKey tox ]
      (MembersListed gid now others :) <$> savedConference tox g gid
    audioConference = 1
    saved = savedGroup tox
    chatIdLength = 32

-- | Media goes to the call's playout, not the model, or to the other end of
-- a call we relay.
called :: Network -> AV.Event PublicKey -> IO [Event]
called network = \case
  AV.Invited pk caps -> do
    now <- getCurrentTime
    pure [CallInvited (contactId pk) now (AV.sendsVideo caps)]
  AV.PeerChanged pk caps -> do
    -- Video goes to one end of a relayed call while the other sends it.
    relayed pk >>= mapM_ (\peer -> AV.setBitRate av peer AV.Video (if AV.sendsVideo caps then fromIntegral videoRate else 0))
    now <- getCurrentTime
    pure [CallPeer (contactId pk) now (AV.sendsVideo caps)]
  AV.Ended pk end -> do
    networkLog network ("Call ended: " <> Text.pack (show end))
    modifyMVar_ (networkPlayouts network) (pure . Map.delete pk)
    -- A relayed call ends at both ends.
    unrelayed network pk >>= mapM_ (\other -> AV.control av other AV.Cancel)
    now <- getCurrentTime
    -- Both ends called at once (either end may notice).
    pure $ if end `elem` [AV.Failed MSI.InvalidState, AV.PeerFailed MSI.InvalidState]
      then [CallGlared (contactId pk) now]
      else [CallEnded (contactId pk) now]
  AV.AudioReceived pk number captured frame -> [] <$ (relayed pk >>= \case
    Just peer -> AV.clock >>= \now -> void (AV.sendAudio av peer now frame)
    Nothing -> playout pk (Playout.audio <$> AV.clock <*> pure number <*> pure captured <*> pure frame))
  AV.VideoReceived pk captured picture -> [] <$ (relayed pk >>= \case
    Just peer -> AV.clock >>= \now -> void (AV.sendVideo av peer now picture)
    Nothing -> playout pk (Playout.picture <$> AV.clock <*> pure captured <*> pure picture))
  where
    av = networkAV network
    relayed pk = Map.lookup pk <$> readMVar (networkRelays network)
    playout pk came = came >>= \add -> modifyMVar_ (networkPlayouts network) (pure . Map.alter (Just . add . fromMaybe Playout.empty) pk)

-- | Stops relaying the call, giving its other end.
unrelayed :: Network -> PublicKey -> IO (Maybe PublicKey)
unrelayed network pk = modifyMVar (networkRelays network) $ \known -> pure $ case Map.lookup pk known of
  Just other -> (Map.delete other (Map.delete pk known), Just other)
  Nothing    -> (known, Nothing)

perform :: Network -> Command -> IO [Event]
perform network = \case
  SetProfile profile -> do
    named <- Tox.selfSetName tox (Text.encodeUtf8 (profileNickname profile))
    stated <- Tox.selfSetStatusMessage tox (Text.encodeUtf8 (profileStatus profile))
    unless (named && stated) $ networkLog network "Tox refused the nickname or status message"
    Tox.selfSetStatus tox (profileAway profile)
    -- Each group has its own copy of our name and status.
    groups <- Map.keys <$> readIORef (networkGroups network)
    [] <$ forM_ groups (\g -> do
      void $ Tox.groupSetNick tox g (Text.encodeUtf8 (groupNick profile))
      Tox.groupSetStatus tox g (profileAway profile))
  SetGroupStatus gid away -> [] <$ (readIORef (networkGroups network)
    >>= mapM_ (\g -> Tox.groupSetStatus tox g away) . Map.keys . Map.filter (== gid))
  Befriend cid -> [] <$ forM_ (key cid) (Tox.friendAddNoRequest tox)
  SetTyping cid typing -> [] <$ forM_ (key cid) (\pk -> Tox.friendSetTyping tox pk typing)
  SendSync cid packets -> [] <$ forM_ (key cid) (\pk -> queueSync network pk packets)
  FetchBlob cid blob packets name -> do
    -- Prefix the blob ID so it doesn't clash with friends' other files.
    let target = (\n -> networkReceived network </> Files.fitted (take 8 (Text.unpack (hex blob)) <> "-") (Files.safeName n)) <$> name
    Blobs.located (networkBlobs network) blob target >>= \case
      Just path -> do
        small <- if isNothing name then Just <$> BS.readFile path else pure Nothing
        pure [BlobArrived blob path small]
      Nothing -> [] <$ forM_ (key cid) (\pk -> unless (null packets) $ do
        Blobs.want (networkBlobs network) pk blob target
        queueSync network pk packets)
  ServeBlob cid blob missing -> [] <$ forM_ (key cid) (\pk -> do
    served <- Blobs.serve tox (networkBlobs network) pk blob
    unless served $ queueSync network pk missing)
  StoreBlob bytes -> [] <$ Blobs.store (networkBlobs network) bytes
  ShareFile cid path name size -> do
    blob <- Blobs.storeFile (networkBlobs network) path
    now <- getCurrentTime
    pure [FileShared cid now name size blob path]
  -- A file that's gone can't be held (the devices see it ended without one).
  HoldFile cid i path -> (try (Blobs.storeFile (networkBlobs network) path) :: IO (Either IOException BS.ByteString)) >>= \case
    Right blob -> pure [FileHeld cid i blob]
    Left _ -> pure []
  MeasureDebug -> pure . Measured <$> measure network
  Unfriend cid -> [] <$ forM_ (key cid) (\pk -> friendGone network pk >> Tox.friendDelete tox pk)
  SendAvatar cid png -> [] <$ forM_ (key cid) (\pk -> Avatars.send tox (networkAvatars network) pk png)
  RequestFriend address message -> do
    let adding = Tox.friendAdd tox (unhex address) (Text.encodeUtf8 message)
    added <- adding >>= \case
      -- Tox drops requests from friends, so a friend who deleted us only
      -- gets our request if we add them as new.
      Left (Just Tox.AlreadyFriend) | Just (pk, _) <- Tox.parseAddress (unhex address) ->
        Tox.friendDelete tox pk >> adding
      result -> pure result
    pure [AddFailed address err | Left err <- [added]]
  SendMessage cid i message confirmed -> case key cid of
    Nothing -> pure []
    Just pk -> map fromOutgoing <$> Outgoing.send tox (networkOutgoing network) cid pk i message confirmed
  SendFile cid i path name size fileId -> case key cid of
    Nothing -> pure []
    Just pk -> do
      started <- Files.offer tox files cid pk path name size fileId
      pure [FileStarted cid i ref | Just ref <- [started]]
  AcceptFile cid ref path -> maybe (pure []) (\pk -> map fromFiles <$> Files.accept tox files pk ref path) (key cid)
  CancelFile cid ref -> [] <$ forM_ (key cid) (\pk -> Files.cancel tox files pk ref)
  ForgetFile path -> [] <$ (try (removeFile path) :: IO (Either IOException ()))
  CallFriend cid camera -> calling cid (\pk -> AV.call av pk (rates camera))
  AnswerFriend cid camera -> calling cid (\pk -> AV.answer av pk (rates camera))
  -- With the other end of a relayed call.
  HangUpCall cid -> [] <$ forM_ (key cid) (\pk ->
    unrelayed network pk >>= mapM_ (\other -> AV.control av other AV.Cancel) . (pk :) . maybe [] pure)
  RelayCall friend device video -> case (key friend, key device) of
    (Just f, Just d) -> AV.call av d (rates video) >>= \case
      Right () -> [] <$ modifyMVar_ (networkRelays network) (pure . Map.insert f d . Map.insert d f)
      Left refusal -> networkLog network ("Couldn't call the device: " <> Text.pack (show refusal)) >> ended device
    _ -> pure []
  CameraCall cid camera ->
    [] <$ forM_ (key cid) (\pk -> AV.setBitRate av pk AV.Video (if camera then fromIntegral videoRate else 0))
  JoinGroup cid gid invite nick password -> case key cid of
    Nothing -> pure []
    Just pk -> Tox.groupInviteAccept tox pk invite (Text.encodeUtf8 nick) (Text.encodeUtf8 password) >>= \case
      Right g -> [] <$ modifyIORef' (networkGroups network) (Map.insert g gid)
      Left Tox.AcceptAlreadyJoined -> pure []
      Left refusal -> do
        networkLog network ("Group invite refused: " <> Text.pack (show refusal))
        pure [GroupRefused gid Tox.JoinFailUnknown]
  JoinConference cid gid cookie -> case key cid of
    Nothing -> pure []
    Just pk -> Tox.conferenceJoin tox pk cookie >>= \case
      Right g -> [] <$ modifyIORef' (networkConferences network) (Map.insert g gid)
      Left Tox.JoinAlreadyJoined -> pure []
      Left refusal -> do
        networkLog network ("Conference invite refused: " <> Text.pack (show refusal))
        pure [GroupRefused gid Tox.JoinFailUnknown]
  RestoreGroup NgcGroup gid bytes -> Tox.groupRestore tox bytes >>= \case
    Just g -> [] <$ modifyIORef' (networkGroups network) (Map.insert g gid)
    Nothing -> do
      networkLog network "Tox couldn't rejoin a group from what it kept"
      pure [GroupRefused gid Tox.JoinFailUnknown]
  -- Tox rejoins conferences as peers come online, without an event.
  RestoreGroup _ gid bytes -> Tox.conferenceRestore tox bytes >>= \case
    Just g -> do
      modifyIORef' (networkConferences network) (Map.insert g gid)
      now <- getCurrentTime
      pure [GroupConnected gid now]
    Nothing -> do
      networkLog network "Tox couldn't rejoin a conference from what it kept"
      pure [GroupRefused gid Tox.JoinFailUnknown]
  SendGroupMessage gid i message -> numberOf gid >>= \case
    Just g -> do
      sent <- mapM (Tox.groupSendMessage tox g Tox.MessageNormal) (parts message)
      case sequence sent of
        Right toxIds -> pure [GroupSent gid i (listToMaybe toxIds)]
        Left refusal -> [] <$ networkLog network ("Group message refused: " <> Text.pack (show refusal))
    Nothing -> conferenceOf gid >>= \case
      Nothing -> pure []
      -- Nobody to send it to yet, so it waits for peers.
      Just g -> do
        sent <- mapM (Tox.conferenceSendMessage tox g Tox.MessageNormal) (parts message)
        pure [GroupSent gid i Nothing | and sent]
  FetchGroupFile gid inf -> numberOf gid >>= \case
    Just g -> Map.lookup fid <$> readIORef (networkGroupFiles network) >>= \case
      -- Paused this run: what came is still there.
      Just (_, Just path, done) -> (GroupFileMoved gid fid path done 0 :) <$> withShare network g (const (\s -> (Share.fetch inf s, [])))
      _ -> do
        let path = networkReceived network </> Files.fitted (Text.unpack (Text.take 8 (hex fid)) <> "-") (Files.safeName (utf8 (Info.name inf)))
        BS.writeFile path BS.empty
        modifyIORef' (networkGroupFiles network) (Map.insert fid (inf, Just path, 0))
        (GroupFileMoved gid fid path 0 0 :) <$> withShare network g (const (\s -> (Share.fetch inf s, [])))
    Nothing -> pure [GroupFileEnded gid fid False]
    where fid = Info.hash inf
  PauseGroupDownload gid fid -> numberOf gid >>= \case
    Just g  -> withShare network g (const (\s -> (Share.pause fid s, [])))
    Nothing -> pure []
  LearnGroupFile gid n fid -> numberOf gid >>= \case
    Just g  -> withShare network g (const (\s -> (Share.learn n fid s, [])))
    Nothing -> pure []
  ShareGroupFile gid i path name -> [] <$ writeChan (networkHashing network) (gid, i, path, name)
  SeedGroupFiles gid whole -> numberOf gid >>= \case
    Just g -> do
      modifyIORef' (networkGroupFiles network) (\m -> foldr (\(inf, path) -> Map.insert (Info.hash inf) (inf, Just path, Info.size inf)) m whole)
      withShare network g (const (\s -> (foldr (Share.seed . fst) s whole, [])))
    Nothing -> pure []
  -- A few seconds after it joins, as totato does: a member that just
  -- joined may not take our packets yet.
  AskHistory gid n -> numberOf gid >>= \case
    Just g -> [] <$ forkIO (threadDelay 5000000 >> floor <$> getPOSIXTime >>= \unixNow -> void (withShare network g (const (Share.requestHistory unixNow n))))
    Nothing -> pure []
  AnswerHistory gid n range recalled -> numberOf gid >>= \case
    Just g -> Tox.groupSelfPublicKey tox g >>= \self -> withShare network g $ \t -> Share.answerHistory t n range
      [ History.Entry (floor (utcTimeToPOSIXSeconds time * 10)) ppk mid body
      | (time, sender, mid, body) <- recalled, Just ppk <- [maybe (Sodium.encode <$> self) (Just . unhex) sender] ]
    Nothing -> pure []
  SetGroupTopic gid topic -> numberOf gid >>= \case
    Nothing -> pure []
    Just g -> Tox.groupSetTopic tox g (Text.encodeUtf8 topic) >>= \case
      False -> pure [ToxRefused (TopicRefused gid)]
      True -> do
        now <- getCurrentTime
        (TopicSetByUs gid now topic :) <$> savedGroup tox g gid
  FoundGroup NewConference name _ -> Tox.conferenceNew tox >>= \case
    Nothing -> pure [ToxRefused (FoundingRefused NewConference)]
    Just g -> do
      void $ Tox.conferenceSetTitle tox g (Text.encodeUtf8 name)
      Tox.conferenceId tox g >>= \case
        Nothing -> pure []
        Just conference -> do
          let gid = GroupId (hex conference)
          modifyIORef' (networkConferences network) (Map.insert g gid)
          now <- getCurrentTime
          (GroupCreated gid TextConference name now :) <$> savedConference tox g gid
  FoundGroup founding name nick ->
    Tox.groupNew tox (founding == PrivateGroup) (Text.encodeUtf8 name) (Text.encodeUtf8 nick) >>= \case
      Nothing -> pure [ToxRefused (FoundingRefused founding)]
      Just g -> Tox.groupChatId tox g >>= \case
        Nothing -> pure []
        Just chat -> do
          let gid = GroupId (hex chat)
          modifyIORef' (networkGroups network) (Map.insert g gid)
          now <- getCurrentTime
          (GroupCreated gid NgcGroup name now :) <$> groupStanding tox g gid
  JoinPublicGroup gid@(GroupId chat) nick password ->
    Tox.groupJoin tox (unhex chat) (Text.encodeUtf8 nick) (Text.encodeUtf8 password) >>= \case
      Right g -> [] <$ modifyIORef' (networkGroups network) (Map.insert g gid)
      Left Tox.AcceptAlreadyJoined -> pure []
      Left refusal -> do
        networkLog network ("Public group refused: " <> Text.pack (show refusal))
        pure [GroupRefused gid Tox.JoinFailUnknown]
  -- Tox can only invite friends who are online, to a connected group.
  InviteToGroup gid cid -> case key cid of
    Nothing -> pure []
    Just pk -> do
      invited <- numberOf gid >>= \case
        Just g -> Tox.groupInviteFriend tox g pk
        Nothing -> conferenceOf gid >>= maybe (pure False) (Tox.conferenceInvite tox pk)
      pure [if invited then InviteSent gid cid else ToxRefused (InviteRefused gid cid)]
  SendPrivate gid i n message -> numberOf gid >>= \case
    Nothing -> pure []
    Just g -> do
      sent <- mapM (Tox.groupSendPrivateMessage tox g n Tox.MessageNormal) (parts message)
      case sequence sent of
        Right _ -> pure [GroupSent gid i Nothing]
        Left refusal -> [] <$ networkLog network ("Private message refused: " <> Text.pack (show refusal))
  -- Tox only reports our own moderation as the result (no event).
  ModerateMember gid n permission -> numberOf gid >>= \case
    Nothing -> pure []
    Just g -> do
      done <- case permission of
        Kick          -> Tox.groupKickPeer tox g n
        MakeModerator -> Tox.groupSetRole tox g n Tox.Moderator
        MakeUser      -> Tox.groupSetRole tox g n Tox.User
        MakeObserver  -> Tox.groupSetRole tox g n Tox.Observer
      now <- getCurrentTime
      case done of
        Left refusal -> pure [ToxRefused (ModerationRefused gid n refusal)]
        Right () -> (\standing -> GroupModerated gid now Nothing (Just n) (moderation permission)
                                   : [MemberLeft gid now n Tox.ExitKicked | permission == Kick] ++ standing)
                      <$> groupStanding tox g gid
  -- Only what changed (each change goes to every peer).
  SetGroupSettings gid wanted -> numberOf gid >>= \case
    Nothing -> pure []
    Just g -> Tox.groupSettings tox g >>= \case
      Nothing -> pure []
      Just now -> do
        let changes =
              [ Tox.groupSetPrivacy tox g (Tox.groupPrivate wanted) | Tox.groupPrivate wanted /= Tox.groupPrivate now ]
              ++ [ Tox.groupSetPassword tox g (Tox.groupPassword wanted) | Tox.groupPassword wanted /= Tox.groupPassword now ]
              ++ [ Tox.groupSetPeerLimit tox g (Tox.groupPeerLimit wanted) | Tox.groupPeerLimit wanted /= Tox.groupPeerLimit now ]
              ++ [ Tox.groupSetTopicLock tox g (Tox.groupTopicLock wanted) | Tox.groupTopicLock wanted /= Tox.groupTopicLock now ]
              ++ [ Tox.groupSetVoice tox g (Tox.groupVoice wanted) | Tox.groupVoice wanted /= Tox.groupVoice now ]
        taken <- and <$> sequence changes
        (++) [ToxRefused (SettingRefused gid) | not taken] <$> groupStanding tox g gid
  QuitGroup gid -> numberOf gid >>= \case
    Just g -> do
      modifyIORef' (networkGroups network) (Map.delete g)
      [] <$ Tox.groupLeave tox g ""
    Nothing -> conferenceOf gid >>= \case
      Nothing -> pure []
      Just g -> do
        modifyIORef' (networkConferences network) (Map.delete g)
        [] <$ Tox.conferenceDelete tox g
  where
    numberOf = groupNumber network
    conferenceOf gid = fmap fst . find ((== gid) . snd) . Map.toList <$> readIORef (networkConferences network)
    av = networkAV network
    rates camera = BitRates audioRate (if camera then videoRate else 0)
    -- A refused call ends at once, unless the friend called first (that
    -- comes as an invite).
    calling cid begin = case key cid of
      Nothing -> ended cid
      Just pk -> begin pk >>= \case
        Left AV.AlreadyInCall -> [] <$ networkLog network "Call refused: the friend's is there"
        Left refusal -> networkLog network ("Call refused: " <> Text.pack (show refusal)) >> ended cid
        Right () -> pure []
    ended cid = (\now -> [CallEnded cid now]) <$> getCurrentTime
    tox = networkTox network
    files = networkFiles network
    key = contactKey

savedGroup :: Tox.Tox -> Word32 -> GroupId -> IO [Event]
savedGroup tox g gid = maybe [] (pure . GroupSaved gid) <$> Tox.groupSave tox g

-- | Our role, the members' roles, founder settings and whether we may set
-- the topic, plus the saved group.
groupStanding :: Tox.Tox -> Word32 -> GroupId -> IO [Event]
groupStanding tox g gid = do
  self <- Tox.groupSelfRole tox g
  peers <- Tox.groupPeers tox g
  settings <- Tox.groupSettings tox g
  allowed <- Tox.groupMayTopic tox g
  (++) ([RolesKnown gid self [(Tox.groupPeerId p, Tox.groupPeerRole p) | p <- peers], TopicAllowed gid allowed]
        ++ [MemberStatus gid (Tox.groupPeerId p) st | p <- peers, Just st <- [userStatus (Tox.groupPeerStatus p)]]
        ++ [SettingsKnown gid s | Just s <- [settings]])
    <$> savedGroup tox g gid

userStatus :: Word8 -> Maybe Tox.UserStatus
userStatus n
  | n <= fromIntegral (fromEnum (maxBound :: Tox.UserStatus)) = Just (toEnum (fromIntegral n))
  | otherwise = Nothing

moderation :: MemberAction -> Tox.Moderation
moderation = \case
  Kick          -> Tox.ModKick
  MakeModerator -> Tox.ModModerator
  MakeUser      -> Tox.ModUser
  MakeObserver  -> Tox.ModObserver

savedConference :: Tox.Tox -> Tox.ConferenceNumber -> GroupId -> IO [Event]
savedConference tox g gid = maybe [] (pure . GroupSaved gid) <$> Tox.conferenceSave tox g

-- | The next 10 ms of a call's audio, to be heard some ms from now.
pullAudio :: Network -> ContactId -> Int -> IO (Maybe AV.AudioFrame)
pullAudio network cid ahead = playing network cid $ \now -> Playout.pull now (AV.later ahead now)

-- | The picture of a call to show, seen until some ms from now, if a new one
-- is due.
dueVideo :: Network -> ContactId -> Int -> IO (Maybe Picture)
dueVideo network cid ahead = playing network cid $ \now -> Playout.pictureAt now (AV.later ahead now)

playing :: Network -> ContactId -> (AV.MediaTime -> Playout -> (Maybe (AV.MediaTime, a), Playout)) -> IO (Maybe a)
playing network cid step = case contactKey cid of
  Nothing -> pure Nothing
  Just pk -> AV.clock >>= \now -> modifyMVar (networkPlayouts network) $ \playouts ->
    pure $ case Map.lookup pk playouts of
      Nothing -> (playouts, Nothing)
      Just p  -> let (due, p') = step now p in (Map.insert pk p' playouts, snd <$> due)

sendAudio :: Network -> ContactId -> Int -> Int -> Pcm -> IO ()
sendAudio network cid rate channels pcm = forM_ (contactKey cid) $ \pk -> recorded <$> AV.clock >>= \captured ->
  void . Lanes.timed (networkLanes network) "Call audio out" $ AV.sendAudio (networkAV network) pk captured (AV.AudioFrame rate channels pcm)
  where
    -- Its first sample, as the frame has just been recorded.
    recorded = AV.later (negate (V.length pcm `div` channels * 1000 `div` rate))

sendVideo :: Network -> ContactId -> Picture -> IO ()
sendVideo network cid picture = forM_ (contactKey cid) $ \pk -> AV.clock >>= \captured ->
  void . Lanes.timed (networkLanes network) "Call video out" $ AV.sendVideo (networkAV network) pk captured picture

contactKey :: ContactId -> Maybe PublicKey
contactKey (ContactId text) = publicKey (Text.encodeUtf8 text)

publicKey :: BS.ByteString -> Maybe PublicKey
publicKey = either (const Nothing) Sodium.decode . Base16.decode

contactId :: PublicKey -> ContactId
contactId = ContactId . hex . Sodium.encode

hex :: BS.ByteString -> Text.Text
hex = Text.toUpper . Text.decodeUtf8 . Base16.encode

utf8 :: BS.ByteString -> Text.Text
utf8 = Text.decodeUtf8Lenient
