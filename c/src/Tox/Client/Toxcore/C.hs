{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeApplications  #-}
-- | Tox on c-toxcore. One thread owns the C instance and runs what the
-- operations ask for between its iterations, as c-toxcore isn't thread-safe.
module Tox.Client.Toxcore.C
  ( cToxcore
  , savedGroup
  , chatIdOf
  ) where

import           Control.Concurrent             (forkIO)
import           Control.Concurrent.MVar        (newEmptyMVar, putMVar,
                                                 takeMVar, tryPutMVar)
import           Control.Concurrent.STM         (TMVar, TQueue, TVar,
                                                 atomically, check, flushTQueue,
                                                 newEmptyTMVarIO, newTQueueIO,
                                                 newTVarIO, orElse, putTMVar,
                                                 readTMVar, readTQueue,
                                                 readTVar, tryReadTMVar,
                                                 writeTQueue, writeTVar)
import           Control.Exception              (SomeException, finally,
                                                 throwIO, toException, try)
import           Control.Monad                  (void)
import qualified Crypto.Saltine.Class           as Sodium
import           Data.Bifunctor                 (bimap, first)
import           Data.Binary                    (Binary)
import qualified Data.Binary                    as Binary
import           Data.Bits                      (shiftL, shiftR)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import           Data.Either                    (isRight)
import           Data.Foldable                  (find)
import           Data.IORef                     (IORef, modifyIORef', newIORef,
                                                 readIORef)
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (catMaybes, fromMaybe)
import qualified Data.Set                       as Set
import qualified Data.Text                      as Text
import           Data.Word                      (Word16, Word32, Word8)
import           GHC.Clock                      (getMonotonicTime)
import qualified Network.Tox.C.Conference       as C
import qualified Network.Tox.C.Events           as C
import qualified Network.Tox.C.File             as C
import qualified Network.Tox.C.Friend           as C
import qualified Network.Tox.C.Group            as C
import qualified Network.Tox.C.Lifecycle        as C
import           Network.Tox.C.Marshal          (FixedBytes (..), fromBytes)
import qualified Network.Tox.C.Options          as C
import qualified Network.Tox.C.Tox              as C
import qualified Network.Tox.C.Types            as C
import           System.Timeout                 (timeout)
import qualified Tox.Application.Tox            as Hs
import           Tox.Client.Toxcore
import           Tox.Client.Types               (GroupKind (..), Toxcore (..))
import qualified Tox.Core.Log                   as Log
import           Tox.Crypto.Core.Key            (PublicKey, SecretKey)
import qualified Tox.Crypto.Core.KeyPair        as KeyPair
import qualified Tox.Network.Core.HostAddress   as Host
import           Tox.Network.Core.NodeInfo      (NodeInfo (NodeInfo))
import           Tox.Network.Core.PortNumber    (PortNumber (..))
import           Tox.Network.Core.SocketAddress (SocketAddress (..))
import qualified Tox.Persistence                as P
import qualified Tox.Persistence.Conferences    as PC
import qualified Tox.Persistence.Groups         as PG

cToxcore :: Engine
cToxcore = Engine CToxcore start

-- | Runs on the thread that owns the C instance, given 'Nothing' once it's
-- gone.
type Job = Maybe C.Tox -> IO ()

data Loop = Loop
  { loopJobs   :: TQueue Job
  , loopEvents :: TQueue Event
  , loopStop   :: TVar Bool
  , loopEnded  :: TMVar ()
  , loopPeers  :: IORef Peers
    -- ^ Only the owning thread uses it.
  }

-- | The peers in each group, as c-toxcore can't list them.
type Peers = Map.Map GroupNumber (Set.Set PeerId)

start :: Options -> [(GroupKind, BS.ByteString)] -> IO (Either StartError Tox)
start options kept = C.defaultOptions >>= \case
  Left _ -> pure (Left NoMemory)
  Right defaults -> either (pure . Left) begin (configured defaults <$> traverse proxied (optProxy options))
  where
    configured defaults proxy = loaded defaults
      { C.ipv6Enabled = optIpv6Enabled options
      , C.udpEnabled = optUdpEnabled options
      , C.localDiscoveryEnabled = optLocalDiscovery options
      , C.proxyType = maybe C.ProxyTypeNone fst proxy
      , C.proxyHost = maybe "" (Hs.proxyHost . snd) proxy
      , C.proxyPort = maybe 0 (Hs.proxyPort . snd) proxy
      , C.startPort = fst (optUdpPorts options)
      , C.endPort = snd (optUdpPorts options)
      , C.tcpPort = fromMaybe 0 (optTcpPort options)
      , C.experimentalGroupsPersistence = True
      }
    proxied p = case Hs.proxyType p of
      Hs.HttpProxy    -> Right (C.ProxyTypeHttp, p)
      Hs.Socks5Proxy  -> Right (C.ProxyTypeSocks5, p)
      Hs.GatewayProxy -> Left BadProxy
    loaded o = case optSaveData options of
      NoSaveData   -> o { C.savedataType = C.SavedataTypeNone }
      ToxSave save -> o { C.savedataType = C.SavedataTypeToxSave, C.savedata = save }
      SecretKey sk -> o { C.savedataType = C.SavedataTypeToxSave, C.savedata = toxSave sk kept }
    begin cOptions = do
      loop <- Loop <$> newTQueueIO <*> newTQueueIO <*> newTVarIO False <*> newEmptyTMVarIO <*> newIORef Map.empty
      started <- newEmptyMVar
      void . forkIO $ do
        result <- try @SomeException (C.withToxLogging cOptions (logged (optLogger options)) (\tox -> do
          keys <- (,) <$> C.toxSelfGetPublicKey tox <*> C.toxSelfGetSecretKey tox
          putMVar started (Right (Right keys))
          running loop (optLogger options) tox))
          `finally` (atomically (putTMVar (loopEnded loop) () >> flushTQueue (loopJobs loop)) >>= mapM_ ($ Nothing))
        let failed e = optLogger options Log.LevelError ("Tox stopped: " <> Text.pack (show e)) >> void (tryPutMVar started (Left e))
        either failed (either (void . tryPutMVar started . Right . Left . refused) pure) result
      takeMVar started >>= either throwIO (\case
        Left err -> pure (Left err)
        Right (pk, sk) -> pure . maybe (Left BadSaveData) (Right . instance' loop) $
          (,) <$> fromC pk <*> Sodium.decode (toBytes sk))
    refused = \case
      C.ErrNewPortAlloc     -> PortInUse
      C.ErrNewProxyBadType  -> BadProxy
      C.ErrNewProxyBadHost  -> BadProxy
      C.ErrNewProxyBadPort  -> BadProxy
      C.ErrNewProxyNotFound -> BadProxy
      C.ErrNewLoadEncrypted -> BadSaveData
      C.ErrNewLoadBadFormat -> BadSaveData
      C.ErrNewMalloc        -> NoMemory
      C.ErrNewNull          -> NoMemory

-- | Our keys and the groups and conferences we'll restore, in toxcore's save
-- format.
toxSave :: SecretKey -> [(GroupKind, BS.ByteString)] -> BS.ByteString
toxSave sk kept = LBS.toStrict . Binary.encode $ P.SaveData
  [ P.SectionNospamKeys (P.NospamKeys 0 (KeyPair.publicKey (KeyPair.fromSecretKey sk)) sk)
  , P.SectionGroups (PG.Groups [ g | (NgcGroup, saved) <- kept, Just (PG.Groups gs) <- [decoded saved], g <- gs ])
  , P.SectionConferences (PC.Conferences [ c | (kind, saved) <- kept, kind /= NgcGroup, Just c <- [decoded saved] ])
  , P.SectionEOF
  ]

logged :: (Log.LogLevel -> Text.Text -> IO ()) -> C.LogCallback
logged logger level file line function message =
  logger (levelOf level) (Text.pack (file <> ":" <> show line <> " " <> function <> ": " <> message))
  where
    levelOf = \case
      C.LogLevelTrace   -> Log.LevelDebug
      C.LogLevelDebug   -> Log.LevelDebug
      C.LogLevelInfo    -> Log.LevelInfo
      C.LogLevelWarning -> Log.LevelWarn
      C.LogLevelError   -> Log.LevelError

-- | Iterates when c-toxcore asks to, and runs jobs in between. A batch of
-- events that fails to decode is lost, not Tox.
running :: Loop -> (Log.LogLevel -> Text.Text -> IO ()) -> C.Tox -> IO ()
running loop logger tox = go =<< due
  where
    due = (+) <$> getMonotonicTime <*> ((/ 1000) . fromIntegral <$> C.toxIterationInterval tox)
    -- Due iterations before jobs, so a stream of jobs can't hold Tox up.
    go deadline = getMonotonicTime >>= \now -> if now >= deadline
      then iterateTox >> (go =<< due)
      else timeout (ceiling ((deadline - now) * 1000000)) (atomically next) >>= \case
        Nothing         -> go deadline
        Just Nothing    -> pure ()
        Just (Just job) -> job (Just tox) >> go deadline
    next = (Nothing <$ (readTVar (loopStop loop) >>= check)) `orElse` (Just <$> readTQueue (loopJobs loop))
    iterateTox = try @SomeException (C.toxEventsIterate tox >>= mapM_ (\events ->
      mapM (translate tox (loopPeers loop)) events >>= atomically . mapM_ (writeTQueue (loopEvents loop)) . concat))
      >>= either (logger Log.LevelError . ("Events lost: " <>) . Text.pack . show) pure

-- | Runs it on the owning thread and waits for it.
call :: Loop -> (C.Tox -> IO a) -> IO a
call loop f = do
  answer <- newEmptyMVar
  queued <- atomically $ tryReadTMVar (loopEnded loop) >>= \case
    Just () -> pure False
    Nothing -> True <$ writeTQueue (loopJobs loop) (\case
      Just tox -> try (f tox) >>= putMVar answer
      Nothing  -> putMVar answer (Left (toException ToxStopped)))
  if queued then takeMVar answer >>= either throwIO pure else throwIO ToxStopped

-- | Runs it on the owning thread without waiting.
cast :: Loop -> (C.Tox -> IO ()) -> IO ()
cast loop f = atomically $ tryReadTMVar (loopEnded loop) >>= \case
  Just () -> pure ()
  Nothing -> writeTQueue (loopJobs loop) (mapM_ (void . try @SomeException . f))

instance' :: Loop -> (PublicKey, SecretKey) -> Tox
instance' loop (pk, sk) = Tox
  { kill = atomically (writeTVar (loopStop loop) True) >> atomically (readTMVar (loopEnded loop))
  , nextEvent = atomically ((Just <$> readTQueue (loopEvents loop)) `orElse` (Nothing <$ readTMVar (loopEnded loop)))
      >>= maybe (throwIO ToxStopped) pure
  , bootstrap = \n -> ask $ \t -> void (atNode (C.toxBootstrap t) n)
  , addTcpRelay = \n -> ask $ \t -> void (atNode (C.toxAddTcpRelay t) n)
  , selfConnectionStatus = ask (fmap connection . C.toxSelfGetConnectionStatus)
  , selfPublicKey = pk
  , selfSecretKey = sk
  , selfAddress = ask (fmap toBytes . C.toxSelfGetAddress)
  , selfGetNospam = ask C.toxSelfGetNospam
  , selfSetNospam = \n -> ask (`C.toxSelfSetNospam` n)
  , selfSetName = \name -> ask $ \t -> isRight <$> C.toxSelfSetName t name
  , selfSetStatusMessage = \message -> ask $ \t -> isRight <$> C.toxSelfSetStatusMessage t message
  , selfSetStatus = \status -> ask $ \t -> C.toxSelfSetStatus t (userStatusC status)
  , friendAdd = \address message -> case parseAddress address of
      Nothing -> pure (Left Nothing)
      Just (friend, _) -> maybe (pure (Left Nothing)) (\a -> ask $ \t -> bimap added (const friend) <$> C.toxFriendAdd t a message)
        (fromBytes address)
  , friendAddNoRequest = \friend -> ask $ \t -> bimap noRequest (const ()) <$> C.toxFriendAddNorequest t (cKey friend)
  , friendDelete = \friend -> ask $ \t -> friendly t friend False (fmap isRight . C.toxFriendDelete t)
  , friendSendMessage = \friend kind message -> ask $ \t -> friendly t friend (Left SendFriendNotFound) $ \f ->
      bimap sendError (\(C.FriendMessageId i) -> i) <$> C.toxFriendSendMessage t f (messageTypeC kind) message
  , friendSetTyping = \friend typing -> ask $ \t -> friendly t friend False (\f -> isRight <$> C.toxSelfSetTyping t f typing)
  , friendSendLosslessPacket = \friend packet -> ask $ \t -> friendly t friend (Left CustomFriendNotFound) $ \f ->
      first packetError <$> C.toxFriendSendLosslessPacket t f packet
  , friendQueueLosslessPacket = \friend packet -> tell $ \t -> friendly t friend () (void . flip (C.toxFriendSendLosslessPacket t) packet)
  , friendQueueLossyPacket = \friend packet -> tell $ \t -> friendly t friend () (void . flip (C.toxFriendSendLossyPacket t) packet)
  , fileSend = \friend kind size fileId name -> ask $ \t -> friendly t friend (Left FileFriendNotFound) $ \f ->
      bimap fileSendError (\(C.FileNumber n) -> fromIntegral n) <$> C.toxFileSend t f kind size (fromBytes =<< fileId) name
  , fileControl = \friend ref control -> ask $ \t -> friendly t friend (Left FileFriendNotFound) $ \f ->
      first fileControlError <$> C.toxFileControl t f (fileNumber ref) (fileControlC control)
  , fileSeek = \friend n position -> ask $ \t -> friendly t friend (Left FileFriendNotFound) $ \f ->
      first fileSeekError <$> C.toxFileSeek t f (fileNumber (Incoming n)) position
  , fileSendChunk = \friend n position bytes -> ask $ \t -> friendly t friend (Left FileFriendNotFound) $ \f ->
      first fileChunkError <$> C.toxFileSendChunk t f (fileNumber (Outgoing n)) position bytes
  , conferenceNew = ask (fmap (either (const Nothing) (Just . conference)) . C.toxConferenceNew)
  , conferenceInvite = \friend g -> ask $ \t -> friendly t friend False (\f -> isRight <$> C.toxConferenceInvite t f (cConference g))
  , conferenceJoin = \friend cookie -> ask $ \t -> friendly t friend (Left JoinFriendNotConnected) $ \f ->
      bimap joinError conference <$> C.toxConferenceJoin t f cookie
  , conferenceDelete = \g -> ask $ \t -> isRight <$> C.toxConferenceDelete t (cConference g)
  , conferenceSendMessage = \g kind message -> ask $ \t ->
      isRight <$> C.toxConferenceSendMessage t (cConference g) (messageTypeC kind) message
  , conferenceSetTitle = \g title -> ask $ \t -> isRight <$> C.toxConferenceSetTitle t (cConference g) title
  , conferenceTitle = \g -> ask $ \t -> hush <$> C.toxConferenceGetTitle t (cConference g)
  , conferencePeers = \g -> ask $ \t -> C.toxConferencePeerCount t (cConference g) >>= \case
      Left _ -> pure []
      Right count -> catMaybes <$> mapM (conferencePeer t g) (takeWhile (< count) [0 ..])
  , conferenceId = \g -> ask $ \t -> fmap toBytes <$> C.toxConferenceGetId t (cConference g)
  , conferenceSave = \g -> ask $ \t -> C.toxConferenceGetId t (cConference g) >>= \case
      Nothing -> pure Nothing
      Just cid -> savedConference (toBytes cid) <$> C.toxGetSavedata t
  , conferenceRestore = \saved -> ask $ \t -> case decoded saved of
      Nothing -> pure Nothing
      Just c -> hush . fmap conference <$> C.toxConferenceById t (cKey (PC.conferenceId c))
  , groupNew = \private name nick -> ask $ \t -> hush . fmap group <$> C.toxGroupNew t (privacyC private) name nick
  , groupInviteFriend = \g friend -> ask $ \t -> friendly t friend False (fmap isRight . C.toxGroupInviteFriend t (cGroup g))
  , groupJoin = \chatId nick password -> ask $ \t -> joined t chatId $ \chat ->
      bimap groupJoinError group <$> C.toxGroupJoin t chat nick password
  , groupChatId = \g -> ask $ \t -> hush . fmap toBytes <$> C.toxGroupGetChatId t (cGroup g)
  , groupInviteAccept = \friend invite nick password -> ask $ \t -> joined t (BS.take chatIdSize invite) $ \_ ->
      friendly t friend (Left AcceptFailed) $ \f -> bimap acceptError group <$> C.toxGroupInviteAccept t f invite nick password
  , groupSendMessage = \g kind message -> ask $ \t ->
      bimap groupSendError (\(C.GroupMessageId i) -> i) <$> C.toxGroupSendMessage t (cGroup g) (messageTypeC kind) message
  , groupSendPrivateMessage = \g n kind message -> ask $ \t -> bimap privateSendError (\(C.GroupMessageId i) -> i)
      <$> C.toxGroupSendPrivateMessage t (cGroup g) (C.GroupPeerNumber n) (messageTypeC kind) message
  , groupSendCustomPacket = \g lossless packet -> ask $ \t ->
      first customSendError <$> C.toxGroupSendCustomPacket t (cGroup g) lossless packet
  , groupSendCustomPrivatePacket = \g n lossless packet -> ask $ \t ->
      first customPrivateSendError <$> C.toxGroupSendCustomPrivatePacket t (cGroup g) (C.GroupPeerNumber n) lossless packet
  , groupSetTopic = \g topic -> ask $ \t -> isRight <$> C.toxGroupSetTopic t (cGroup g) topic
  , groupMayTopic = \g -> ask $ \t -> do
      role <- C.toxGroupSelfGetRole t (cGroup g)
      lock <- C.toxGroupGetTopicLock t (cGroup g)
      pure . fromMaybe False $ mayTopic . (== C.GroupTopicLockEnabled) <$> hush lock <*> (roleOf <$> hush role)
  , groupSelfRole = \g -> ask $ \t -> hush . fmap roleOf <$> C.toxGroupSelfGetRole t (cGroup g)
  , groupSelfPublicKey = \g -> ask $ \t -> (fromC =<<) . hush <$> C.toxGroupSelfGetPublicKey t (cGroup g)
  , groupPeerCanSpeak = \g n -> ask $ \t -> do
      role <- C.toxGroupPeerGetRole t (cGroup g) (C.GroupPeerNumber n)
      voice <- C.toxGroupGetVoiceState t (cGroup g)
      pure . fromMaybe False $ canSpeak . voiceOf <$> hush voice <*> (roleOf <$> hush role)
  , groupSettings = \g -> ask $ \t -> do
      private <- C.toxGroupGetPrivacyState t (cGroup g)
      password <- C.toxGroupGetPassword t (cGroup g)
      limit <- C.toxGroupGetPeerLimit t (cGroup g)
      lock <- C.toxGroupGetTopicLock t (cGroup g)
      voice <- C.toxGroupGetVoiceState t (cGroup g)
      pure . hush $ GroupSettings <$> fmap (== C.GroupPrivacyStatePrivate) private <*> password <*> limit
        <*> fmap (== C.GroupTopicLockEnabled) lock <*> fmap voiceOf voice
  , groupTopic = \g -> ask $ \t -> hush <$> C.toxGroupGetTopic t (cGroup g)
  , groupName = \g -> ask $ \t -> hush <$> C.toxGroupGetName t (cGroup g)
  , groupSetNick = \g nick -> ask $ \t -> isRight <$> C.toxGroupSelfSetName t (cGroup g) nick
  , groupSetStatus = \g status -> ask $ \t -> isRight <$> C.toxGroupSelfSetStatus t (cGroup g) (userStatusC status)
  , groupPeers = \g -> ask $ \t -> do
      known <- Map.findWithDefault Set.empty g <$> readIORef (loopPeers loop)
      catMaybes <$> mapM (groupPeer t g) (Set.toList known)
  , groupLeave = \g part -> ask $ \t -> do
      modifyIORef' (loopPeers loop) (Map.delete g)
      isRight <$> C.toxGroupLeave t (cGroup g) part
  , groupSave = \g -> ask $ \t -> C.toxGroupGetChatId t (cGroup g) >>= \case
      Left _ -> pure Nothing
      Right chat -> savedGroup (toBytes chat) <$> C.toxGetSavedata t
  , groupRestore = \saved -> ask $ \t -> case decoded saved of
      Just (PG.Groups [g]) | Just chat <- fromBytes (chatIdOf g) -> hush . fmap group <$> C.toxGroupById t chat
      _ -> pure Nothing
  , groupKickPeer = \g n -> ask $ \t -> first kickError <$> C.toxGroupKickPeer t (cGroup g) (C.GroupPeerNumber n)
  , groupSetRole = \g n role -> ask $ \t -> first roleError <$> C.toxGroupSetRole t (cGroup g) (C.GroupPeerNumber n) (roleC role)
  , groupSetPassword = \g password -> ask $ \t -> isRight <$> C.toxGroupSetPassword t (cGroup g) password
  , groupSetPeerLimit = \g limit -> ask $ \t -> isRight <$> C.toxGroupSetPeerLimit t (cGroup g) limit
  , groupSetPrivacy = \g private -> ask $ \t -> isRight <$> C.toxGroupSetPrivacyState t (cGroup g) (privacyC private)
  , groupSetTopicLock = \g locked -> ask $ \t ->
      isRight <$> C.toxGroupSetTopicLock t (cGroup g) (if locked then C.GroupTopicLockEnabled else C.GroupTopicLockDisabled)
  , groupSetVoice = \g voice -> ask $ \t -> isRight <$> C.toxGroupSetVoiceState t (cGroup g) (voiceC voice)
  -- c-toxcore doesn't tell.
  , sizes = pure []
  , timings = pure Map.empty
  , traffic = pure []
  }
  where
    ask :: (C.Tox -> IO a) -> IO a
    ask = call loop
    tell = cast loop

-- | An event as hs-toxcore gives it: friends by key, not number.
translate :: C.Tox -> IORef Peers -> C.Event -> IO [Event]
translate t peers = \case
  C.SelfConnectionStatus e -> pure [SelfConnectionChanged (connection (C.selfConnectionStatus e))]
  C.FriendRequest e -> pure [FriendRequest pk (C.friendRequestMessage e) | Just pk <- [fromC (C.friendRequestPublicKey e)]]
  C.FriendConnectionStatus e ->
    friend (C.friendConnectionStatusFriendNumber e) (`FriendConnectionChanged` connection (C.friendConnectionStatus e))
  C.FriendLossyPacket e -> friend (C.friendLossyPacketFriendNumber e) (`FriendLossyPacket` C.friendLossyPacketData e)
  C.FriendLosslessPacket e -> friend (C.friendLosslessPacketFriendNumber e) (`FriendLosslessPacket` C.friendLosslessPacketData e)
  C.FriendName e -> friend (C.friendNameFriendNumber e) (`FriendName` C.friendName e)
  C.FriendStatus e -> friend (C.friendStatusFriendNumber e) (`FriendStatus` userStatusOf (C.friendStatus e))
  C.FriendStatusMessage e -> friend (C.friendStatusMessageFriendNumber e) (`FriendStatusMessage` C.friendStatusMessage e)
  C.FriendMessage e -> friend (C.friendMessageFriendNumber e) $ \pk ->
    FriendMessage pk (messageTypeOf (C.friendMessageType e)) (C.friendMessage e)
  C.FriendReadReceipt e -> friend (C.friendReadReceiptFriendNumber e) $ \pk ->
    let C.FriendMessageId i = C.friendReadReceiptMessageId e in FriendReadReceipt pk i
  C.FriendTyping e -> friend (C.friendTypingFriendNumber e) (`FriendTyping` C.friendTyping e)
  C.FileChunkRequest e -> friend (C.fileChunkRequestFriendNumber e) $ \pk ->
    FileChunkRequest pk (fileIndex (C.fileChunkRequestFileNumber e)) (C.fileChunkRequestPosition e) (C.fileChunkRequestLength e)
  -- c-toxcore gives the file's ID only when asked.
  C.FileRecv e -> do
    fileId <- either (const BS.empty) toBytes <$> C.toxFileGetFileId t (C.fileRecvFriendNumber e) (C.fileRecvFileNumber e)
    friend (C.fileRecvFriendNumber e) $ \pk -> FileReceive pk (fileIndex (C.fileRecvFileNumber e)) (C.fileRecvKind e)
      (C.fileRecvFileSize e) fileId (C.fileRecvFilename e)
  C.FileRecvChunk e -> friend (C.fileRecvChunkFriendNumber e) $ \pk ->
    FileReceiveChunk pk (fileIndex (C.fileRecvChunkFileNumber e)) (C.fileRecvChunkPosition e) (C.fileRecvChunkData e)
  C.FileRecvControl e -> friend (C.fileRecvControlFriendNumber e) $ \pk ->
    FileControlChanged pk (fileRef (C.fileRecvControlFileNumber e)) (fileControlOf (C.fileRecvControl e))
  C.ConferenceInvite e -> friend (C.conferenceInviteFriendNumber e) $ \pk ->
    ConferenceInvite pk (fromIntegral (fromEnum (C.conferenceInviteType e))) (C.conferenceInviteCookie e)
  C.ConferenceConnected e -> pure [ConferenceConnected (conference (C.conferenceConnectedConferenceNumber e))]
  C.ConferencePeerListChanged e -> pure [ConferencePeerListChanged (conference (C.conferencePeerListChangedConferenceNumber e))]
  C.ConferencePeerName e -> pure
    [ConferencePeerName (conference (C.conferencePeerNameConferenceNumber e)) (peer (C.conferencePeerNamePeerNumber e)) (C.conferencePeerName e)]
  -- The title we get while joining comes from no peer.
  C.ConferenceTitle e -> pure
    [ ConferenceTitle (conference (C.conferenceTitleConferenceNumber e))
        (if C.conferenceTitlePeerNumber e == C.ConferencePeerNumber maxBound then Nothing else Just (peer (C.conferenceTitlePeerNumber e)))
        (C.conferenceTitle e) ]
  C.ConferenceMessage e -> pure
    [ ConferenceMessage (conference (C.conferenceMessageConferenceNumber e)) (peer (C.conferenceMessagePeerNumber e))
        (messageTypeOf (C.conferenceMessageType e)) (C.conferenceMessage e) ]
  C.GroupPeerName e -> pure [GroupPeerName (group (C.groupPeerNameGroupNumber e)) (peerId (C.groupPeerNamePeerId e)) (C.groupPeerName e)]
  C.GroupPeerStatus e -> pure
    [GroupPeerStatus (group (C.groupPeerStatusGroupNumber e)) (peerId (C.groupPeerStatusPeerId e)) (fromIntegral (fromEnum (C.groupPeerStatus e)))]
  C.GroupTopic e -> others (C.groupTopicGroupNumber e) >>= \other ->
    pure [GroupTopic (group (C.groupTopicGroupNumber e)) (other (C.groupTopicPeerId e)) (C.groupTopic e)]
  C.GroupPrivacyState e -> pure [GroupPrivacy (group (C.groupPrivacyStateGroupNumber e)) (C.groupPrivacyState e == C.GroupPrivacyStatePrivate)]
  C.GroupVoiceState e -> pure [GroupVoiceState (group (C.groupVoiceStateGroupNumber e)) (voiceOf (C.groupVoiceState e))]
  C.GroupTopicLock e -> pure [GroupTopicLock (group (C.groupTopicLockGroupNumber e)) (C.groupTopicLock e == C.GroupTopicLockEnabled)]
  C.GroupPeerLimit e -> pure [GroupPeerLimit (group (C.groupPeerLimitGroupNumber e)) (fromIntegral (C.groupPeerLimit e))]
  C.GroupPassword e -> pure [GroupPassword (group (C.groupPasswordGroupNumber e)) (C.groupPassword e)]
  C.GroupMessage e -> pure
    [ GroupMessage (group (C.groupMessageGroupNumber e)) (peerId (C.groupMessagePeerId e)) (messageTypeOf (C.groupMessageMessageType e))
        (C.groupMessage e) (let C.GroupMessageId i = C.groupMessageMessageId e in i) ]
  C.GroupPrivateMessage e -> pure
    [ GroupPrivateMessage (group (C.groupPrivateMessageGroupNumber e)) (peerId (C.groupPrivateMessagePeerId e))
        (messageTypeOf (C.groupPrivateMessageMessageType e)) (C.groupPrivateMessage e)
        (let C.GroupMessageId i = C.groupPrivateMessageMessageId e in i) ]
  C.GroupCustomPacket e -> pure
    [GroupCustomPacket (group (C.groupCustomPacketGroupNumber e)) (peerId (C.groupCustomPacketPeerId e)) (C.groupCustomPacketData e)]
  C.GroupCustomPrivatePacket e -> pure
    [ GroupCustomPrivatePacket (group (C.groupCustomPrivatePacketGroupNumber e)) (peerId (C.groupCustomPrivatePacketPeerId e))
        (C.groupCustomPrivatePacketData e) ]
  C.GroupInvite e -> friend (C.groupInviteFriendNumber e) $ \pk -> GroupInvite pk (C.groupInviteInviteData e) (C.groupInviteGroupName e)
  C.GroupPeerJoin e -> do
    let g = group (C.groupPeerJoinGroupNumber e)
        n = peerId (C.groupPeerJoinPeerId e)
    modifyIORef' peers (Map.insertWith Set.union g (Set.singleton n))
    key <- C.toxGroupPeerGetPublicKey t (C.groupPeerJoinGroupNumber e) (C.groupPeerJoinPeerId e)
    pure [GroupPeerJoin g n pk | Just pk <- [fromC =<< hush key]]
  C.GroupPeerExit e -> do
    let g = group (C.groupPeerExitGroupNumber e)
        n = peerId (C.groupPeerExitPeerId e)
        how = exitOf (C.groupPeerExitExitType e)
    modifyIORef' peers (if how == ExitSelfDisconnected then Map.delete g else Map.adjust (Set.delete n) g)
    pure [GroupPeerExit g n how (C.groupPeerExitName e) (C.groupPeerExitPartMessage e)]
  C.GroupSelfJoin e -> pure [GroupSelfJoin (group (C.groupSelfJoinGroupNumber e))]
  C.GroupJoinFail e -> pure [GroupJoinFail (group (C.groupJoinFailGroupNumber e)) (joinFailOf (C.groupJoinFailFailType e))]
  C.GroupModeration e -> others (C.groupModerationGroupNumber e) >>= \other -> pure
    [ GroupModeration (group (C.groupModerationGroupNumber e)) (other (C.groupModerationSourcePeerId e))
        (other (C.groupModerationTargetPeerId e)) (moderationOf (C.groupModerationModType e)) ]
  C.DhtNodesResponse _ -> pure []
  where
    friend n f = either (const []) (maybe [] (pure . f) . fromC) <$> C.toxFriendGetPublicKey t n
    -- Our own peer ID as 'Nothing'.
    others g = (\self n -> if Right n == self then Nothing else Just (peerId n)) <$> C.toxGroupSelfGetPeerId t g

conferencePeer :: C.Tox -> ConferenceNumber -> Word32 -> IO (Maybe ConferencePeer)
conferencePeer t g n = do
  name <- C.toxConferencePeerGetName t (cConference g) (C.ConferencePeerNumber n)
  key <- C.toxConferencePeerGetPublicKey t (cConference g) (C.ConferencePeerNumber n)
  pure $ ConferencePeer (fromIntegral n) <$> (fromC =<< hush key) <*> hush name

groupPeer :: C.Tox -> GroupNumber -> PeerId -> IO (Maybe GroupPeer)
groupPeer t g n = do
  key <- C.toxGroupPeerGetPublicKey t (cGroup g) (C.GroupPeerNumber n)
  name <- C.toxGroupPeerGetName t (cGroup g) (C.GroupPeerNumber n)
  status <- C.toxGroupPeerGetStatus t (cGroup g) (C.GroupPeerNumber n)
  role <- C.toxGroupPeerGetRole t (cGroup g) (C.GroupPeerNumber n)
  pure $ GroupPeer n <$> (fromC =<< hush key) <*> hush name <*> (fromIntegral . fromEnum <$> hush status) <*> (roleOf <$> hush role)

-- | The conference with this ID in a save, as 'conferenceSave' gives it.
savedConference :: BS.ByteString -> BS.ByteString -> Maybe SavedConference
savedConference cid save =
  LBS.toStrict . Binary.encode <$> find ((== cid) . Sodium.encode . PC.conferenceId)
    [c | P.SectionConferences (PC.Conferences cs) <- sectionsOf save, c <- cs]

-- | The group with this chat ID in a save, as 'groupSave' gives it.
savedGroup :: BS.ByteString -> BS.ByteString -> Maybe SavedGroup
savedGroup chat save =
  LBS.toStrict . Binary.encode . PG.Groups . pure <$> find ((== chat) . chatIdOf)
    [g | P.SectionGroups (PG.Groups gs) <- sectionsOf save, g <- gs]

-- | Up to the last section: c-toxcore's save ends in zeros, as
-- tox_get_savedata_size counts room for relays and path nodes it may not have.
sectionsOf :: BS.ByteString -> [P.Section]
sectionsOf save = case Binary.decodeOrFail (LBS.fromStrict save) of
  Right (_, _, P.SaveData sections) -> sections
  Left _                            -> []

chatIdOf :: PG.Group -> BS.ByteString
chatIdOf = BS.takeEnd chatIdSize . Sodium.encode . PG.chatPublicKey . PG.groupKeys

-- | All of it, or nothing.
decoded :: Binary a => BS.ByteString -> Maybe a
decoded bytes = case Binary.decodeOrFail (LBS.fromStrict bytes) of
  Right (rest, _, a) | LBS.null rest -> Just a
  _                                  -> Nothing

-- | Gives the default to a key that isn't a friend's.
friendly :: C.Tox -> PublicKey -> a -> (C.FriendNumber -> IO a) -> IO a
friendly t pk none f = C.toxFriendByPublicKey t (cKey pk) >>= either (const (pure none)) f

-- | Refuses a group we're already in.
joined :: C.Tox -> BS.ByteString -> (C.GroupChatId -> IO (Either AcceptError GroupNumber)) -> IO (Either AcceptError GroupNumber)
joined t chatId f = case fromBytes chatId of
  Nothing -> pure (Left AcceptBadInvite)
  Just chat -> C.toxGroupById t chat >>= either (const (f chat)) (const (pure (Left AcceptAlreadyJoined)))

chatIdSize :: Int
chatIdSize = 32

atNode :: (String -> Word16 -> C.DhtId -> IO a) -> NodeInfo -> IO a
atNode f (NodeInfo _ (SocketAddress host (PortNumber port)) key) = f (Host.showAddress host) port (cKey key)

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

fromC :: FixedBytes k => k -> Maybe PublicKey
fromC = Sodium.decode . toBytes

-- | hs-toxcore's keys are always the size of C's.
cKey :: FixedBytes k => PublicKey -> k
cKey = unsafeFromBytes . Sodium.encode

-- | c-toxcore numbers the files we receive from 1 << 16 up.
fileRef :: C.FileNumber -> FileRef
fileRef (C.FileNumber n)
  | n >= 1 `shiftL` 16 = Incoming (fromIntegral ((n `shiftR` 16) - 1))
  | otherwise = Outgoing (fromIntegral n)

fileIndex :: C.FileNumber -> Word8
fileIndex n = case fileRef n of
  Incoming i -> i
  Outgoing i -> i

fileNumber :: FileRef -> C.FileNumber
fileNumber = C.FileNumber . \case
  Outgoing n -> fromIntegral n
  Incoming n -> (fromIntegral n + 1) `shiftL` 16

conference :: C.ConferenceNumber -> ConferenceNumber
conference (C.ConferenceNumber g) = fromIntegral g

cConference :: ConferenceNumber -> C.ConferenceNumber
cConference = C.ConferenceNumber . fromIntegral

peer :: C.ConferencePeerNumber -> PeerNumber
peer (C.ConferencePeerNumber n) = fromIntegral n

group :: C.GroupNumber -> GroupNumber
group (C.GroupNumber g) = g

cGroup :: GroupNumber -> C.GroupNumber
cGroup = C.GroupNumber

peerId :: C.GroupPeerNumber -> PeerId
peerId (C.GroupPeerNumber n) = n

-- | hs-toxcore's rules.
mayTopic :: Bool -> Role -> Bool
mayTopic locked role = role <= if locked then Moderator else User

canSpeak :: Voice -> Role -> Bool
canSpeak voice role = role /= Observer && case voice of
  VoiceAll        -> role <= User
  VoiceModerators -> role <= Moderator
  VoiceFounder    -> role == Founder

connection :: C.Connection -> ConnectionStatus
connection = \case
  C.ConnectionNone -> NotConnected
  C.ConnectionTcp  -> ConnectedTcp
  C.ConnectionUdp  -> ConnectedUdp

userStatusOf :: C.UserStatus -> UserStatus
userStatusOf = \case
  C.UserStatusNone -> Online
  C.UserStatusAway -> Away
  C.UserStatusBusy -> Busy

userStatusC :: UserStatus -> C.UserStatus
userStatusC = \case
  Online -> C.UserStatusNone
  Away   -> C.UserStatusAway
  Busy   -> C.UserStatusBusy

messageTypeOf :: C.MessageType -> MessageType
messageTypeOf = \case
  C.MessageTypeNormal -> MessageNormal
  C.MessageTypeAction -> MessageAction

messageTypeC :: MessageType -> C.MessageType
messageTypeC = \case
  MessageNormal -> C.MessageTypeNormal
  MessageAction -> C.MessageTypeAction

fileControlOf :: C.FileControl -> FileControl
fileControlOf = \case
  C.FileControlResume -> FileResume
  C.FileControlPause  -> FilePause
  C.FileControlCancel -> FileCancel

fileControlC :: FileControl -> C.FileControl
fileControlC = \case
  FileResume -> C.FileControlResume
  FilePause  -> C.FileControlPause
  FileCancel -> C.FileControlCancel

roleOf :: C.GroupRole -> Role
roleOf = \case
  C.GroupRoleFounder   -> Founder
  C.GroupRoleModerator -> Moderator
  C.GroupRoleUser      -> User
  C.GroupRoleObserver  -> Observer

roleC :: Role -> C.GroupRole
roleC = \case
  Founder   -> C.GroupRoleFounder
  Moderator -> C.GroupRoleModerator
  User      -> C.GroupRoleUser
  Observer  -> C.GroupRoleObserver

voiceOf :: C.GroupVoiceState -> Voice
voiceOf = \case
  C.GroupVoiceStateAll       -> VoiceAll
  C.GroupVoiceStateModerator -> VoiceModerators
  C.GroupVoiceStateFounder   -> VoiceFounder

voiceC :: Voice -> C.GroupVoiceState
voiceC = \case
  VoiceAll        -> C.GroupVoiceStateAll
  VoiceModerators -> C.GroupVoiceStateModerator
  VoiceFounder    -> C.GroupVoiceStateFounder

privacyC :: Bool -> C.GroupPrivacyState
privacyC private = if private then C.GroupPrivacyStatePrivate else C.GroupPrivacyStatePublic

exitOf :: C.GroupExitType -> ExitType
exitOf = \case
  C.GroupExitTypeQuit             -> ExitQuit
  C.GroupExitTypeTimeout          -> ExitTimeout
  C.GroupExitTypeDisconnected     -> ExitDisconnected
  C.GroupExitTypeSelfDisconnected -> ExitSelfDisconnected
  C.GroupExitTypeKick             -> ExitKicked
  C.GroupExitTypeSyncError        -> ExitSyncError

joinFailOf :: C.GroupJoinFail -> JoinFail
joinFailOf = \case
  C.GroupJoinFailPeerLimit       -> JoinFailFull
  C.GroupJoinFailInvalidPassword -> JoinFailBadPassword
  C.GroupJoinFailUnknown         -> JoinFailUnknown

moderationOf :: C.GroupModEvent -> Moderation
moderationOf = \case
  C.GroupModEventKick      -> ModKick
  C.GroupModEventObserver  -> ModObserver
  C.GroupModEventUser      -> ModUser
  C.GroupModEventModerator -> ModModerator

-- | 'Nothing' for an address that isn't one.
added :: C.ErrFriendAdd -> Maybe AddFriendError
added = \case
  C.ErrFriendAddTooLong      -> Just MessageTooLong
  C.ErrFriendAddNoMessage    -> Just MessageEmpty
  C.ErrFriendAddOwnKey       -> Just OwnKey
  C.ErrFriendAddAlreadySent  -> Just AlreadyFriend
  C.ErrFriendAddSetNewNospam -> Just AlreadyFriend
  C.ErrFriendAddBadChecksum  -> Nothing
  C.ErrFriendAddNull         -> Nothing
  C.ErrFriendAddMalloc       -> Nothing

noRequest :: C.ErrFriendAdd -> AddFriendError
noRequest = \case
  C.ErrFriendAddOwnKey -> OwnKey
  _                    -> AlreadyFriend

sendError :: C.ErrFriendSendMessage -> SendMessageError
sendError = \case
  C.ErrFriendSendMessageFriendNotFound     -> SendFriendNotFound
  C.ErrFriendSendMessageFriendNotConnected -> SendFriendNotConnected
  C.ErrFriendSendMessageSendq              -> SendQueueFull
  C.ErrFriendSendMessageTooLong            -> SendTooLong
  C.ErrFriendSendMessageEmpty              -> SendEmpty
  C.ErrFriendSendMessageNull               -> SendEmpty

packetError :: C.ErrFriendCustomPacket -> CustomPacketError
packetError = \case
  C.ErrFriendCustomPacketFriendNotFound     -> CustomFriendNotFound
  C.ErrFriendCustomPacketFriendNotConnected -> CustomFriendNotConnected
  C.ErrFriendCustomPacketInvalid            -> CustomInvalid
  C.ErrFriendCustomPacketEmpty              -> CustomEmpty
  C.ErrFriendCustomPacketNull               -> CustomEmpty
  C.ErrFriendCustomPacketTooLong            -> CustomTooLong
  C.ErrFriendCustomPacketSendq              -> CustomSendQueueFull

fileSendError :: C.ErrFileSend -> FileError
fileSendError = \case
  C.ErrFileSendFriendNotFound     -> FileFriendNotFound
  C.ErrFileSendFriendNotConnected -> FileFriendNotConnected
  C.ErrFileSendNameTooLong        -> FileNameTooLong
  C.ErrFileSendTooMany            -> FileTooMany
  C.ErrFileSendNull               -> FileNameTooLong

fileControlError :: C.ErrFileControl -> FileError
fileControlError = \case
  C.ErrFileControlFriendNotFound     -> FileFriendNotFound
  C.ErrFileControlFriendNotConnected -> FileFriendNotConnected
  C.ErrFileControlNotFound           -> FileNotFound
  C.ErrFileControlSendq              -> FileSendQueueFull
  C.ErrFileControlNotPaused          -> FileBadState
  C.ErrFileControlDenied             -> FileBadState
  C.ErrFileControlAlreadyPaused      -> FileBadState

fileSeekError :: C.ErrFileSeek -> FileError
fileSeekError = \case
  C.ErrFileSeekFriendNotFound     -> FileFriendNotFound
  C.ErrFileSeekFriendNotConnected -> FileFriendNotConnected
  C.ErrFileSeekNotFound           -> FileNotFound
  C.ErrFileSeekSendq              -> FileSendQueueFull
  C.ErrFileSeekDenied             -> FileBadState
  C.ErrFileSeekInvalidPosition    -> FileBadChunk

fileChunkError :: C.ErrFileSendChunk -> FileError
fileChunkError = \case
  C.ErrFileSendChunkFriendNotFound     -> FileFriendNotFound
  C.ErrFileSendChunkFriendNotConnected -> FileFriendNotConnected
  C.ErrFileSendChunkNotFound           -> FileNotFound
  C.ErrFileSendChunkSendq              -> FileSendQueueFull
  C.ErrFileSendChunkNotTransferring    -> FileBadState
  C.ErrFileSendChunkInvalidLength      -> FileBadChunk
  C.ErrFileSendChunkWrongPosition      -> FileBadChunk
  C.ErrFileSendChunkNull               -> FileBadChunk

joinError :: C.ErrConferenceJoin -> JoinError
joinError = \case
  C.ErrConferenceJoinInvalidLength   -> JoinBadCookie
  C.ErrConferenceJoinNull            -> JoinBadCookie
  C.ErrConferenceJoinWrongType       -> JoinWrongType
  C.ErrConferenceJoinFriendNotFound  -> JoinFriendNotConnected
  C.ErrConferenceJoinFailSend        -> JoinFriendNotConnected
  C.ErrConferenceJoinDuplicate       -> JoinAlreadyJoined
  C.ErrConferenceJoinInitFail        -> JoinTooMany

groupJoinError :: C.ErrGroupJoin -> AcceptError
groupJoinError = \case
  C.ErrGroupJoinBadChatId -> AcceptBadInvite
  C.ErrGroupJoinEmpty     -> AcceptBadNick
  C.ErrGroupJoinTooLong   -> AcceptBadNick
  C.ErrGroupJoinInit      -> AcceptFailed
  C.ErrGroupJoinPassword  -> AcceptFailed
  C.ErrGroupJoinCore      -> AcceptFailed

acceptError :: C.ErrGroupInviteAccept -> AcceptError
acceptError = \case
  C.ErrGroupInviteAcceptBadInvite       -> AcceptBadInvite
  C.ErrGroupInviteAcceptNull            -> AcceptBadInvite
  C.ErrGroupInviteAcceptTooLong         -> AcceptBadNick
  C.ErrGroupInviteAcceptEmpty           -> AcceptBadNick
  C.ErrGroupInviteAcceptInitFailed      -> AcceptFailed
  C.ErrGroupInviteAcceptPassword        -> AcceptFailed
  C.ErrGroupInviteAcceptFriendNotFound  -> AcceptFailed
  C.ErrGroupInviteAcceptFailSend        -> AcceptFailed

groupSendError :: C.ErrGroupSendMessage -> GroupSendError
groupSendError = \case
  C.ErrGroupSendMessageGroupNotFound -> GroupSendNoGroup
  C.ErrGroupSendMessageTooLong       -> GroupSendTooLong
  C.ErrGroupSendMessageEmpty         -> GroupSendEmpty
  C.ErrGroupSendMessageBadType       -> GroupSendEmpty
  C.ErrGroupSendMessagePermissions   -> GroupSendNoVoice
  C.ErrGroupSendMessageFailSend      -> GroupSendNotConnected
  C.ErrGroupSendMessageDisconnected  -> GroupSendNotConnected

privateSendError :: C.ErrGroupSendPrivateMessage -> GroupSendError
privateSendError = \case
  C.ErrGroupSendPrivateMessageGroupNotFound -> GroupSendNoGroup
  C.ErrGroupSendPrivateMessagePeerNotFound  -> GroupSendNoPeer
  C.ErrGroupSendPrivateMessageTooLong       -> GroupSendTooLong
  C.ErrGroupSendPrivateMessageEmpty         -> GroupSendEmpty
  C.ErrGroupSendPrivateMessageBadType       -> GroupSendEmpty
  C.ErrGroupSendPrivateMessagePermissions   -> GroupSendNoVoice
  C.ErrGroupSendPrivateMessageFailSend      -> GroupSendNotConnected
  C.ErrGroupSendPrivateMessageDisconnected  -> GroupSendNotConnected

customSendError :: C.ErrGroupSendCustomPacket -> GroupSendError
customSendError = \case
  C.ErrGroupSendCustomPacketGroupNotFound -> GroupSendNoGroup
  C.ErrGroupSendCustomPacketTooLong       -> GroupSendTooLong
  C.ErrGroupSendCustomPacketEmpty         -> GroupSendEmpty
  C.ErrGroupSendCustomPacketDisconnected  -> GroupSendNotConnected
  C.ErrGroupSendCustomPacketFailSend      -> GroupSendNotConnected

customPrivateSendError :: C.ErrGroupSendCustomPrivatePacket -> GroupSendError
customPrivateSendError = \case
  C.ErrGroupSendCustomPrivatePacketGroupNotFound -> GroupSendNoGroup
  C.ErrGroupSendCustomPrivatePacketTooLong       -> GroupSendTooLong
  C.ErrGroupSendCustomPrivatePacketEmpty         -> GroupSendEmpty
  C.ErrGroupSendCustomPrivatePacketPeerNotFound  -> GroupSendNoPeer
  C.ErrGroupSendCustomPrivatePacketFailSend      -> GroupSendNotConnected
  C.ErrGroupSendCustomPrivatePacketDisconnected  -> GroupSendNotConnected

kickError :: C.ErrGroupKickPeer -> ModError
kickError = \case
  C.ErrGroupKickPeerGroupNotFound -> ModNoGroup
  C.ErrGroupKickPeerPeerNotFound  -> ModNoPeer
  C.ErrGroupKickPeerPermissions   -> ModNotAllowed
  C.ErrGroupKickPeerSelf          -> ModNotAllowed
  C.ErrGroupKickPeerFailAction    -> ModFailed
  C.ErrGroupKickPeerFailSend      -> ModFailed

roleError :: C.ErrGroupSetRole -> ModError
roleError = \case
  C.ErrGroupSetRoleGroupNotFound -> ModNoGroup
  C.ErrGroupSetRolePeerNotFound  -> ModNoPeer
  C.ErrGroupSetRolePermissions   -> ModNotAllowed
  C.ErrGroupSetRoleSelf          -> ModNotAllowed
  C.ErrGroupSetRoleAssignment    -> ModNoChange
  C.ErrGroupSetRoleFailAction    -> ModFailed
