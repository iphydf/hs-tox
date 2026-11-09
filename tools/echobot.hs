{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
-- | A Tox bot on hs-toxcore and hs-toxav that sends friends back what they
-- send it: messages, files, and the audio and video of their calls, also in
-- groups they invite it to. Clients can test against it, also with `!`
-- commands for what a friend can't send itself: calls it places, files,
-- bursts of messages and more. rs-toxcore-c's echobot does the same on
-- c-toxcore.
module Main (main) where

import           Control.Concurrent      (Chan, ThreadId, forkIO, killThread,
                                          newChan, readChan, threadDelay,
                                          writeChan)
import           Control.Exception       (AsyncException (..), IOException,
                                          SomeAsyncException, fromException,
                                          handleJust, try, tryJust)
import           Control.Monad           (forM_, forever, guard, mfilter,
                                          unless, void, when, (>=>))
import qualified Crypto.Saltine.Class    as Sodium
import           Data.Bool               (bool)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as Base16
import qualified Data.ByteString.Char8   as Char8
import qualified Data.ByteString.Lazy    as LBS
import           Data.Char               (isDigit, toLower)
import           Data.Functor            ((<&>))
import           Data.IORef              (IORef, atomicModifyIORef', newIORef,
                                          readIORef)
import           Data.List               (find, foldl')
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (fromMaybe, isNothing, mapMaybe)
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           Data.Time               (NominalDiffTime, UTCTime, addUTCTime,
                                          defaultTimeLocale, diffUTCTime,
                                          formatTime, getCurrentTime)
import           Data.Word               (Word32, Word64, Word8)
import           Network.HTTP.Client     (HttpException, httpLbs, parseRequest,
                                          responseBody)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Options.Applicative
import           System.Directory        (createDirectoryIfMissing,
                                          doesDirectoryExist, doesFileExist,
                                          getFileSize, listDirectory,
                                          removeDirectoryRecursive, removeFile,
                                          renameFile)
import           System.Environment      (lookupEnv)
import           System.FilePath         ((-<.>), (<.>), (</>))
import           System.IO               (BufferMode (..), Handle, IOMode (..),
                                          SeekMode (..), hClose, hFileSize,
                                          hSeek, hSetBuffering, hSetFileSize,
                                          openBinaryFile, stdout,
                                          withBinaryFile)
import           Text.Read               (readMaybe)

import           Avatar                  (avatar)
import qualified Tox.Application.Tox     as Tox
import qualified Tox.AV                  as AV
import           Tox.AV                  (BitRates (..), Media (..))
import qualified Tox.Chat.GroupChat      as Group
import qualified Tox.Client.Debug        as Debug
import           Tox.Client.Nodes        (Node (..), bootstrapFrom, builtIn,
                                          nodesUrl, readNodes, usable)
import           Tox.Core.Log            (LogLevel (..))
import           Tox.Crypto.Core.Key     (PublicKey)
import           Tox.Crypto.Core.Sign    (sha256)
import qualified Tox.Session.Friend      as Messenger

data Args = Args
  { argSavefile      :: Maybe FilePath
  , argNick          :: Text
  , argStatusMessage :: Text
  , argNodesUrl      :: String
  , argNodes         :: [String]
  , argSnapshots     :: Maybe FilePath
  }

args :: Parser Args
args = Args
  <$> optional (strOption (long "savefile" <> short 's' <> metavar "FILE"))
  <*> strOption (long "nick" <> value "echobot-hs" <> showDefault)
  <*> strOption (long "status-message" <> value "Echoes messages and calls. Send !help for more" <> showDefault)
  <*> strOption (long "nodes-url" <> value (Text.unpack nodesUrl) <> showDefault)
  <*> many (strOption (long "bootstrap-node" <> metavar "HOST:PORT:HEXPK"
        <> help "Bootstrap and TCP relay node, in place of the --nodes-url list. Repeatable."))
  <*> optional (strOption (long "snapshots" <> metavar "DIR"
        <> help "Where to keep each friend's latest picture as a greyscale PGM, every 10 seconds."))

-- | Kilobits per second.
audioRate, videoRate :: Int
audioRate = 48
videoRate = 5000

-- | How long a friend waits between calls they ask for, and the longest they
-- can have one or a message wait, say to lock their phone first.
callCooldown, maxDelay :: Int
callCooldown = 10
maxDelay = 120

-- | How long the bot shows typing before it echoes a message.
typingTime :: NominalDiffTime
typingTime = 1

-- | How long the bot remembers what it said in a group, not to echo it back,
-- so that two bots in a group don't echo each other forever.
echoMemory :: NominalDiffTime
echoMemory = 10

maxBurst, longLines :: Int
maxBurst = 100
longLines = 100

-- | TOX_CONFERENCE_TYPE_TEXT.
conferenceText :: Word8
conferenceText = 0

mib :: Word64
mib = 1024 * 1024

-- | The largest file the bot takes or makes, and the most it holds at once.
maxFileSize, maxHeld :: Word64
maxFileSize = 100 * mib
maxHeld = 1024 * mib

-- | Microseconds between tries to send a chunk toxcore had no room for.
queueWait :: Int
queueWait = 10000

saveInterval, statsInterval :: Int
saveInterval = 600
statsInterval = 10

-- | A `!` command, by any of its names in any case: what it takes and does,
-- for the help, and what it does with the request, 'Nothing' if its words
-- don't fit.
data Command = Command
  { commandNames :: [String]
  , commandArgs  :: BS.ByteString
  , commandDoes  :: BS.ByteString
  , commandRun   :: Bot -> PublicKey -> Request -> Maybe (IO [BS.ByteString])
  }

-- | A `!` command as a friend sent it: its name, the words after it, and
-- what follows the name as typed.
data Request = Request
  { requestName  :: String
  , requestWords :: [String]
  , requestRest  :: BS.ByteString
  }

commands :: [Command]
commands =
  [ Command ["call", "video"] " [seconds]" ("I call you, after that many seconds if given (up to " <> shown maxDelay <> ")") $ \bot pk r ->
      fmap pure . call bot pk (requestName r == "video") <$> case requestWords r of
        []        -> Just 0
        [seconds] -> within 0 maxDelay seconds
        _         -> Nothing
  , Command ["hangup"] "" "I end our call" $ \bot pk -> alone (pure <$> hangUp bot pk)
  , Command ["file"] " <size>" ("I send you a file of random bytes, as !file 20M (up to " <> sizeText maxFileSize <> ")") $ \bot pk r ->
      case requestWords r of
        [size] -> parseSize size >>= \bytes ->
          pure . either (("Couldn't make the file: " <>) . shown) id <$> try @IOException (makeFile bot pk bytes)
            <$ guard (bytes <= maxFileSize)
        _ -> Nothing
  , Command ["long"] "" "a message too long for one, in parts" $ \_ _ -> alone (pure longMessage)
  , Command ["burst"] " <count>" ("that many messages at once (up to " <> shown maxBurst <> ")") $ \_ _ r ->
      case requestWords r of
        [count] -> (\n -> pure [shown i <> " of " <> shown n | i <- [1 .. n]]) <$> within 1 maxBurst count
        _       -> Nothing
  , Command ["later"] " <seconds> <text>" ("I send you the text after that many seconds (up to " <> shown maxDelay <> ")") $ \bot pk r ->
      case requestWords r of
        seconds : _ : _ -> later bot pk (Char8.dropWhile spaced . Char8.dropWhile (not . spaced) $ requestRest r)
          <$> within 0 maxDelay seconds
        _ -> Nothing
  , Command ["away", "busy", "online"] "" "I change my status" $ \bot _ r -> flip alone r $
      ["I'm " <> Char8.pack (requestName r) <> " now"] <$ Tox.selfSetStatus (botTox bot) (case requestName r of
        "away" -> Tox.Away
        "busy" -> Tox.Busy
        _      -> Tox.Online)
  , Command ["group", "conference"] "" "I make one and invite you" $ \bot pk r -> alone (pure <$> invite bot pk (requestName r)) r
  ]
  where
    -- For commands that take no words.
    alone reply r = reply <$ guard (null (requestWords r))
    within from to = mfilter (\n -> n >= from && n <= to) . readMaybe

-- | The commands, and what the bot does with anything else.
usage :: BS.ByteString
usage = Char8.intercalate "\n" $ map line commands ++ ["Anything else I send back, files too, also in groups."]
  where
    line c = Char8.intercalate ", " [Char8.pack ('!' : name) <> commandArgs c | name <- commandNames c] <> ": " <> commandDoes c

full :: BS.ByteString
full = "I hold too many files now, try later"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  opts <- execParser (info (args <**> helper) (progDesc "Echoes messages, files and calls, on hs-toxcore."))
  savefile <- case argSavefile opts of
    Just path -> pure path
    Nothing -> lookupEnv "BUILD_WORKSPACE_DIRECTORY" >>= maybe
      (fail "--savefile is needed outside `bazel run`")
      (pure . (</> "hs-tox/tools/echobot.tox"))
  -- The last run's files: their transfers are gone.
  let dir = savefile -<.> "files"
  doesDirectoryExist dir >>= flip when (removeDirectoryRecursive dir)
  createDirectoryIfMissing True dir
  saved <- doesFileExist savefile >>= \case
    True  -> Tox.ToxSave <$> BS.readFile savefile
    False -> pure Tox.NoSaveData
  tox <- Tox.new Tox.defaultOptions
    { Tox.optSaveData = saved
    , Tox.optLocalDiscovery = False
    , Tox.optLogger = \level line -> when (level >= LevelWarn) (putStrLn (Text.unpack line))
    } >>= either (fail . show) pure
  -- The arguments win over the save, so that renaming the bot renames it,
  -- and it starts online.
  void . Tox.selfSetName tox . Text.encodeUtf8 $ argNick opts
  void . Tox.selfSetStatusMessage tox . Text.encodeUtf8 $ argStatusMessage opts
  Tox.selfSetStatus tox Tox.Online
  save tox savefile
  Tox.selfAddress tox >>= putStrLn . ("Bot ID: " <>) . Char8.unpack . Base16.encode
  nodes opts >>= mapM_ (bootstrapFrom tox) . take 8 . usable

  av <- AV.new AV.Transport
    { AV.sendLossless = Tox.friendQueueLosslessPacket tox
    , AV.sendLossy = Tox.friendQueueLossyPacket tox
    }
  asked <- Asked <$> newIORef Map.empty <*> newIORef Map.empty
  bot <- Bot tox av savefile (Text.encodeUtf8 (argNick opts)) dir asked
    <$> newChan <*> newChan <*> newIORef Map.empty <*> newIORef Set.empty <*> newIORef (Files Map.empty Map.empty Map.empty Map.empty)
    <*> newIORef Map.empty <*> newIORef Map.empty <*> newIORef Set.empty
  -- Each friend's latest picture, while they send video.
  videos <- newIORef Map.empty
  void . forkIO . forever $ AV.nextEvent av >>= \event -> do
    atomicModifyIORef' videos $ \showing -> (, ()) $ case event of
      AV.VideoReceived pk _ picture -> Map.insert pk picture showing
      AV.Ended pk _                 -> Map.delete pk showing
      _                             -> showing
    logFailure (echoCall av event)
  -- How each call's video comes, as hTox's debug line tells it.
  void . forkIO . forever $ do
    threadDelay (statsInterval * 1000000)
    now <- getCurrentTime
    readIORef videos >>= mapM_ (\(pk, picture) -> do
      AV.videoStats av pk >>= mapM_ (putStrLn . ((hex pk <> "'s ") <>) . Text.unpack . Debug.videoText)
      AV.videoEncoderName av pk >>= mapM_ (putStrLn . (("Sending " <> hex pk <> " video with ") <>))
      forM_ (argSnapshots opts) $ \snapshots ->
        snapshot (snapshots </> take 8 (hex pk) <> "-" <> formatTime defaultTimeLocale "%H%M%S" now <> ".pgm") picture)
      . Map.toList
  void . forkIO . forever $ threadDelay (saveInterval * 1000000) >> save tox savefile
  void . forkIO . forever $ readChan (botEchoes bot) >>= echo bot
  void . forkIO . forever $ readChan (botChunks bot) >>= sendChunk tox
  -- Until Tox stops, as the bot shuts down.
  void . forkIO . handleJust (\Tox.ToxStopped -> Just ()) pure . forever $
    Tox.nextEvent tox >>= logFailure . handle bot
  -- Until Ctrl-C, which saves first.
  handleJust (\case UserInterrupt -> Just (); _ -> Nothing) pure . forever $ threadDelay 1000000000
  putStrLn "Shutting down"
  save tox savefile
  Tox.kill tox

-- | The picture's brightness, which shows what went wrong with one.
snapshot :: FilePath -> AV.Picture -> IO ()
snapshot path (AV.Picture w h y _ _) =
  BS.writeFile path (Char8.pack ("P5\n" <> show w <> " " <> show h <> "\n255\n") <> y)

-- | Logs what goes wrong with an event, so that it can't stop the bot.
logFailure :: IO () -> IO ()
logFailure = tryJust synchronous >=> either (putStrLn . ("Failed to handle an event: " <>) . show) pure
  where
    -- Not those that stop a thread, as Ctrl-C does.
    synchronous e = e <$ guard (isNothing (fromException @SomeAsyncException e))

-- | Written to a new file first, so that a crash can't leave half a save.
save :: Tox.Tox -> FilePath -> IO ()
save tox path = do
  bytes <- Tox.saveData tox
  BS.writeFile (path <> ".new") bytes
  renameFile (path <> ".new") path

-- | The nodes given, or else the list's now, or the built-in ones if it
-- can't be read.
nodes :: Args -> IO [Node]
nodes opts
  | null (argNodes opts) = fetched >>= maybe (builtIn <$ putStrLn ("No usable nodes from " <> url <> ", using the built-in list")) pure
  | otherwise = pure . mapMaybe node $ argNodes opts
  where
    url = argNodesUrl opts
    fetched = try @HttpException (parseRequest url >>= \request -> newTlsManager >>= httpLbs request)
      >>= either (\e -> Nothing <$ putStrLn ("Failed to fetch nodes from " <> url <> ": " <> show e))
                 (pure . readNodes . LBS.toStrict . responseBody)
    node spec = case Text.splitOn ":" (Text.pack spec) of
      [host, port, key] | Just p <- readMaybe (Text.unpack port), Right k <- Base16.decode (Text.encodeUtf8 key) ->
        Just (Node (Text.unpack host) p (Just p) k True)
      _ -> Nothing

-- | When each friend last asked to be called, and the calls asked for with a
-- delay, until they're placed.
data Asked = Asked
  { askedAt      :: IORef (Map.Map PublicKey UTCTime)
  , askedWaiting :: IORef (Map.Map PublicKey ThreadId)
  }

-- | A piece of a file toxcore asked for: the friend, the file number, and
-- where in the file it goes.
data Chunk = Chunk PublicKey Word8 Word64 BS.ByteString

-- | An echo to send when it's due, shown as typed until then.
data Due = Due UTCTime PublicKey Tox.MessageType BS.ByteString

-- | A file a friend sends the bot, written to disk as it comes. A friend
-- sending it again with the same ID, as after going offline, resumes it.
data Receiving = Receiving
  { receivingPath   :: FilePath
  , receivingHandle :: Handle
  , receivingName   :: BS.ByteString
  }

-- | A file the bot sends a friend, deleted once sent. If the friend goes
-- offline first, it's offered again with the same ID when they're back, so
-- that they can resume it.
data Offer = Offer
  { offerPath :: FilePath
  , offerId   :: BS.ByteString
  , offerName :: BS.ByteString
  , offerSize :: Word64
  }

-- | What the bot sends under a file number.
data Sending = SendingAvatar | SendingFile Offer Handle

data Files = Files
  { filesReceiving :: Map.Map (PublicKey, Word8) Receiving
  , filesSending   :: Map.Map (PublicKey, Word8) Sending
  , filesEnded     :: Map.Map (PublicKey, Word8) Int
    -- ^ How many ends toxcore reported for transfers under each number after
    -- giving it to a new one: it frees a number as the transfer ends, before
    -- the bot hears of the end.
  , filesUnsent    :: Map.Map PublicKey [Offer]
    -- ^ Files whose sending stopped as their friend went offline.
  }

data Chat = InConference Tox.ConferenceNumber | InGroup Group.GroupNumber
  deriving (Eq, Ord)

data Bot = Bot
  { botTox      :: Tox.Tox
  , botAv       :: AV.ToxAV PublicKey
  , botSavefile :: FilePath
  , botNick     :: BS.ByteString
  , botDir      :: FilePath -- ^ Where the files the bot takes and sends are.
  , botAsked    :: Asked
  , botEchoes   :: Chan Due
  , botChunks   :: Chan Chunk -- ^ In the order toxcore asked for them.
  , botTyping   :: IORef (Map.Map PublicKey Int) -- ^ How many echoes each friend has coming.
  , botOnline   :: IORef (Set.Set PublicKey)
  , botFiles    :: IORef Files
  , botSaid     :: IORef (Map.Map Chat [(UTCTime, BS.ByteString)])
    -- ^ What the bot said in each group lately, with when.
  , botPeers    :: IORef (Map.Map Group.GroupNumber (Set.Set Group.PeerId))
    -- ^ Who else is in each group, to leave it when they've all gone.
  , botMet      :: IORef (Set.Set Tox.ConferenceNumber)
    -- ^ The conferences where the bot has seen others.
  }

-- | Accepts friend requests and invites, echoes messages and files, does `!`
-- commands, and passes calls' packets to ToxAV.
handle :: Bot -> Tox.Event -> IO ()
handle bot = \case
  Tox.FriendRequest pk _ -> do
    putStrLn $ "Accepting " <> hex pk <> "'s friend request"
    Tox.friendAddNoRequest tox pk >>= either (putStrLn . ("Failed to accept: " <>) . show) (const saved)
  Tox.FriendMessage pk ty message -> case Char8.uncons (Char8.dropWhile (== ' ') message) of
    Just ('!', request) -> do
      replies <- run bot pk request
      putStrLn $ hex pk <> " sent " <> show message <> ": " <> foldMap (show . Char8.takeWhile (/= '\n')) (take 1 replies)
      mapM_ (Tox.friendSendMessage tox pk Tox.MessageNormal) replies
    _ -> do
      void $ Tox.friendSetTyping tox pk True
      atomicModifyIORef' (botTyping bot) ((, ()) . Map.insertWith (+) pk 1)
      at <- addUTCTime typingTime <$> getCurrentTime
      writeChan (botEchoes bot) (Due at pk ty message)
  Tox.FriendConnectionChanged pk status -> do
    AV.friendConnection (botAv bot) pk (status /= Tox.NotConnected)
    connection bot pk (status /= Tox.NotConnected)
  Tox.FriendLosslessPacket pk packet -> forward AV.losslessIds pk packet
  Tox.FriendLossyPacket pk packet -> forward AV.lossyIds pk packet
  Tox.FileReceive pk n kind size fileId name -> receive bot pk n kind size fileId name
  Tox.FileReceiveChunk pk n position bytes -> received bot pk n position bytes
  Tox.FileChunkRequest pk n position len -> requested bot pk n position len
  Tox.FileControlChanged pk ref Tox.FileCancel -> cancelled bot pk ref
  Tox.ConferenceInvite pk ty cookie -> when (ty == conferenceText) $ do
    putStrLn $ "Joining " <> hex pk <> "'s conference"
    Tox.conferenceJoin tox pk cookie >>= either (putStrLn . ("Failed to join: " <>) . show) (const saved)
  Tox.ConferenceMessage g peer ty message -> do
    ours <- any (\p -> Tox.conferencePeerNumber p == peer && Tox.conferencePeerPublicKey p == Tox.selfPublicKey tox)
      <$> Tox.conferencePeers tox g
    unless ours $ echoes bot (InConference g) message >>= flip when (void $ Tox.conferenceSendMessage tox g ty message)
  Tox.ConferencePeerListChanged g -> do
    others <- (> 1) . length <$> Tox.conferencePeers tox g
    met <- atomicModifyIORef' (botMet bot) $ \m -> (bool Set.delete Set.insert others g m, Set.member g m)
    when (met && not others) $ do
      putStrLn $ "Leaving conference " <> show g <> ", as everyone else left"
      atomicModifyIORef' (botSaid bot) ((, ()) . Map.delete (InConference g))
      void $ Tox.conferenceDelete tox g
      saved
  Tox.GroupInvite pk inviteData _ -> do
    putStrLn $ "Joining " <> hex pk <> "'s group"
    Tox.groupInviteAccept tox pk inviteData (botNick bot) "" >>= either (putStrLn . ("Failed to join: " <>) . show) (const saved)
  Tox.GroupMessage g _ ty message _ ->
    echoes bot (InGroup g) message >>= flip when (void $ Tox.groupSendMessage tox g ty message)
  Tox.GroupPeerJoin g peer _ -> atomicModifyIORef' (botPeers bot) ((, ()) . Map.insertWith Set.union g (Set.singleton peer))
  Tox.GroupPeerExit g peer exit _ _ -> do
    alone <- atomicModifyIORef' (botPeers bot) $ \m ->
      let others = Set.delete peer (Map.findWithDefault Set.empty g m)
      in (Map.insert g others m, Set.null others)
    when (exit /= Tox.ExitSelfDisconnected && alone) $ do
      putStrLn $ "Leaving group " <> show g <> ", as everyone else left"
      atomicModifyIORef' (botPeers bot) ((, ()) . Map.delete g)
      atomicModifyIORef' (botSaid bot) ((, ()) . Map.delete (InGroup g))
      void $ Tox.groupLeave tox g ""
      saved
  Tox.SelfConnectionChanged status -> putStrLn $ "Connection: " <> show status
  _ -> pure ()
  where
    tox = botTox bot
    saved = save tox (botSavefile bot)
    forward :: [Word8] -> PublicKey -> BS.ByteString -> IO ()
    forward ids pk packet = forM_ (BS.uncons packet) $ \(ident, _) ->
      when (ident `elem` ids) $ AV.receivePacket (botAv bot) pk packet

-- | The replies to a `!` command, a name in any case: the help if it's none
-- of the commands.
run :: Bot -> PublicKey -> BS.ByteString -> IO [BS.ByteString]
run bot pk request = fromMaybe (pure [usage]) $
  find ((requestName r `elem`) . commandNames) commands >>= \c -> commandRun c bot pk r
  where
    (name, rest) = Char8.dropWhile spaced <$> Char8.break spaced (Char8.dropWhileEnd spaced (Char8.dropWhile spaced request))
    r = Request (map toLower (Char8.unpack name)) (map Char8.unpack (spaceWords rest)) rest

-- | Calls a friend who asked, after this many seconds, and says what came of
-- it.
call :: Bot -> PublicKey -> Bool -> Int -> IO BS.ByteString
call bot pk video seconds = do
  now <- getCurrentTime
  allowed <- atomicModifyIORef' (askedAt asked) $ \m -> case Map.lookup pk m of
    Just at | diffUTCTime now at < fromIntegral callCooldown -> (m, False)
    _ -> (Map.insert pk now m, True)
  if not allowed then pure ("Wait " <> shown callCooldown <> " s between calls") else do
    let place = AV.call (botAv bot) pk (BitRates (fromIntegral audioRate) (if video then fromIntegral videoRate else 0))
          >>= either (\e -> ("Couldn't call you: " <> shown e) <$ putStrLn ("Failed to call: " <> show e))
                     (const ("Calling you" <$ putStrLn ("Calling " <> hex pk)))
    if seconds == 0 then place else do
      waiter <- forkIO $ do
        threadDelay (seconds * 1000000)
        atomicModifyIORef' (askedWaiting asked) (\m -> (Map.delete pk m, ()))
        void place
      atomicModifyIORef' (askedWaiting asked) (\m -> (Map.insert pk waiter m, ()))
      pure ("Calling you in " <> shown seconds <> " s")
  where
    asked = botAsked bot

-- | Ends a friend's call, or the one they're waiting for.
hangUp :: Bot -> PublicKey -> IO BS.ByteString
hangUp bot pk = do
  pending <- atomicModifyIORef' (askedWaiting (botAsked bot)) (\m -> (Map.delete pk m, Map.lookup pk m))
  mapM_ killThread pending
  AV.control (botAv bot) pk AV.Cancel <&> \case
    Right () -> "Hung up"
    Left _ | Just _ <- pending -> "Won't call you"
           | otherwise -> "No call to end"

-- | Sends a friend the text after this many seconds, and says so.
later :: Bot -> PublicKey -> BS.ByteString -> Int -> IO [BS.ByteString]
later bot pk text seconds = do
  void . forkIO $ threadDelay (seconds * 1000000) >> void (Tox.friendSendMessage (botTox bot) pk Tox.MessageNormal text)
  pure ["Sending it in " <> shown seconds <> " s"]

-- | Makes a group or conference, as named, and invites the friend to it.
invite :: Bot -> PublicKey -> String -> IO BS.ByteString
invite bot pk kind = do
  invited <- if kind == "group"
    then Tox.groupNew tox True name (botNick bot) >>= maybe (pure False) (\g -> Tox.groupInviteFriend tox g pk)
    else Tox.conferenceNew tox >>= maybe (pure False) (\g -> Tox.conferenceSetTitle tox g name >> Tox.conferenceInvite tox pk g)
  pure $ bool "Couldn't make a " "Invited you to my " invited <> Char8.pack kind
  where
    tox = botTox bot
    name = botNick bot <> "'s " <> Char8.pack kind

-- | Sends an echo once it's due, and stops typing to the friend if no more
-- are coming.
echo :: Bot -> Due -> IO ()
echo bot (Due at pk ty message) = do
  now <- getCurrentTime
  threadDelay . ceiling $ max 0 (diffUTCTime at now) * 1000000
  void $ Tox.friendSendMessage (botTox bot) pk ty message
  done <- atomicModifyIORef' (botTyping bot) $ \m ->
    let left = Map.update (\n -> n - 1 <$ guard (n > 1)) pk m in (left, Map.notMember pk left)
  when done . void $ Tox.friendSetTyping (botTox bot) pk False

-- | Whether to echo a message in a group: not one the bot said there lately.
echoes :: Bot -> Chat -> BS.ByteString -> IO Bool
echoes bot chat message = do
  now <- getCurrentTime
  atomicModifyIORef' (botSaid bot) $ \said ->
    let recent = filter ((< echoMemory) . diffUTCTime now . fst) (Map.findWithDefault [] chat said)
        fresh = message `notElem` map snd recent
    in (Map.insert chat (bool recent ((now, message) : recent) fresh) said, fresh)

-- | Sends the avatar and the files that weren't sent to a friend who came
-- online, and keeps those being sent to one who went offline.
connection :: Bot -> PublicKey -> Bool -> IO ()
connection bot pk online = do
  was <- atomicModifyIORef' (botOnline bot) $ \s -> (bool Set.delete Set.insert online pk s, Set.member pk s)
  when (was /= online) $ if online
    then do
      Tox.fileSend tox pk Tox.fileKindAvatar (fromIntegral (BS.length avatar)) (Just (sha256 avatar)) ""
        >>= either (putStrLn . (("Failed to send " <> hex pk <> " the avatar: ") <>) . show)
                   (\n -> start bot (pk, n) SendingAvatar)
      unsent <- atomicModifyIORef' (botFiles bot) $ \f ->
        (f { filesUnsent = Map.delete pk (filesUnsent f) }, Map.findWithDefault [] pk (filesUnsent f))
      mapM_ (offer bot pk) unsent
    else do
      f <- readIORef (botFiles bot)
      let theirs :: (PublicKey, Word8) -> a -> Bool
          theirs (friend, _) _ = friend == pk
          (gone, receiving) = Map.partitionWithKey theirs (filesReceiving f)
          (stopped, sending) = Map.partitionWithKey theirs (filesSending f)
          offers = [o | SendingFile o _ <- Map.elems stopped]
      mapM_ (hClose . receivingHandle) gone
      sequence_ [hClose stored | SendingFile _ stored <- Map.elems stopped]
      modifyFiles bot . const $ f
        { filesReceiving = receiving
        , filesSending = sending
        , filesEnded = Map.filterWithKey ((not .) . theirs) (filesEnded f)
        , filesUnsent = Map.insertWith (flip (<>)) pk offers (filesUnsent f)
        }
  where
    tox = botTox bot

receive :: Bot -> PublicKey -> Word8 -> Word32 -> Word64 -> BS.ByteString -> BS.ByteString -> IO ()
receive bot pk n kind size fileId name
  | kind /= Tox.fileKindData = cancel
  | size > maxFileSize = refuse ("I take files up to " <> sizeText maxFileSize)
  | otherwise = held (botDir bot) >>= \have -> if have + size > maxHeld then refuse full else do
      let path = filePath bot pk fileId <.> "part"
      stored <- openBinaryFile path ReadWriteMode
      kept <- fromIntegral <$> hFileSize stored
      if | kept >= size -> hSetFileSize stored 0
         | kept > 0 -> do
             putStrLn $ "Resuming " <> hex pk <> "'s file at " <> show kept
             void $ Tox.fileSeek tox pk n kept
         | otherwise -> pure ()
      void $ Tox.fileControl tox pk (Tox.Incoming n) Tox.FileResume
      modifyFiles bot $ \f -> f { filesReceiving = Map.insert (pk, n) (Receiving path stored name) (filesReceiving f) }
  where
    tox = botTox bot
    cancel = void $ Tox.fileControl tox pk (Tox.Incoming n) Tox.FileCancel
    refuse why = cancel >> void (Tox.friendSendMessage tox pk Tox.MessageNormal why)

-- | Writes a chunk, or sends the whole file back once it's all there.
received :: Bot -> PublicKey -> Word8 -> Word64 -> BS.ByteString -> IO ()
received bot pk n position bytes = do
  receiving <- Map.lookup (pk, n) . filesReceiving <$> readIORef (botFiles bot)
  forM_ receiving $ \r -> let stored = receivingHandle r in if not (BS.null bytes)
    then hSeek stored AbsoluteSeek (fromIntegral position) >> BS.hPut stored bytes
    else do
      modifyFiles bot $ \f -> f { filesReceiving = Map.delete (pk, n) (filesReceiving f) }
      size <- fromIntegral <$> hFileSize stored
      hClose stored
      fileId <- randomId
      let path = filePath bot pk fileId
      renameFile (receivingPath r) path
      putStrLn $ "Sending " <> hex pk <> " their file back"
      offer bot pk (Offer path fileId (receivingName r) size)

requested :: Bot -> PublicKey -> Word8 -> Word64 -> Int -> IO ()
requested bot pk n position len
  | len == 0 = end bot (pk, n)
  | otherwise = Map.lookup (pk, n) . filesSending <$> readIORef (botFiles bot) >>= \case
      Just SendingAvatar -> send . BS.take len $ BS.drop (fromIntegral position) avatar
      Just (SendingFile _ stored) -> hSeek stored AbsoluteSeek (fromIntegral position) >> BS.hGet stored len >>= send
      Nothing -> pure ()
  where
    send = writeChan (botChunks bot) . Chunk pk n position

-- | Waits while toxcore has no room, so that no later chunk comes first.
sendChunk :: Tox.Tox -> Chunk -> IO ()
sendChunk tox chunk@(Chunk pk n position bytes) = Tox.fileSendChunk tox pk n position bytes >>= \case
  Left Tox.FileSendQueueFull -> threadDelay queueWait >> sendChunk tox chunk
  _ -> pure ()

cancelled :: Bot -> PublicKey -> Tox.FileRef -> IO ()
cancelled bot pk = \case
  Tox.Incoming n -> do
    receiving <- atomicModifyIORef' (botFiles bot) $ \f ->
      (f { filesReceiving = Map.delete (pk, n) (filesReceiving f) }, Map.lookup (pk, n) (filesReceiving f))
    forM_ receiving $ \r -> hClose (receivingHandle r) >> removeFile (receivingPath r)
  Tox.Outgoing n -> end bot (pk, n)

-- | Sends under a number, which ended any transfer it was for before.
start :: Bot -> (PublicKey, Word8) -> Sending -> IO ()
start bot key sending = do
  before <- atomicModifyIORef' (botFiles bot) $ \f -> case Map.lookup key (filesSending f) of
    Nothing -> (f { filesSending = Map.insert key sending (filesSending f) }, Nothing)
    Just b -> (f { filesSending = Map.insert key sending (filesSending f)
                 , filesEnded = Map.insertWith (+) key 1 (filesEnded f) }, Just b)
  forM_ before forget

-- | Forgets a transfer that ended, unless the end is of one before it.
end :: Bot -> (PublicKey, Word8) -> IO ()
end bot key = do
  ended <- atomicModifyIORef' (botFiles bot) $ \f -> case Map.lookup key (filesEnded f) of
    Just _ -> (f { filesEnded = Map.update (\k -> k - 1 <$ guard (k > 1)) key (filesEnded f) }, Nothing)
    Nothing -> (f { filesSending = Map.delete key (filesSending f) }, Map.lookup key (filesSending f))
  forM_ ended forget

forget :: Sending -> IO ()
forget = \case
  SendingAvatar -> pure ()
  SendingFile o stored -> hClose stored >> removeFile (offerPath o)

-- | Sends a file of random bytes, and says so.
makeFile :: Bot -> PublicKey -> Word64 -> IO BS.ByteString
makeFile bot pk size = held (botDir bot) >>= \have -> if have + size > maxHeld then pure full else do
  fileId <- randomId
  let path = filePath bot pk fileId
      name = "random-" <> sizeText size <> ".bin"
  withBinaryFile "/dev/urandom" ReadMode $ \from -> withBinaryFile path WriteMode $ \to ->
    let copy left = unless (left == 0) $ do
          let chunk = min left mib
          BS.hGet from (fromIntegral chunk) >>= BS.hPut to
          copy (left - chunk)
    in copy size
  offer bot pk (Offer path fileId name size)
  pure ("Sending you " <> name)

-- | Keeps the offer to send again when the friend comes back, if it fails.
offer :: Bot -> PublicKey -> Offer -> IO ()
offer bot pk o = try @IOException (openBinaryFile (offerPath o) ReadMode) >>= \case
  Left e -> putStrLn $ "Failed to open " <> offerPath o <> ": " <> show e
  Right stored -> Tox.fileSend (botTox bot) pk Tox.fileKindData (offerSize o) (Just (offerId o)) (offerName o) >>= \case
    Right n -> start bot (pk, n) (SendingFile o stored)
    Left e -> do
      putStrLn $ "Failed to send " <> hex pk <> " a file: " <> show e
      hClose stored
      modifyFiles bot $ \f -> f { filesUnsent = Map.insertWith (flip (<>)) pk [o] (filesUnsent f) }

modifyFiles :: Bot -> (Files -> Files) -> IO ()
modifyFiles bot f = atomicModifyIORef' (botFiles bot) ((, ()) . f)

-- | Where a friend's file with this ID is kept.
filePath :: Bot -> PublicKey -> BS.ByteString -> FilePath
filePath bot pk fileId = botDir bot </> hex pk <> "-" <> Char8.unpack (Base16.encode fileId)

-- | The bytes of all files kept now.
held :: FilePath -> IO Word64
held dir = listDirectory dir >>= fmap (sum . map fromIntegral) . mapM (getFileSize . (dir </>))

randomId :: IO BS.ByteString
randomId = withBinaryFile "/dev/urandom" ReadMode (`BS.hGet` Messenger.fileIdLength)

-- | A size as `!file` takes it: bytes, or with K or M for KiB or MiB.
parseSize :: String -> Maybe Word64
parseSize word = case span isDigit (map toLower word) of
  (digits@(_ : _), unit) | Just times <- lookup unit [("", 1), ("k", 1024), ("m", toInteger mib)]
                         , bytes <- read digits * times, bytes <= toInteger (maxBound :: Word64) ->
    Just (fromInteger bytes)
  _ -> Nothing

sizeText :: Word64 -> BS.ByteString
sizeText size
  | size >= mib, size `mod` mib == 0 = shown (size `div` mib) <> "M"
  | size >= 1024, size `mod` 1024 == 0 = shown (size `div` 1024) <> "K"
  | otherwise = shown size

-- | Lines too many for one message, split as clients split them: at the last
-- line break that fits.
longMessage :: [BS.ByteString]
longMessage = reverse . foldl' add [] $ map line [1 .. longLines]
  where
    line n = Text.encodeUtf8 ("Line " <> Text.pack (show n) <> " of " <> Text.pack (show longLines) <> ": äöü € 😀")
    add (part : parts) l | BS.length part + 1 + BS.length l <= Tox.maxMessageLength = part <> "\n" <> l : parts
    add parts l = l : parts

spaced :: Char -> Bool
spaced = (`elem` (" \t\r\n" :: String))

spaceWords :: BS.ByteString -> [BS.ByteString]
spaceWords = filter (not . BS.null) . Char8.splitWith spaced

shown :: Show a => a -> BS.ByteString
shown = Char8.pack . show

-- | Answers calls and sends back what comes, with the times it was captured
-- at, so that its audio and video stay in step as the friend sent them.
echoCall :: AV.ToxAV PublicKey -> AV.Event PublicKey -> IO ()
echoCall av = \case
  AV.Invited pk caps -> do
    putStrLn $ "Answering " <> hex pk <> "'s call (video: " <> show (AV.sendsVideo caps) <> ")"
    AV.answer av pk (rates caps) >>= either (putStrLn . ("Failed to answer: " <>) . show) pure
  AV.PeerChanged pk caps ->
    -- Video back only while the friend sends some.
    void $ AV.setBitRate av pk Video (if AV.sendsVideo caps then videoRate else 0)
  AV.AudioReceived pk _ captured frame -> void $ AV.sendAudio av pk captured frame
  AV.VideoReceived pk captured picture -> void $ AV.sendVideo av pk captured picture
  AV.Ended pk why -> putStrLn $ hex pk <> "'s call ended: " <> show why
  where
    rates caps = BitRates (fromIntegral audioRate) (if AV.sendsVideo caps then fromIntegral videoRate else 0)

hex :: PublicKey -> String
hex = Char8.unpack . Base16.encode . Sodium.encode
