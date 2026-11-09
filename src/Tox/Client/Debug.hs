{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
-- | Where time and memory go, for the debug screen and the log.
module Tox.Client.Debug
  ( Report (..)
  , Call (..)
  , callText
  , videoText
  , Runtime (..)
  , Threads
  , runtime
  , threads
  , summary
  , processRows
  , toxRows
  , recordSample
  , export
  , heapByType
  ) where

import           Control.Exception     (IOException, try)
import           Control.Monad         (forM_, unless, void)
import           Data.Int              (Int64)
import           Data.List             (isPrefixOf, sortOn)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (mapMaybe)
import           Data.Ord              (Down (..))
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import           Data.Time             (UTCTime, addUTCTime, defaultTimeLocale,
                                        formatTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Word             (Word64)
import           GHC.Conc.Sync         (ThreadStatus (..), listThreads,
                                        threadLabel, threadStatus)
import           GHC.Profiling         (requestHeapCensus)
import qualified GHC.Stats             as Stats
import           System.Directory      (listDirectory, removeFile)
import           System.FilePath       (dropExtension, takeExtension, (</>))
import           System.Mem            (performMajorGC)
import           Text.Read             (readMaybe)

import qualified Tox.Application.Tox   as Tox
import qualified Tox.AV                as AV
import qualified Tox.AV.Playout        as Playout
import           Tox.Core.Lanes        (Lane (..))

-- | How a call's audio played and its video came, so far.
data Call = Call
  { callWith  :: Text -- ^ The start of the friend's key.
  , callAudio :: Playout.Stats
  , callVideo :: Maybe AV.VideoStats
  , callSends :: Maybe Text -- ^ The encoder we send our video with.
  } deriving (Eq, Show)

callText :: Call -> Text
callText (Call with (Playout.Stats up down made skips) video sends) =
  "call with " <> with <> ": audio " <> n up <> " sped up, " <> n down <> " slowed down, "
  <> n made <> " blocks made up, " <> n skips <> " jumps ahead"
  <> maybe "" (("; " <>) . videoText) video
  <> maybe "" ("; sending video with " <>) sends
  where n = Text.pack . show

-- | A smear is from a lost video frame to the next key frame, which we ask
-- the friend for.
videoText :: AV.VideoStats -> Text
videoText v =
  "video " <> n (AV.framesCame v) <> " frames, " <> n (AV.framesLost v) <> " lost ("
  <> n (AV.framesIncomplete v) <> " incomplete), " <> n (AV.keyFramesCame v) <> " key frames, "
  <> n (AV.smears v) <> " smears over " <> n (AV.smearedFrames v) <> " frames ("
  <> n (AV.smearedMs v) <> " ms), " <> n (AV.picturesDecoded v) <> " pictures, "
  <> n (AV.decodeFailures v) <> " failed"
  where n = Text.pack . show

-- | From the GHC runtime, which keeps them only when started with @-T@.
data Runtime = Runtime
  { rtLiveBytes  :: Word64 -- ^ After the last GC.
  , rtMaxLive    :: Word64 -- ^ The most after any major GC.
  , rtGcs        :: Int
  , rtMajorGcs   :: Int
  , rtGcCpu      :: Int64 -- ^ Nanoseconds, like the rest.
  , rtMutatorCpu :: Int64
  , rtGcElapsed  :: Int64
  , rtElapsed    :: Int64
  , rtAllocated  :: Word64 -- ^ Bytes, since the start.
  , rtCopied     :: Word64 -- ^ Bytes the GC copied, since the start.
  , rtMemInUse   :: Word64 -- ^ Taken from the system, after the last GC.
  , rtLarge      :: Word64 -- ^ Large objects, like big byte arrays.
  , rtFragmented :: Word64 -- ^ In blocks that pinned objects keep.
  } deriving (Eq, Show)

-- | How many threads have each label and state.
type Threads = Map (Text, Text) Int

data Report = Report
  { reportRuntime :: Maybe Runtime
  , reportThreads :: Threads
  , reportApp     :: Map Text Lane -- ^ The app's own work.
  , reportTox     :: Map Text Lane -- ^ Tox's main loop.
  , reportSizes   :: [Tox.Size]
  , reportTraffic :: [(Tox.Counted, Tox.Count)] -- ^ Since Tox started.
  , reportCalls   :: [Call]
  } deriving (Eq, Show)

runtime :: IO (Maybe Runtime)
runtime = Stats.getRTSStatsEnabled >>= \case
  False -> pure Nothing
  True -> Stats.getRTSStats >>= \s -> pure $ Just Runtime
    { rtLiveBytes  = Stats.gcdetails_live_bytes (Stats.gc s)
    , rtMaxLive    = Stats.max_live_bytes s
    , rtGcs        = fromIntegral (Stats.gcs s)
    , rtMajorGcs   = fromIntegral (Stats.major_gcs s)
    , rtGcCpu      = Stats.gc_cpu_ns s
    , rtMutatorCpu = Stats.mutator_cpu_ns s
    , rtGcElapsed  = Stats.gc_elapsed_ns s
    , rtElapsed    = Stats.elapsed_ns s
    , rtAllocated  = Stats.allocated_bytes s
    , rtCopied     = Stats.copied_bytes s
    , rtMemInUse   = Stats.gcdetails_mem_in_use_bytes (Stats.gc s)
    , rtLarge      = Stats.gcdetails_large_objects_bytes (Stats.gc s)
    , rtFragmented = Stats.gcdetails_block_fragmentation_bytes (Stats.gc s)
    }

threads :: IO Threads
threads = listThreads >>= fmap (Map.fromListWith (+)) . mapM (\t -> do
  name <- maybe "unnamed" Text.pack <$> threadLabel t
  status <- threadStatus t
  pure ((name, describe status), 1))
  where
    describe = \case
      ThreadRunning   -> "running"
      ThreadFinished  -> "finished"
      ThreadDied      -> "died"
      ThreadBlocked r -> "blocked on " <> Text.pack (show r)

-- | One line for the log, with what was sent and received since the report
-- before if there was one, else since Tox started.
summary :: Maybe Report -> Report -> Text
summary before r = Text.intercalate "; " $
  [ "heap " <> mb (rtLiveBytes rt) <> " (most " <> mb (rtMaxLive rt) <> "), GC " <> pct (rtGcCpu rt) (rtGcCpu rt + rtMutatorCpu rt) <> " of CPU"
  | Just rt <- [reportRuntime r] ]
  ++ [ "threads " <> Text.pack (show (sum (reportThreads r))) ]
  ++ [ "busiest " <> Text.intercalate ", " (map lane busiest) | not (null busiest) ]
  ++ [ "big " <> Text.intercalate ", " [ Tox.sizeName s <> " " <> Text.pack (show (Tox.sizeCount s)) | s <- big ] | not (null big) ]
  ++ trafficSummary (maybe "since Tox started" (const "since the last") before)
       (sinceThen (maybe [] reportTraffic before) (reportTraffic r))
  ++ map callText (reportCalls r)
  where
    busiest = take 5 . sortOn (Down . laneTotal . snd) $ Map.toList (reportApp r) ++ Map.toList (reportTox r)
    lane (name, l) = name <> " " <> ms (laneTotal l) <> " in " <> Text.pack (show (laneCount l)) <> " (worst " <> ms (laneWorst l) <> ")"
    big = take 5 . sortOn (Down . Tox.sizeCount) $ filter ((> 0) . Tox.sizeCount) (reportSizes r)
    mb b = Text.pack (show (b `div` 1048576)) <> " MB"
    ms n = Text.pack (show (n `div` 1000000)) <> " ms"
    pct a b = Text.pack (show (if b == 0 then 0 else a * 100 `div` b)) <> "%"

-- | The runtime, threads and the app's work, as names and numbers. Times are
-- in microseconds.
processRows :: Report -> [(Text, Int)]
processRows r =
  maybe [] runtimeRows (reportRuntime r)
  ++ [ ("threads " <> name <> " (" <> status <> ")", n) | ((name, status), n) <- Map.toList (reportThreads r) ]
  ++ laneRows "app" (reportApp r)
  where
    runtimeRows rt =
      [ ("heap live", fromIntegral (rtLiveBytes rt))
      , ("heap most live", fromIntegral (rtMaxLive rt))
      , ("heap in use", fromIntegral (rtMemInUse rt))
      , ("heap large objects", fromIntegral (rtLarge rt))
      , ("heap fragmented", fromIntegral (rtFragmented rt))
      , ("allocated", fromIntegral (rtAllocated rt))
      , ("gc copied", fromIntegral (rtCopied rt))
      , ("gc count", rtGcs rt)
      , ("gc major count", rtMajorGcs rt)
      , ("gc cpu us", micros (rtGcCpu rt))
      , ("gc elapsed us", micros (rtGcElapsed rt))
      , ("mutator cpu us", micros (rtMutatorCpu rt))
      , ("elapsed us", micros (rtElapsed rt))
      ]
    micros = fromIntegral . (`div` 1000)

-- | One profile's Tox, with its name in front.
toxRows :: Text -> Report -> [(Text, Int)]
toxRows profile r =
  laneRows (profile <> " tox") (reportTox r)
  ++ [ (profile <> " size " <> Tox.sizeName s, Tox.sizeCount s) | s <- reportSizes r ]
  ++ concat
    [ [ (key "packets", fromIntegral (Tox.countTimes c)), (key "bytes", fromIntegral (Tox.countBytes c)) ]
    | (what, c) <- reportTraffic r, let key k = profile <> " traffic " <> Tox.countedName what <> " " <> k ]
  ++ concatMap callRows (reportCalls r)
  where
    callRows (Call with (Playout.Stats up down made skips) video _) =
      [ (key ("audio " <> k), v) | (k, v) <- [("sped up", up), ("slowed down", down), ("made up", made), ("jumps ahead", skips)] ]
      ++ concat
        [ [ (key ("video " <> k), f v) | (k, f) <- videoFields ] | Just v <- [video] ]
      where key k = profile <> " call " <> with <> " " <> k
    videoFields =
      [ ("frames", AV.framesCame), ("lost", AV.framesLost), ("incomplete", AV.framesIncomplete)
      , ("key frames", AV.keyFramesCame), ("smears", AV.smears), ("smeared frames", AV.smearedFrames)
      , ("smeared ms", AV.smearedMs), ("pictures", AV.picturesDecoded), ("failed", AV.decodeFailures) ]

-- | Counts from the first totals to the second.
sinceThen :: [(Tox.Counted, Tox.Count)] -> [(Tox.Counted, Tox.Count)] -> [(Tox.Counted, Tox.Count)]
sinceThen old new =
  [ (what, Tox.Count (Tox.countTimes c - Tox.countTimes o) (Tox.countBytes c - Tox.countBytes o))
  | (what, c) <- new
  , let o = Map.findWithDefault (Tox.Count 0 0) what (Map.fromList old)
  , Tox.countTimes c > Tox.countTimes o ]

-- | All we sent and received, the kinds we sent most of, and what our
-- requests were for.
trafficSummary :: Text -> [(Tox.Counted, Tox.Count)] -> [Text]
trafficSummary period counted =
  [ "traffic " <> period <> ": out " <> wire Tox.Sent <> ", in " <> wire Tox.Received ]
  ++ [ "most sent " <> Text.intercalate ", " [ sent k <> " " <> times c <> " (" <> kb (Tox.countBytes c) <> ")" | (k, c) <- mostSent ] | not (null mostSent) ]
  ++ [ "requests " <> Text.intercalate ", " [ Tox.purposeName p <> " " <> times c | (p, c) <- requests ] | not (null requests) ]
  where
    wire way = kb (sum [ Tox.countBytes c | (what, c) <- counted, onWire way what ])
      <> " (" <> times' (sum [ Tox.countTimes c | (Tox.Packets w Tox.Udp _, c) <- counted, w == way ]) <> " UDP packets)"
    -- Relayed packets are in the relay link's bytes already.
    onWire way = \case
      Tox.Packets w Tox.Udp _ -> w == way
      Tox.RelayLink w         -> w == way
      _                       -> False
    mostSent = take 3 . sortOn (Down . Tox.countBytes . snd) $
      [ ((carrier, kind), c) | (Tox.Packets Tox.Sent carrier kind, c) <- counted ]
    sent (carrier, kind) = Tox.kindName kind <> if carrier == Tox.Relay then " over relays" else ""
    requests = sortOn (Down . Tox.countTimes . snd) [ (p, c) | (Tox.Requests p, c) <- counted ]
    times = times' . Tox.countTimes
    times' = Text.pack . show
    kb b = Text.pack (show (b `div` 1024)) <> " KB"

laneRows :: Text -> Map Text Lane -> [(Text, Int)]
laneRows prefix ls = concat
  [ [ (key "count", laneCount l), (key "total us", us (laneTotal l)), (key "worst us", us (laneWorst l)) ]
  | (name, l) <- Map.toList ls, let key what = prefix <> " lane " <> name <> " " <> what ]
  where us = fromIntegral . (`div` 1000)

-- | How far back the export goes, in seconds.
window :: Integer
window = 30 * 60

-- | Seconds each file of samples covers, by the clock, so whole files
-- expire.
block :: Integer
block = 10 * 60

-- | Files of samples, with the second each starts at. Others named like them
-- are left over.
sampleFiles :: FilePath -> IO [(Maybe Integer, FilePath)]
sampleFiles dir = do
  names <- listDirectory dir
  pure [ (readMaybe (drop (length prefix) (dropExtension n)), dir </> n)
       | n <- names, prefix `isPrefixOf` n, takeExtension n == ".tsv" ]
  where prefix = "debug-"

-- | On disk, so it doesn't add to the memory it measures. Files wholly older
-- than the window go.
recordSample :: FilePath -> UTCTime -> [(Text, Int)] -> IO ()
recordSample dir now rows = do
  let second = floor (utcTimeToPOSIXSeconds now)
      start = second `div` block * block
  files <- sampleFiles dir
  forM_ files $ \(from, path) -> unless (maybe False (\s -> s + block > second - window) from) $
    void (try @IOException (removeFile path))
  Text.appendFile (dir </> ("debug-" <> show start <> ".tsv")) (sampleLines now rows)

-- | Sorts by time, as text.
stamp :: UTCTime -> Text
stamp = Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

sampleLines :: UTCTime -> [(Text, Int)] -> Text
sampleLines now rows = Text.unlines [ stamp now <> "\t" <> k <> "\t" <> Text.pack (show v) | (k, v) <- rows ]

-- | The window's samples and one now.
export :: FilePath -> UTCTime -> [(Text, Int)] -> IO Text
export dir now rows = do
  files <- sortOn fst . mapMaybe (\(from, path) -> (, path) <$> from) <$> sampleFiles dir
  kept <- mapM (fmap (either (const Text.empty) id) . try @IOException . Text.readFile . snd) files
  let cutoff = stamp (addUTCTime (fromInteger (negate window)) now)
      recent = filter ((>= cutoff) . Text.takeWhile (/= '\t')) (concatMap Text.lines kept)
  pure (Text.unlines ("time\tname\tvalue" : recent) <> sampleLines now rows)

-- | Bytes on the heap by closure type, from a census the runtime writes to
-- the file when started with @-hT@. Nothing without one.
heapByType :: FilePath -> IO [(Text, Int)]
heapByType path = do
  requestHeapCensus
  performMajorGC
  text <- either (const Text.empty) id <$> try @IOException (Text.readFile path)
  let lastSample = takeWhile (not . ("END_SAMPLE" `Text.isPrefixOf`)) . drop 1
        . dropWhile (not . ("BEGIN_SAMPLE" `Text.isPrefixOf`)) . lastBlock $ Text.lines text
      lastBlock ls = case [ i | (i, l) <- zip [0 :: Int ..] ls, "BEGIN_SAMPLE" `Text.isPrefixOf` l ] of
        []     -> []
        starts -> drop (last starts) ls
  pure [ ("heap type " <> name, n) | l <- lastSample, [name, bytes] <- [Text.splitOn "\t" l]
       , Just n <- [readMaybe (Text.unpack bytes)] ]
