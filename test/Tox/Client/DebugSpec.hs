{-# LANGUAGE OverloadedStrings #-}
module Tox.Client.DebugSpec (spec) where

import           Control.Exception   (bracket)
import qualified Data.Text           as Text
import           Data.Time           (UTCTime (..), addUTCTime, fromGregorian)
import           System.Directory    (createDirectory, getTemporaryDirectory,
                                      listDirectory, removePathForcibly)
import           System.FilePath     ((</>))
import           Test.Hspec

import qualified Tox.Application.Tox as Tox

import           Tox.Client.Debug
import           Tox.Client.Sample   (sampleReport)

spec :: Spec
spec =
  describe "Debug rows" $ do
    it "give the runtime, threads and the app's work, times in microseconds" $
      mapM_ (\row -> processRows sampleReport `shouldContain` [row])
        [ ("heap live", 20 * 1048576), ("heap fragmented", 1048576), ("gc cpu us", 50000)
        , ("threads tox main loop (blocked on BlockedOnSTM)", 1)
        , ("app lane Render count", 3), ("app lane Render total us", 4500), ("app lane Render worst us", 2000) ]

    it "give each profile's Tox under its name, empty sizes too" $
      mapM_ (\row -> toxRows "profile 7" sampleReport `shouldContain` [row])
        [ ("profile 7 tox lane Periodic work total us", 9000)
        , ("profile 7 size Shared keys", 3000), ("profile 7 size TCP relays", 0)
        , ("profile 7 traffic UDP sent NodesRequest packets", 10), ("profile 7 traffic UDP sent NodesRequest bytes", 20480) ]

    it "sums up the traffic since the report before, else since Tox started" $ do
      let earlier = sampleReport { reportTraffic = [ (Tox.Requests Tox.SearchFriend, Tox.Count 3 600) ] }
          line = Text.splitOn "; " . summary (Just earlier)
      line sampleReport `shouldContain`
        [ "traffic since the last: out 23 KB (10 UDP packets), in 0 KB (0 UDP packets)"
        , "most sent NodesRequest 10 (20 KB)", "requests searching for friends 4" ]
      Text.splitOn "; " (summary Nothing sampleReport) `shouldContain` ["requests searching for friends 7"]

    it "give each call's audio and video, in the rows and the log" $ do
      mapM_ (\row -> toxRows "profile 7" sampleReport `shouldContain` [row])
        [ ("profile 7 call 1d16d2ab audio made up", 2), ("profile 7 call 1d16d2ab video lost", 7)
        , ("profile 7 call 1d16d2ab video smeared frames", 3), ("profile 7 call 1d16d2ab video smeared ms", 66) ]
      Text.splitOn "; " (summary Nothing sampleReport) `shouldContain`
        [ "call with 1d16d2ab: audio 3 sped up, 1 slowed down, 2 blocks made up, 0 jumps ahead"
        , "video 600 frames, 7 lost (5 incomplete), 22 key frames, 2 smears over 3 frames (66 ms), 598 pictures, 0 failed"
        , "sending video with c2.exynos.h264.encoder, NV12 480x640 in rows of 512, planes of 640" ]

    it "keeps the last 30 minutes of samples, in files that expire whole" $ withDir $ \dir -> do
      let at m = addUTCTime (m * 60) (UTCTime (fromGregorian 2026 11 7) 0)
      mapM_ (\m -> recordSample dir (at m) [("heap live", round m)]) [0, 5, 12, 25, 41]
      files <- length <$> listDirectory dir
      exported <- export dir (at 43) [("heap live", 43)]
      (files, [ v | [_, _, v] <- map (Text.splitOn "\t") (drop 1 (Text.lines exported)) ])
        `shouldBe` (3, ["25", "41", "43"])

    it "reads the last heap census, nothing without one" $ withDir $ \dir -> do
      let path = dir </> "heap.hp"
      writeFile path $ unlines
        [ "JOB \"htox\"", "BEGIN_SAMPLE 0.0", "END_SAMPLE 0.0"
        , "BEGIN_SAMPLE 1.0", "ARR_WORDS\t4096", "ghc-prim:GHC.Types.:\t48", "END_SAMPLE 1.0" ]
      heapByType path `shouldReturn` [("heap type ARR_WORDS", 4096), ("heap type ghc-prim:GHC.Types.:", 48)]
      heapByType (dir </> "none.hp") `shouldReturn` []
  where
    withDir = bracket
      (getTemporaryDirectory >>= \tmp -> let dir = tmp </> "tox-debug-spec" in
        removePathForcibly dir >> createDirectory dir >> pure dir)
      removePathForcibly
