{-# LANGUAGE OverloadedStrings #-}
module Tox.Client.Toxcore.CSpec (spec) where

import qualified Data.Binary             as Binary
import qualified Data.ByteString.Lazy    as LBS
import           Data.Function           (on)
import           Data.List               (nubBy)
import           Test.Hspec              (Spec, describe, it, shouldBe)
import           Test.QuickCheck         (arbitrary, forAll, resize, (===))
import qualified Tox.Client.Toxcore      as Tox
import           Tox.Client.Types        (GroupKind (..))
import qualified Tox.Crypto.Core.KeyPair as KeyPair
import qualified Tox.Persistence         as P
import qualified Tox.Persistence.Groups  as PG

import           Tox.Client.Toxcore.C    (cToxcore, chatIdOf, savedGroup)

spec :: Spec
spec = describe "c-toxcore" $ do
  -- hs-toxcore's groups get big fast.
  it "gives each group of a save by its chat ID" . forAll (resize 10 arbitrary) $ \groups -> do
    let gs = nubBy ((==) `on` chatIdOf) groups
        save = LBS.toStrict (Binary.encode (P.SaveData [P.SectionGroups (PG.Groups gs), P.SectionEOF]))
    map (`savedGroup` save) (map chatIdOf gs) === map (Just . LBS.toStrict . Binary.encode . PG.Groups . pure) gs

  it "keeps a group over a restart" $ do
    keys <- KeyPair.newKeyPair
    let options = Tox.defaultOptions
          { Tox.optSaveData = Tox.SecretKey (KeyPair.secretKey keys)
          , Tox.optUdpEnabled = False
          , Tox.optLocalDiscovery = False
          , Tox.optLogger = \_ _ -> pure ()
          }
    Right tox <- Tox.engineStart cToxcore options []
    Just g <- Tox.groupNew tox True "group" "me"
    chat <- Tox.groupChatId tox g
    Just saved <- Tox.groupSave tox g
    Tox.kill tox
    Right tox' <- Tox.engineStart cToxcore options [(NgcGroup, saved)]
    restored <- Tox.groupRestore tox' saved >>= traverse (Tox.groupChatId tox')
    Tox.kill tox'
    restored `shouldBe` Just chat
