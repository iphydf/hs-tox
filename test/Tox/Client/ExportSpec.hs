{-# LANGUAGE OverloadedStrings #-}
module Tox.Client.ExportSpec (spec) where

import qualified Crypto.Saltine.Class    as Sodium
import qualified Data.Binary             as Binary
import           Data.Bits               (shiftR)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as Base16
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Map.Strict         as Map
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           Test.Hspec
import qualified Tox.Application.Save    as Save
import qualified Tox.Crypto.Core.KeyPair as KeyPair

import           Tox.Client
import           Tox.Client.Export
import           Tox.Client.Import
import           Tox.Client.Sample
import           Tox.Client.Types

hex :: BS.ByteString -> Text.Text
hex = Text.toUpper . Text.decodeUtf8 . Base16.encode

spec :: Spec
spec = do
  it "writes our keys, profile and contacts as other clients read them" $ do
    keys <- KeyPair.newKeyPair
    save <- maybe (fail "no save") pure $ exportSave (Identity (Sodium.encode (KeyPair.secretKey keys)) 0x0BADF00D) sample
    profile <- maybe (fail "unreadable save") pure $ Save.fromSaveData (Binary.decode (LBS.fromStrict save))
    KeyPair.publicKey (Save.profileKeys profile) `shouldBe` KeyPair.publicKey keys
    -- Same Tox ID.
    toxId (hex (Sodium.encode (KeyPair.publicKey keys))) "0BADF00D"
      `shouldBe` toxId (hex (Sodium.encode (KeyPair.publicKey (Save.profileKeys profile))))
                       (hex (BS.pack [fromIntegral (Save.profileNospam profile `shiftR` s) | s <- [0, 8, 16, 24]]))
    Save.profileName profile `shouldBe` Text.encodeUtf8 (profileNickname (clientProfile sample))
    map Save.sfName (Save.profileFriends profile) `shouldBe` map Text.encodeUtf8 ["Alice", "Bob"]

  it "takes back what it gives, as another client's profile" $ do
    keys <- KeyPair.newKeyPair
    let me = Identity (Sodium.encode (KeyPair.secretKey keys)) 0x0BADF00D
    save <- maybe (fail "no save") pure (exportSave me sample)
    (known, client) <- either (fail . show) pure (importSave save)
    known `shouldBe` me
    profileNickname (clientProfile client) `shouldBe` profileNickname (clientProfile sample)
    Map.toList (contactName <$> clientContacts client) `shouldBe` [(alice, Just "Alice"), (bob, Just "Bob")]
    importSave "toxEsave and the rest" `shouldBe` Left Encrypted

  it "tells a profile saved with a password from one that isn't a profile" $ do
    importSave "toxEsave and the rest" `shouldBe` Left Encrypted
    unlockSave "password" "toxEsave too short" `shouldBe` Left NotAProfile
    unlockSave "password" "toxEsave" `shouldBe` Left NotAProfile
