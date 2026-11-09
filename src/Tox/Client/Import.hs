{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | An account from another client's .tox file: keys, profile, friends and
-- pending requests. We leave out conferences and groups.
module Tox.Client.Import
  ( ImportError (..)
  , importSave
  , unlockSave
  ) where

import qualified Crypto.Saltine.Class          as Sodium
import           Data.Bifunctor                (first)
import qualified Data.Binary                   as Binary
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base16        as Base16
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Data.Word                     (byteSwap32)
import qualified Tox.Application.EncryptedSave as EncryptedSave
import qualified Tox.Application.Save          as Save
import qualified Tox.Application.Tox           as Tox
import           Tox.Client
import           Tox.Client.Types
import qualified Tox.Crypto.Core.KeyPair       as KeyPair

data ImportError
  = Encrypted
  | WrongPassword
  | NotAProfile
  deriving (Eq, Show)

importSave :: BS.ByteString -> Either ImportError (Identity, Client)
importSave bytes
  | EncryptedSave.isEncrypted bytes = Left Encrypted
  | otherwise = case Binary.decodeOrFail (LBS.fromStrict bytes) of
      Right (_, _, saved) | Just p <- Save.fromSaveData saved -> Right (account p)
      _ -> Left NotAProfile
  where
    account p =
      ( Identity (Sodium.encode (KeyPair.secretKey (Save.profileKeys p)))
          -- The API gives it the other way round from how it's saved.
          (byteSwap32 (Save.profileNospam p))
      , initialClient
          { clientProfile = (clientProfile initialClient)
              { profileNickname = text (Save.profileName p)
              , profileStatus = text (Save.profileStatusMessage p)
              , profileAway = Save.profileStatus p }
          , clientContacts = Map.fromList (map friend (Save.profileFriends p))
          } )
    friend f =
      let key = hex (Sodium.encode (Save.sfPublicKey f))
      in ( ContactId key
         , Contact
             { contactName = if BS.null (Save.sfName f) then Nothing else Just (text (Save.sfName f))
             , contactOnline = False
             , contactMessages = []
             , contactUnread = 0
             , contactRequest = (\(nospam, message) -> (toxId key (hex nospam), text message)) <$> Save.sfRequest f
             , contactStatusMessage = text (Save.sfStatusMessage f)
             , contactStatus = Tox.Online
             , contactMute = Nothing
             , contactAvatar = Nothing
             , contactPictures = Nothing
             , contactEarlier = False
             } )
    text = Text.decodeUtf8Lenient
    hex = Text.toUpper . Text.decodeUtf8 . Base16.encode

-- | The save inside a .tox file saved with a password.
unlockSave :: Text.Text -> BS.ByteString -> Either ImportError BS.ByteString
unlockSave password = first refused . EncryptedSave.decryptSave (Text.encodeUtf8 password)
  where
    refused = \case
      EncryptedSave.NotEncrypted  -> NotAProfile
      EncryptedSave.WrongPassword -> WrongPassword
