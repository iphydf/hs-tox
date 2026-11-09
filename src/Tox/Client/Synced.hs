{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
-- | The client's values as account sync has them, and back.
module Tox.Client.Synced
  ( keyOf
  , contactKey
  , contactIdOf
  , unhex'
  , hexOf
  , keyHex
  , chatGroupId
  , chatIdOf
  , muteKind
  , picturesKind
  , syncPictures
  , picturesOf
  , syncRole
  , syncVoice
  ) where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Data.Word              (Word64)
import qualified Tox.Application.Tox    as Tox
import qualified Tox.Sync.Fact          as Sync
import qualified Tox.Sync.Id            as Sync
import qualified Tox.Sync.Setting       as Sync

import qualified Tox.Client.Pictures    as Pictures
import           Tox.Client.Types

keyOf :: Text -> Maybe Sync.PublicKey
keyOf key = case unhex' key of
  Just bytes | BS.length bytes == 32 -> Just (Sync.PublicKey bytes)
  _                                  -> Nothing

contactKey :: ContactId -> Maybe Sync.PublicKey
contactKey (ContactId key) = keyOf key

contactIdOf :: Sync.PublicKey -> ContactId
contactIdOf = ContactId . keyHex

unhex' :: Text -> Maybe BS.ByteString
unhex' = either (const Nothing) Just . Base16.decode . Text.encodeUtf8

hexOf :: BS.ByteString -> Text
hexOf = Text.toUpper . Text.decodeUtf8 . Base16.encode

keyHex :: Sync.PublicKey -> Text
keyHex (Sync.PublicKey pk) = hexOf pk

chatGroupId :: Sync.ChatId -> GroupId
chatGroupId (Sync.GroupChat i)      = GroupId (hexOf i)
chatGroupId (Sync.ConferenceChat i) = GroupId (hexOf i)

-- | Chat of a group or conference, by its ID in hex.
chatIdOf :: GroupId -> Group -> Maybe Sync.ChatId
chatIdOf (GroupId gid) g = (if groupKind g == NgcGroup then Sync.GroupChat else Sync.ConferenceChat) <$> unhex' gid

muteKind, picturesKind :: Word64
muteKind = Sync.settingKind (Sync.Mute Nothing True)
picturesKind = Sync.settingKind (Sync.PictureFormat Sync.Original)

-- | Our picture format key as a sync encoding ('picturesOf' turns it back).
syncPictures :: Text -> Maybe Sync.PictureEncoding
syncPictures key
  | key == Pictures.formatKey Pictures.qoi = Just Sync.Qoi
  | otherwise = Nothing

picturesOf :: Sync.PictureEncoding -> Maybe Text
picturesOf = \case
  Sync.Qoi      -> Just (Pictures.formatKey Pictures.qoi)
  Sync.Original -> Nothing

syncRole :: Tox.Role -> Sync.Role
syncRole = \case
  Tox.Founder   -> Sync.Founder
  Tox.Moderator -> Sync.Moderator
  Tox.User      -> Sync.User
  Tox.Observer  -> Sync.Observer

syncVoice :: Tox.Voice -> Sync.VoiceState
syncVoice = \case
  Tox.VoiceAll        -> Sync.VoiceEveryone
  Tox.VoiceModerators -> Sync.VoiceModerators
  Tox.VoiceFounder    -> Sync.VoiceFounder
