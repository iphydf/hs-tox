{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
-- | What a client needs from a Tox library, so it can run on hs-toxcore or
-- another one. The operations are hs-toxcore's, with the same names.
module Tox.Client.Toxcore
  ( Tox (..)
  , Engine (..)
  , SavedGroup
  , SavedConference
  , StartError (..)
  , hsToxcore
    -- * Values the operations take and give
  , NodeInfo
  , GroupNumber
  , Tox.Options (..)
  , Tox.defaultOptions
  , Tox.SaveData (..)
  , Tox.Event (..)
  , Tox.ConnectionStatus (..)
  , Tox.ToxStopped (..)
  , Tox.MessageType (..)
  , Tox.UserStatus (..)
  , Tox.AddFriendError (..)
  , Tox.SendMessageError (..)
  , Tox.CustomPacketError (..)
  , Tox.FileRef (..)
  , Tox.FileControl (..)
  , Tox.FileError (..)
  , Tox.fileKindData
  , Tox.fileKindAvatar
  , Tox.ConferenceNumber
  , Tox.PeerNumber
  , Tox.JoinError (..)
  , Tox.ConferencePeer (..)
  , Tox.PeerId
  , Tox.ExitType (..)
  , Tox.JoinFail (..)
  , Tox.AcceptError (..)
  , Tox.GroupSendError (..)
  , Tox.Role (..)
  , Tox.Moderation (..)
  , Tox.Voice (..)
  , Tox.ModError (..)
  , Tox.GroupPeer (..)
  , Tox.GroupSettings (..)
  , Tox.Size (..)
  , Tox.parseAddress
  , Tox.resolveNode
  ) where

import qualified Data.ByteString           as BS
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import           Data.Word                 (Word16, Word32, Word64, Word8)
import qualified Tox.Application.Tox       as Tox
import           Tox.Chat.GroupChat        (GroupNumber)
import           Tox.Client.Types          (GroupKind, Toxcore (..))
import           Tox.Crypto.Core.Key       (PublicKey, SecretKey)
import           Tox.Network.Core.NodeInfo (NodeInfo)

-- | A running Tox.
data Tox = Tox
  { kill                         :: IO ()
  , nextEvent                    :: IO Tox.Event
  , bootstrap                    :: NodeInfo -> IO ()
  , addTcpRelay                  :: NodeInfo -> IO ()
  , selfConnectionStatus         :: IO Tox.ConnectionStatus
  , selfPublicKey                :: PublicKey
  , selfSecretKey                :: SecretKey
  , selfAddress                  :: IO BS.ByteString
  , selfGetNospam                :: IO Word32
  , selfSetNospam                :: Word32 -> IO ()
  , selfSetName                  :: BS.ByteString -> IO Bool
  , selfSetStatusMessage         :: BS.ByteString -> IO Bool
  , selfSetStatus                :: Tox.UserStatus -> IO ()
  , friendAdd                    :: BS.ByteString -> BS.ByteString -> IO (Either (Maybe Tox.AddFriendError) PublicKey)
  , friendAddNoRequest           :: PublicKey -> IO (Either Tox.AddFriendError ())
  , friendDelete                 :: PublicKey -> IO Bool
  , friendSendMessage            :: PublicKey -> Tox.MessageType -> BS.ByteString -> IO (Either Tox.SendMessageError Word32)
  , friendSetTyping              :: PublicKey -> Bool -> IO Bool
  , friendSendLosslessPacket     :: PublicKey -> BS.ByteString -> IO (Either Tox.CustomPacketError ())
  , friendQueueLosslessPacket    :: PublicKey -> BS.ByteString -> IO ()
  , friendQueueLossyPacket       :: PublicKey -> BS.ByteString -> IO ()
  , fileSend                     :: PublicKey -> Word32 -> Word64 -> Maybe BS.ByteString -> BS.ByteString -> IO (Either Tox.FileError Word8)
  , fileControl                  :: PublicKey -> Tox.FileRef -> Tox.FileControl -> IO (Either Tox.FileError ())
  , fileSeek                     :: PublicKey -> Word8 -> Word64 -> IO (Either Tox.FileError ())
  , fileSendChunk                :: PublicKey -> Word8 -> Word64 -> BS.ByteString -> IO (Either Tox.FileError ())
  , conferenceNew                :: IO (Maybe Tox.ConferenceNumber)
  , conferenceInvite             :: PublicKey -> Tox.ConferenceNumber -> IO Bool
  , conferenceJoin               :: PublicKey -> BS.ByteString -> IO (Either Tox.JoinError Tox.ConferenceNumber)
  , conferenceDelete             :: Tox.ConferenceNumber -> IO Bool
  , conferenceSendMessage        :: Tox.ConferenceNumber -> Tox.MessageType -> BS.ByteString -> IO Bool
  , conferenceSetTitle           :: Tox.ConferenceNumber -> BS.ByteString -> IO Bool
  , conferenceTitle              :: Tox.ConferenceNumber -> IO (Maybe BS.ByteString)
  , conferencePeers              :: Tox.ConferenceNumber -> IO [Tox.ConferencePeer]
  , conferenceId                 :: Tox.ConferenceNumber -> IO (Maybe BS.ByteString)
  , conferenceSave               :: Tox.ConferenceNumber -> IO (Maybe SavedConference)
  , conferenceRestore            :: SavedConference -> IO (Maybe Tox.ConferenceNumber)
  , groupNew                     :: Bool -> BS.ByteString -> BS.ByteString -> IO (Maybe GroupNumber)
  , groupInviteFriend            :: GroupNumber -> PublicKey -> IO Bool
  , groupJoin                    :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO (Either Tox.AcceptError GroupNumber)
  , groupChatId                  :: GroupNumber -> IO (Maybe BS.ByteString)
  , groupInviteAccept            :: PublicKey -> BS.ByteString -> BS.ByteString -> BS.ByteString -> IO (Either Tox.AcceptError GroupNumber)
  , groupSendMessage             :: GroupNumber -> Tox.MessageType -> BS.ByteString -> IO (Either Tox.GroupSendError Word32)
  , groupSendPrivateMessage      :: GroupNumber -> Tox.PeerId -> Tox.MessageType -> BS.ByteString -> IO (Either Tox.GroupSendError Word32)
  , groupSendCustomPacket        :: GroupNumber -> Bool -> BS.ByteString -> IO (Either Tox.GroupSendError ())
  , groupSendCustomPrivatePacket :: GroupNumber -> Tox.PeerId -> Bool -> BS.ByteString -> IO (Either Tox.GroupSendError ())
  , groupSetTopic                :: GroupNumber -> BS.ByteString -> IO Bool
  , groupMayTopic                :: GroupNumber -> IO Bool
  , groupSelfRole                :: GroupNumber -> IO (Maybe Tox.Role)
  , groupSelfPublicKey           :: GroupNumber -> IO (Maybe PublicKey)
  , groupPeerCanSpeak            :: GroupNumber -> Tox.PeerId -> IO Bool
  , groupSettings                :: GroupNumber -> IO (Maybe Tox.GroupSettings)
  , groupTopic                   :: GroupNumber -> IO (Maybe BS.ByteString)
  , groupName                    :: GroupNumber -> IO (Maybe BS.ByteString)
  , groupSetNick                 :: GroupNumber -> BS.ByteString -> IO Bool
  , groupSetStatus               :: GroupNumber -> Tox.UserStatus -> IO Bool
  , groupPeers                   :: GroupNumber -> IO [Tox.GroupPeer]
  , groupLeave                   :: GroupNumber -> BS.ByteString -> IO Bool
  , groupSave                    :: GroupNumber -> IO (Maybe SavedGroup)
  , groupRestore                 :: SavedGroup -> IO (Maybe GroupNumber)
  , groupKickPeer                :: GroupNumber -> Tox.PeerId -> IO (Either Tox.ModError ())
  , groupSetRole                 :: GroupNumber -> Tox.PeerId -> Tox.Role -> IO (Either Tox.ModError ())
  , groupSetPassword             :: GroupNumber -> BS.ByteString -> IO Bool
  , groupSetPeerLimit            :: GroupNumber -> Word16 -> IO Bool
  , groupSetPrivacy              :: GroupNumber -> Bool -> IO Bool
  , groupSetTopicLock            :: GroupNumber -> Bool -> IO Bool
  , groupSetVoice                :: GroupNumber -> Tox.Voice -> IO Bool
  , sizes                        :: IO [Tox.Size]
  , timings                      :: IO (Map.Map Text Tox.Lane)
  , traffic                      :: IO [(Tox.Counted, Tox.Count)]
  }

-- | In hs-toxcore's format, whichever library saved it, so profiles move
-- between libraries.
type SavedGroup = BS.ByteString
type SavedConference = BS.ByteString

data StartError
  = BadSaveData
  | PortInUse
  | BadProxy
  | NoMemory
  deriving (Eq, Show)

-- | A Tox library to start Tox with.
data Engine = Engine
  { engineToxcore :: Toxcore
  , engineStart   :: Tox.Options -> [(GroupKind, BS.ByteString)] -> IO (Either StartError Tox)
    -- ^ The groups and conferences to restore, for libraries that only load
    -- them at the start.
  }

hsToxcore :: Engine
hsToxcore = Engine HsToxcore $ \options _ -> either (Left . refused) (Right . running) <$> Tox.new options
  where
    refused = \case
      Tox.BadSaveData -> BadSaveData
      Tox.PortInUse   -> PortInUse

running :: Tox.Tox -> Tox
running tox = Tox
  { kill                         = Tox.kill tox
  , nextEvent                    = Tox.nextEvent tox
  , bootstrap                    = Tox.bootstrap tox
  , addTcpRelay                  = Tox.addTcpRelay tox
  , selfConnectionStatus         = Tox.selfConnectionStatus tox
  , selfPublicKey                = Tox.selfPublicKey tox
  , selfSecretKey                = Tox.selfSecretKey tox
  , selfAddress                  = Tox.selfAddress tox
  , selfGetNospam                = Tox.selfGetNospam tox
  , selfSetNospam                = Tox.selfSetNospam tox
  , selfSetName                  = Tox.selfSetName tox
  , selfSetStatusMessage         = Tox.selfSetStatusMessage tox
  , selfSetStatus                = Tox.selfSetStatus tox
  , friendAdd                    = Tox.friendAdd tox
  , friendAddNoRequest           = Tox.friendAddNoRequest tox
  , friendDelete                 = Tox.friendDelete tox
  , friendSendMessage            = Tox.friendSendMessage tox
  , friendSetTyping              = Tox.friendSetTyping tox
  , friendSendLosslessPacket     = Tox.friendSendLosslessPacket tox
  , friendQueueLosslessPacket    = Tox.friendQueueLosslessPacket tox
  , friendQueueLossyPacket       = Tox.friendQueueLossyPacket tox
  , fileSend                     = Tox.fileSend tox
  , fileControl                  = Tox.fileControl tox
  , fileSeek                     = Tox.fileSeek tox
  , fileSendChunk                = Tox.fileSendChunk tox
  , conferenceNew                = Tox.conferenceNew tox
  , conferenceInvite             = Tox.conferenceInvite tox
  , conferenceJoin               = Tox.conferenceJoin tox
  , conferenceDelete             = Tox.conferenceDelete tox
  , conferenceSendMessage        = Tox.conferenceSendMessage tox
  , conferenceSetTitle           = Tox.conferenceSetTitle tox
  , conferenceTitle              = Tox.conferenceTitle tox
  , conferencePeers              = Tox.conferencePeers tox
  , conferenceId                 = Tox.conferenceId tox
  , conferenceSave               = Tox.conferenceSave tox
  , conferenceRestore            = Tox.conferenceRestore tox
  , groupNew                     = Tox.groupNew tox
  , groupInviteFriend            = Tox.groupInviteFriend tox
  , groupJoin                    = Tox.groupJoin tox
  , groupChatId                  = Tox.groupChatId tox
  , groupInviteAccept            = Tox.groupInviteAccept tox
  , groupSendMessage             = Tox.groupSendMessage tox
  , groupSendPrivateMessage      = Tox.groupSendPrivateMessage tox
  , groupSendCustomPacket        = Tox.groupSendCustomPacket tox
  , groupSendCustomPrivatePacket = Tox.groupSendCustomPrivatePacket tox
  , groupSetTopic                = Tox.groupSetTopic tox
  , groupMayTopic                = Tox.groupMayTopic tox
  , groupSelfRole                = Tox.groupSelfRole tox
  , groupSelfPublicKey           = Tox.groupSelfPublicKey tox
  , groupPeerCanSpeak            = Tox.groupPeerCanSpeak tox
  , groupSettings                = Tox.groupSettings tox
  , groupTopic                   = Tox.groupTopic tox
  , groupName                    = Tox.groupName tox
  , groupSetNick                 = Tox.groupSetNick tox
  , groupSetStatus               = Tox.groupSetStatus tox
  , groupPeers                   = Tox.groupPeers tox
  , groupLeave                   = Tox.groupLeave tox
  , groupSave                    = Tox.groupSave tox
  , groupRestore                 = Tox.groupRestore tox
  , groupKickPeer                = Tox.groupKickPeer tox
  , groupSetRole                 = Tox.groupSetRole tox
  , groupSetPassword             = Tox.groupSetPassword tox
  , groupSetPeerLimit            = Tox.groupSetPeerLimit tox
  , groupSetPrivacy              = Tox.groupSetPrivacy tox
  , groupSetTopicLock            = Tox.groupSetTopicLock tox
  , groupSetVoice                = Tox.groupSetVoice tox
  , sizes                        = Tox.sizes tox
  , timings                      = Tox.timings tox
  , traffic                      = Tox.traffic tox
  }
