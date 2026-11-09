{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | The state of one profile.
module Tox.Client
  ( Client (..)
  , initialClient
  ) where

import qualified Data.ByteString     as BS
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text)
import           Data.Time           (UTCTime)
import qualified Tox.Application.Tox as Tox

import           Tox.Client.Home     (Home)
import           Tox.Client.Linked   (Linked)
import           Tox.Client.Types

data Client = Client
  { clientProfile      :: Profile
  , clientProfileId    :: ProfileId
  , clientConnection   :: Connection
  , clientContacts     :: Map ContactId Contact
  , clientRequests     :: Map ContactId Text
    -- ^ Friend requests and their messages.
  , clientFormer       :: Map ContactId Contact
    -- ^ Deleted with the chat kept. They come back when added again.
  , clientDeclined     :: Map ContactId Text
    -- ^ Rejected friend requests. They stay hidden while the message stays
    -- the same (Tox sends them again and again).
  , clientDrafts       :: Map ContactId Text
  , clientStaged       :: Map ContactId [Staged]
    -- ^ In the order picked.
  , clientReplies      :: Map ContactId MessageId
    -- ^ Message each chat's draft replies to.
  , clientGroups       :: Map GroupId Group
  , clientInvites      :: Map (ContactId, GroupId) Invite
  , clientGroupDrafts  :: Map GroupId Text
  , clientGroupReplies :: Map GroupId MessageId
  , clientTransfers    :: Map (ContactId, FileRef) MessageId
    -- ^ Messages of the transfers Tox has going.
  , clientCall         :: Maybe Call
  , clientTyping       :: Set ContactId
    -- ^ Friends typing to us now.
  , clientTypingTo     :: Map ContactId UTCTime
    -- ^ Friends we told we're typing, and when we last typed to them.
  , clientAvatar       :: Maybe BS.ByteString
    -- ^ Hash of our avatar.
  , clientAvatars      :: Map BS.ByteString BS.ByteString
    -- ^ Our and friends' avatars as PNGs, by SHA-256.
  , clientAccount      :: AccountSettings
  , clientDevice       :: DeviceSettings
  , clientIds          :: Ids
  , clientLinked       :: Maybe Linked
    -- ^ If this profile is linked to an account on another device. The
    -- chats are then derived from it.
  , clientHome         :: Maybe Home
    -- ^ If the account has devices linked to it here.
  } deriving (Eq, Show)

initialClient :: Client
initialClient = Client
  { clientProfile = Profile
      { profileNickname = "Tox User"
      , profileStatus = "Toxing on Haskell"
      , profileAway = Tox.Online
      , profileToxId = ""
      }
  , clientProfileId = ProfileId 0
  , clientConnection = Offline
  , clientContacts = Map.empty
  , clientRequests = Map.empty
  , clientFormer = Map.empty
  , clientDeclined = Map.empty
  , clientDrafts = Map.empty
  , clientStaged = Map.empty
  , clientReplies = Map.empty
  , clientGroups = Map.empty
  , clientInvites = Map.empty
  , clientGroupDrafts = Map.empty
  , clientGroupReplies = Map.empty
  , clientTransfers = Map.empty
  , clientCall = Nothing
  , clientTyping = Set.empty
  , clientTypingTo = Map.empty
  , clientAvatar = Nothing
  , clientAvatars = Map.empty
  , clientAccount = AccountSettings autoAccepted True
  , clientDevice = DeviceSettings True True True True Nothing
  , clientIds = Ids "" 0
  , clientLinked = Nothing
  , clientHome = Nothing
  }
