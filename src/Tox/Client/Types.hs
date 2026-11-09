{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | What a Tox client keeps: contacts and their messages, groups, invites,
-- calls, the profile and settings.
module Tox.Client.Types
  ( ContactId (..)
  , toxIdLength
  , toxId
  , unhex
  , Identity (..)
  , MessageId (..)
  , Ids (..)
  , nextId
  , unused
  , Delivery (..)
  , Message (..)
  , inUse
  , CallRecord (..)
  , Attachment (..)
  , FileState (..)
  , FileRef (..)
  , autoAccepted
  , Staged (..)
  , AccountSettings (..)
  , DeviceSettings (..)
  , Contact (..)
  , Mute (..)
  , muted
  , mutedBy
  , GroupId (..)
  , Member (..)
  , GroupPhase (..)
  , GroupKind (..)
  , Group (..)
  , newGroup
  , MemberAction (..)
  , memberActions
  , GroupEntry (..)
  , entryInUse
  , entryText
  , Happening (..)
  , GroupFile (..)
  , groupFileName
  , Invite (..)
  , Founding (..)
  , groupMuted
  , mentions
  , memberLabel
  , noticeText
  , groupNick
  , Profile (..)
  , ProfileId (..)
  , Account (..)
  , Connection (..)
  , Call (..)
  , callTarget
  , newCall
  , CallPhase (..)
  , Glare (..)
  , glareDelay
  , glareWait
  , meeting
  , ringTimeout
  , typingPause
  , shownText
  ) where

import qualified Crypto.Hash.SHA256     as SHA256
import           Data.Bits              (shiftR, xor)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import           Data.Char              (isAlphaNum)
import           Data.Either            (fromRight)
import           Data.Int               (Int64)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe, isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           Data.Time              (NominalDiffTime, UTCTime)
import           Data.Word              (Word32, Word64, Word8)
import qualified Tox.Application.Tox    as Tox
import qualified Tox.Share.Info         as Info
import           Tox.Share.Info         (Info)

-- | Friend's public key in upper case hex.
newtype ContactId = ContactId Text
  deriving (Eq, Ord, Show)

toxIdLength :: Int
toxIdLength = 76

-- | Tox ID from a public key and nospam in hex, with the checksum.
toxId :: Text -> Text -> Text
toxId key nospam = Text.toUpper (address <> hex (checksum (unhex address)))
  where address = key <> nospam

-- | 2 bytes: XOR of all the even bytes, and of all the odd ones.
checksum :: BS.ByteString -> BS.ByteString
checksum bytes = BS.pack [foldr xor 0 evens, foldr xor 0 odds]
  where
    indexed = zip [0 :: Int ..] (BS.unpack bytes)
    evens = [b | (i, b) <- indexed, even i]
    odds = [b | (i, b) <- indexed, odd i]

hex :: BS.ByteString -> Text
hex = Text.decodeUtf8 . Base16.encode

-- | Empty if it isn't hex.
unhex :: Text -> BS.ByteString
unhex = fromRight BS.empty . Base16.decode . Text.encodeUtf8

data Identity = Identity
  { identitySecretKey :: BS.ByteString
  , identityNospam    :: Word32
  } deriving (Eq, Show)

-- | From the profile's 'Ids', or a linked account's event ID (32 bytes either
-- way).
newtype MessageId = MessageId BS.ByteString
  deriving (Eq, Ord, Show)

-- | Where the profile's new message IDs come from, so they're unique
-- without any randomness here.
data Ids = Ids
  { idsSeed :: BS.ByteString
    -- ^ Random, made with the profile.
  , idsUsed :: Word64
  } deriving (Eq, Show)

-- | SHA-256 of the seed and the count.
nextId :: Ids -> MessageId
nextId (Ids seed used) = MessageId (SHA256.hash (seed <> BS.pack [fromIntegral (used `shiftR` s) | s <- [56, 48 .. 0 :: Int]]))

-- | The next IDs, in order.
unused :: Ids -> [MessageId]
unused ids = nextId ids : unused ids { idsUsed = idsUsed ids + 1 }

data Delivery = Pending | Sent | Delivered
  deriving (Eq, Show)

data Message = Message
  { messageMine      :: Bool
  , messageText      :: Text
    -- ^ File name for files.
  , messageTime      :: UTCTime
  , messageDelivery  :: Delivery
    -- ^ Always 'Delivered' for theirs.
  , messageFile      :: Maybe Attachment
  , messageCall      :: Maybe CallRecord
    -- ^ 'messageTime' is when it started ringing.
  , messageParts     :: Int
    -- ^ How many of our 'parts' the friend confirmed.
  , messageDelivered :: Maybe UTCTime
    -- ^ When the friend got all of ours, if we know.
  , messageKind      :: Tox.MessageType
  , messageRaw       :: Maybe BS.ByteString
    -- ^ Their bytes as Tox carried them, which the text may not show all of.
    -- Nothing for messages from before we kept them.
  , messageId        :: MessageId
  } deriving (Eq, Show)

-- | Ours until the friend has it (we send it again when they're back), or a
-- file not finished. Chats keep these in memory.
inUse :: Message -> Bool
inUse m = (messageMine m && isNothing (messageFile m) && isNothing (messageCall m) && messageDelivery m /= Delivered)
  || maybe False ((`elem` [Waiting, Moving, Interrupted]) . attachmentState) (messageFile m)

data CallRecord = CallRecord
  { recordVideo    :: Bool
  , recordDuration :: Maybe NominalDiffTime
    -- ^ Nothing if nobody answered.
  , recordMissed   :: Bool
    -- ^ Theirs, and we never answered.
  } deriving (Eq, Show)

data Attachment = Attachment
  { attachmentSize  :: Word64
  , attachmentPath  :: FilePath
    -- ^ Theirs gets written there as it comes in.
  , attachmentDone  :: Word64
    -- ^ Bytes sent or received so far.
  , attachmentState :: FileState
  , attachmentId    :: Maybe BS.ByteString
    -- ^ Tox file ID, so we can resume when it's offered again. Nothing for
    -- files from before we kept it.
  } deriving (Eq, Show)

data FileState
  = Waiting
    -- ^ For the friend to accept ours, or the user to accept theirs.
  | Moving
  | Complete
  | Interrupted
    -- ^ Theirs, cut off by going offline or a restart. Resumes when the
    -- friend offers it again.
  | Cancelled
    -- ^ By either side, or cut off and can't resume.
  deriving (Eq, Show)

-- | Tox's file number, valid while the friend stays online.
data FileRef = Sending Word8 | Receiving Word8
  deriving (Eq, Ord, Show)

-- | Default auto-accept limit for their files.
autoAccepted :: Word64
autoAccepted = 4 * 1024 * 1024

-- | Picked for a chat, sent with its draft.
data Staged
  = StagedFile FilePath Text Word64 BS.ByteString
    -- ^ Path of our copy, name, size and a new file ID.
  | StagedPlace Double Double
  deriving (Eq, Show)

-- | Settings of the account, the same on all its devices.
data AccountSettings = AccountSettings
  { settingAutoAccept :: Word64
    -- ^ Accept their files up to this size without asking (0 = never).
  , settingTyping     :: Bool
    -- ^ Tell friends when we're typing, and show when they are.
  } deriving (Eq, Show)

-- | Settings of this device only. Changes apply when Tox restarts.
data DeviceSettings = DeviceSettings
  { settingUdp   :: Bool
  , settingIpv6  :: Bool
  , settingLan   :: Bool
  , settingStay  :: Bool
    -- ^ Stay online while the app is closed, and start on boot.
  , settingProxy :: Maybe Tox.Proxy
    -- ^ For TCP relays. UDP and local discovery are off with a proxy.
  } deriving (Eq, Show)

data Contact = Contact
  { contactName          :: Maybe Text
  , contactOnline        :: Bool
  , contactMessages      :: [Message]
    -- ^ Oldest first.
  , contactUnread        :: Int
  , contactRequest       :: Maybe (Text, Text)
    -- ^ Our friend request (Tox ID and message) until they're first online.
    -- Tox forgets it on restart, so we send it again.
  , contactStatusMessage :: Text
  , contactStatus        :: Tox.UserStatus
  , contactMute          :: Maybe Mute
  , contactAvatar        :: Maybe BS.ByteString
    -- ^ Hash of their avatar in 'clientAvatars'.
  , contactPictures      :: Maybe Text
    -- ^ Picture format we send them, if not as they are.
  , contactEarlier       :: Bool
    -- ^ The store has messages before these.
  } deriving (Eq, Show)

data Mute = MutedUntil UTCTime | MutedAlways
  deriving (Eq, Show)

muted :: UTCTime -> Contact -> Bool
muted now = mutedBy now . contactMute

mutedBy :: UTCTime -> Maybe Mute -> Bool
mutedBy now = \case
  Just (MutedUntil end) -> now < end
  Just MutedAlways      -> True
  Nothing               -> False

-- | Chat ID in upper case hex. Not the group number (Tox numbers groups anew
-- each time it starts).
newtype GroupId = GroupId Text
  deriving (Eq, Ord, Show)

-- | Key in hex, as used in that group.
data Member = Member
  { memberKey  :: Text
  , memberName :: Text
  } deriving (Eq, Show)

data GroupPhase
  = Joining
    -- ^ Until we have the group state from peers, also after each start.
  | InGroup
  | Refused Tox.JoinFail
  | Kicked
  | Gone
    -- ^ We left. The chat stays.
  deriving (Eq, Show)

-- | NGC groups and the old conferences. We join audio conferences for their
-- text only.
data GroupKind = NgcGroup | TextConference | AudioConference
  deriving (Eq, Show)

-- | Keyed by the chat ID, or the conference ID.
data Group = Group
  { groupName     :: Text
  , groupKind     :: GroupKind
  , groupPhase    :: GroupPhase
  , groupEntries  :: [GroupEntry]
    -- ^ Oldest first.
  , groupUnread   :: Int
  , groupMembers  :: Map Tox.PeerId Member
    -- ^ Others, by Tox's peer number. Not saved.
  , groupSaved    :: Maybe BS.ByteString
    -- ^ Last blob Tox gave us to rejoin it.
  , groupMute     :: Maybe Mute
  , groupMentions :: Bool
    -- ^ Notify about messages naming us while muted (on by default).
  , groupPictures :: Maybe Text
    -- ^ Picture format we send in, if not as they are.
  , groupTopic    :: Text
  , groupMayTopic :: Bool
    -- ^ Whether Tox would take a topic from us. Not saved.
  , groupSince    :: Maybe UTCTime
    -- ^ When we last got in. Tox reports everyone already there as joining
    -- while it meets them. Not saved.
  , groupRoles    :: Map Tox.PeerId Tox.Role
    -- ^ Not saved.
  , groupStatuses :: Map Tox.PeerId Tox.UserStatus
    -- ^ Members who are away or busy. Not saved.
  , groupSelfRole :: Maybe Tox.Role
    -- ^ Not saved.
  , groupSettings :: Maybe Tox.GroupSettings
    -- ^ Founder's settings. Not saved.
  , groupRelayBot :: Maybe Text
    -- ^ Key of the bot whose messages show under the names it relays.
  , groupEarlier  :: Bool
    -- ^ The store has entries before these.
  } deriving (Eq, Show)

newGroup :: Text -> GroupKind -> GroupPhase -> Group
newGroup name kind phase = Group
  { groupName = name, groupKind = kind, groupPhase = phase, groupEntries = [], groupUnread = 0
  , groupMembers = Map.empty, groupSaved = Nothing, groupMute = Nothing, groupMentions = True, groupPictures = Nothing, groupTopic = ""
  , groupMayTopic = False, groupSince = Nothing, groupRoles = Map.empty, groupStatuses = Map.empty, groupSelfRole = Nothing
  , groupSettings = Nothing, groupRelayBot = Nothing, groupEarlier = False }

-- | As in toxcore, the founder can change or kick anyone else, and moderators
-- only users and observers.
data MemberAction = MakeModerator | MakeUser | MakeObserver | Kick
  deriving (Eq, Show)

memberActions :: Maybe Tox.Role -> Tox.Role -> [MemberAction]
memberActions ours theirs = case ours of
  Just Tox.Founder | theirs /= Tox.Founder ->
    [MakeModerator | theirs /= Tox.Moderator] ++ [MakeUser | theirs /= Tox.User]
      ++ [MakeObserver | theirs /= Tox.Observer] ++ [Kick]
  Just Tox.Moderator | theirs `elem` [Tox.User, Tox.Observer] ->
    [MakeUser | theirs /= Tox.User] ++ [MakeObserver | theirs /= Tox.Observer] ++ [Kick]
  _ -> []

data GroupEntry = GroupEntry
  { entryTime :: UTCTime
  , entryWhat :: Happening
  , entryId   :: MessageId
  } deriving (Eq, Show)

-- | Ours until it went (we send it again once in the group).
entryInUse :: GroupEntry -> Bool
entryInUse e = case entryWhat e of
  Said Nothing _ _ Pending _ -> True
  PrivateTo _ _ _ Pending    -> True
  _                          -> False

-- | What a message in the group says, to reply to, copy or forward.
entryText :: GroupEntry -> Maybe Text
entryText e = case entryWhat e of
  Said _ _ body _ _    -> Just (shownText body)
  PrivateFrom _ _ body -> Just (shownText body)
  PrivateTo _ _ body _ -> Just (shownText body)
  _                    -> Nothing

data Happening
  = Said (Maybe Member) Tox.MessageType Text Delivery (Maybe Word32)
    -- ^ Nothing for ours. Ours only go 'Pending' to 'Sent' (groups confirm
    -- nothing). Then the Tox message ID, once Tox gave one, which history
    -- sync matches by.
  | PrivateFrom Member Tox.MessageType Text
  | PrivateTo Member Tox.MessageType Text Delivery
  | Moderated (Maybe Member) (Maybe Member) Tox.Moderation
    -- ^ Who did it to whom. Nothing means us.
  | Arrived Member
  | Departed Member
  | TopicSet (Maybe Member) Text
    -- ^ Nothing for ours.
  | Noted Text
    -- ^ From a linked account's home.
  | SharedFile (Maybe Member) GroupFile (Maybe Word32)
    -- ^ A file shared in the group (Nothing for ours), and the Tox message
    -- ID of its announcement.
  deriving (Eq, Show)

-- | Their name, or the start of their key.
memberLabel :: Member -> Text
memberLabel m
  | not (Text.null (memberName m)) = memberName m
  | not (Text.null (memberKey m)) = Text.take 8 (memberKey m)
  | otherwise = "Someone"

-- | A line saying what happened in a group, if it isn't a message or file:
-- joins, leaves, topics and moderation.
noticeText :: Happening -> Maybe Text
noticeText = \case
  Arrived who -> Just (memberLabel who <> " joined")
  Departed who -> Just (memberLabel who <> " left")
  TopicSet who topic -> Just (maybe "You" memberLabel who <> " set the topic: " <> topic)
  Noted t -> Just t
  Moderated by whom m -> Just $ maybe "You" memberLabel by <> case m of
    Tox.ModKick      -> " removed " <> target
    Tox.ModModerator -> " made " <> target <> " a moderator"
    Tox.ModUser      -> " made " <> target <> " a member who can speak"
    Tox.ModObserver  -> " made " <> target <> " an observer"
    where target = maybe "you" memberLabel whom
  _ -> Nothing

-- | A file shared in a group. We only download it when asked to.
data GroupFile = GroupFile
  { groupFileId   :: BS.ByteString
    -- ^ The SHA1 of its info.
  , groupFileInfo :: Maybe Info
    -- ^ Once it came. We keep it to download the file after a restart.
  , groupFileData :: Maybe Attachment
    -- ^ Once its info came. 'Waiting' until we download it.
  , groupFileRate :: Double
    -- ^ Bytes a second it comes at, while it does.
  } deriving (Eq, Show)

groupFileName :: GroupFile -> Maybe Text
groupFileName = fmap (Text.decodeUtf8Lenient . Info.name) . groupFileInfo

data Invite = Invite
  { inviteKind     :: GroupKind
  , inviteName     :: Text
    -- ^ Empty for conferences.
  , inviteData     :: BS.ByteString
    -- ^ As Tox gave it (we need it to accept).
  , inviteAnswered :: Maybe Text
    -- ^ Accepted, with the password, until the friend is online.
  } deriving (Eq, Show)

-- | Private groups are joined by invite, public ones also by chat ID.
-- Conferences are for older clients.
data Founding = PrivateGroup | PublicGroup | NewConference
  deriving (Eq, Ord, Show, Enum, Bounded)

groupMuted :: UTCTime -> Group -> Bool
groupMuted now = mutedBy now . groupMute

-- | Whether the text has the name as a whole word, ignoring case. Words are
-- letters, digits and underscores.
mentions :: Text -> Text -> Bool
mentions name text
  | Text.null needle = False
  | otherwise = any whole (Text.breakOnAll needle haystack)
  where
    needle = Text.toCaseFold (Text.strip name)
    haystack = Text.toCaseFold text
    whole (before, rest) = not (wordy (Text.takeEnd 1 before))
      && not (wordy (Text.take 1 (Text.drop (Text.length needle) rest)))
    wordy = Text.any (\c -> isAlphaNum c || c == '_')

-- | Our name in groups (Tox takes no empty one).
groupNick :: Profile -> Text
groupNick p = if Text.null (Text.strip (profileNickname p)) then "Tox User" else profileNickname p

data Profile = Profile
  { profileNickname :: Text
  , profileStatus   :: Text
  , profileAway     :: Tox.UserStatus
  , profileToxId    :: Text
  } deriving (Eq, Show)

-- | Database ID of an account.
newtype ProfileId = ProfileId Int64
  deriving (Eq, Ord, Show)

data Account = Account
  { accountId   :: ProfileId
  , accountName :: Text
  , accountKey  :: Text
    -- ^ Public key in hex. Empty until Tox first made its keys.
  } deriving (Eq, Show)

-- | Worst first.
data Connection = Offline | ViaTcp | ViaUdp
  deriving (Eq, Ord, Show)

-- | There's at most 1.
data Call = Call
  { callContact   :: ContactId
  , callPhase     :: CallPhase
  , callStarted   :: UTCTime
    -- ^ When it started ringing.
  , callMine      :: Bool
  , callVideo     :: Bool
    -- ^ Whether it started as a video call.
  , callCamera    :: Bool
    -- ^ Whether we send video.
  , callPeerVideo :: Bool
  , callMuted     :: Bool
    -- ^ Our microphone (we drop its frames while muted).
  , callGlare     :: Glare
  , callEntry     :: Maybe MessageId
    -- ^ Chat entry, added at the start so it's still there if the app stops
    -- during the call.
  , callVia       :: Maybe ContactId
    -- ^ Linked account's home, which relays the audio and video.
  , callDevice    :: Maybe ContactId
    -- ^ The account's device that took it, which we relay it to.
  } deriving (Eq, Show)

callTarget :: Call -> ContactId
callTarget c = fromMaybe (callContact c) (callVia c)

newCall :: ContactId -> CallPhase -> UTCTime -> Bool -> Call
newCall cid phase time video =
  Call cid phase time (phase == Calling) video False False False Clear Nothing Nothing Nothing

data CallPhase
  = Calling
  | Ringing
    -- ^ They call us.
  | Talking UTCTime
  deriving (Eq, Show)

-- | If both ends call at once, Tox fails both calls. The end with the higher
-- key calls again and the other answers that (as other clients do).
data Glare
  = Clear
  | Glared UTCTime
  | Settling
    -- ^ Called again or answered, so another failure ends it.
  deriving (Eq, Show)

-- | How long the end calling again waits for the first call to go away, and
-- how long the other waits for the new call.
glareDelay, glareWait :: NominalDiffTime
glareDelay = 2
glareWait = 8

-- | How long Tox takes to meet the peers already in a group after joining.
meeting :: NominalDiffTime
meeting = 15

ringTimeout :: NominalDiffTime
ringTimeout = 60

-- | What shows of a message's text: up to its first zero byte. What follows
-- stays in the message, as some clients put extras there, or hide things.
shownText :: Text -> Text
shownText = Text.takeWhile (/= '\0')

-- | How long after the user last typed a friend hears they stopped.
typingPause :: NominalDiffTime
typingPause = 3
