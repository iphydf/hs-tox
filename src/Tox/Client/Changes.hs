{-# LANGUAGE StrictData #-}
-- | What changed between two states of a client, for the app to store.
module Tox.Client.Changes
  ( Change (..)
  , Texts (..)
  , changes
  ) where

import qualified Data.ByteString   as BS
import qualified Data.Map.Strict   as Map
import qualified Data.Set          as Set
import           Data.Text         (Text)
import           Data.Word         (Word64)
import qualified Tox.Sync.Event    as Sync
import qualified Tox.Sync.Id       as Sync
import qualified Tox.Sync.Replica  as Replica
import qualified Tox.Sync.Store    as Sync

import           Tox.Client
import qualified Tox.Client.Home   as Home
import           Tox.Client.Home   (Home)
import qualified Tox.Client.Linked as Linked
import           Tox.Client.Linked (Linked)
import           Tox.Client.Types

-- | Text kept per contact.
data Texts = Requests | Declined | Drafts
  deriving (Eq, Show, Enum, Bounded)

data Change
  = ProfileChanged Profile (Maybe BS.ByteString)
    -- ^ With the hash of our avatar.
  | IdsUsed Word64
  | AccountChanged AccountSettings
  | DeviceChanged DeviceSettings
  | AvatarAdded BS.ByteString BS.ByteString
    -- ^ PNG by its SHA-256.
  | AvatarDropped BS.ByteString
  | ContactChanged ContactId Bool Contact
    -- ^ False if deleted with its chat kept. Its messages change separately.
  | ContactDeleted ContactId
    -- ^ With its chat.
  | MessageChanged ContactId Message
    -- ^ New ones in the order of their chat.
  | GroupChanged GroupId Group (Maybe Text)
    -- ^ With its draft. Its entries change separately.
  | GroupDeleted GroupId
  | EntryChanged GroupId GroupEntry
  | InviteChanged ContactId GroupId Invite
  | InviteDeleted ContactId GroupId
  | StagedChanged ContactId [Staged]
  | TextChanged Texts ContactId (Maybe Text)
  | PairingChanged (Maybe Text)
  | EventAdded Sync.EventId Word64 BS.ByteString
    -- ^ A linked account's, with its rank (its chats come from these), or
    -- one the home wrote for its devices.
  deriving (Eq, Show)

-- | For a linked account, its new events instead of its chats (they come
-- from the events).
changes :: Client -> Client -> [Change]
changes old new =
  [ProfileChanged (clientProfile new) (clientAvatar new) | differs clientProfile || differs clientAvatar]
  ++ [IdsUsed (idsUsed (clientIds new)) | differs (idsUsed . clientIds)]
  ++ [AccountChanged (clientAccount new) | differs clientAccount]
  ++ [DeviceChanged (clientDevice new) | differs clientDevice]
  ++ [AvatarAdded hash png | (hash, png) <- Map.toList (Map.difference (clientAvatars new) (clientAvatars old))]
  ++ [AvatarDropped hash | hash <- Map.keys (Map.difference (clientAvatars old) (clientAvatars new))]
  ++ maybe own (linked old) (clientLinked new)
  ++ maybe [] (home old) (clientHome new)
  ++ [ StagedChanged cid (Map.findWithDefault [] cid (clientStaged new))
     | cid <- Set.toList (Map.keysSet (clientStaged old) <> Map.keysSet (clientStaged new))
     , Map.lookup cid (clientStaged old) /= Map.lookup cid (clientStaged new) ]
  ++ texts Drafts clientDrafts
  where
    differs f = f old /= f new
    texts kind f =
      [TextChanged kind cid (Just text) | (cid, text) <- Map.toList (f new), Map.lookup cid (f old) /= Just text]
      ++ [TextChanged kind cid Nothing | cid <- Map.keys (Map.difference (f old) (f new))]
    own = contacts ++ groups ++ invites ++ texts Requests clientRequests ++ texts Declined clientDeclined
    -- Those deleted with their chats kept too.
    everyone m = Map.union ((,) True <$> clientContacts m) ((,) False <$> clientFormer m)
    contacts =
      concat
        [ [ContactChanged cid wanted contact | fmap (fmap bare) before /= Just (wanted, bare contact)]
          ++ [MessageChanged cid m | m <- since messageId kept (contactMessages contact), Map.lookup (messageId m) kept /= Just m]
        | (cid, now@(wanted, contact)) <- Map.toList (everyone new)
        , let before = Map.lookup cid (everyone old)
        , before /= Just now
        , let kept = Map.fromList [(messageId m, m) | m <- maybe [] (contactMessages . snd) before] ]
      ++ [ContactDeleted cid | cid <- Map.keys (Map.difference (everyone old) (everyone new))]
    bare c = c { contactMessages = [], contactEarlier = False }
    groupRow m gid = (,) <$> Map.lookup gid (clientGroups m) <*> Just (Map.lookup gid (clientGroupDrafts m))
    groups =
      concat
        [ [GroupChanged gid g draft | fmap untold before /= Just (untold now)]
          -- All new ones, as history from members goes in among older
          -- entries. A page from the store is written again unchanged.
          ++ [EntryChanged gid e | e <- groupEntries g, Map.lookup (entryId e) kept /= Just e]
        | gid <- Map.keys (clientGroups new)
        , Just now@(g, draft) <- [groupRow new gid]
        , let before = groupRow old gid
        , before /= Just now
        , let kept = Map.fromList [(entryId e, e) | e <- maybe [] (groupEntries . fst) before] ]
      ++ [GroupDeleted gid | gid <- Map.keys (Map.difference (clientGroups old) (clientGroups new))]
    untold (g, draft) = (g { groupEntries = [], groupEarlier = False }, draft)
    invites =
      [InviteChanged cid gid invite | ((cid, gid), invite) <- Map.toList (clientInvites new), Map.lookup (cid, gid) (clientInvites old) /= Just invite]
      ++ [InviteDeleted cid gid | (cid, gid) <- Map.keys (Map.difference (clientInvites old) (clientInvites new))]

-- | The items from the first one we already had (any before it came from the
-- store), or all of them if we had none.
since :: Ord k => (a -> k) -> Map.Map k a -> [a] -> [a]
since key had now = case break ((`Map.member` had) . key) now of
  (_, [])   -> now
  (_, rest) -> rest

linked :: Client -> Linked -> [Change]
linked old l =
  [PairingChanged (Linked.linkedPairing l) | (Linked.linkedPairing <$> clientLinked old) /= Just (Linked.linkedPairing l)]
  ++ [ EventAdded i (Sync.eventRank e) bytes
     | i <- Set.toList (Set.difference (Sync.storedIds (events l)) before)
     , Just e <- [Sync.lookupEvent i (events l)], Just bytes <- [Sync.eventBytes i (events l)] ]
  where
    events = Replica.replicaStore . Linked.linkedReplica
    before = maybe Set.empty (Sync.storedIds . events) (clientLinked old)

home :: Client -> Home -> [Change]
home old h =
  [ EventAdded i (Sync.eventRank e) bytes
  | i <- Set.toList (Set.difference (Sync.storedIds (Home.homeStore h)) before)
  , Just e <- [Sync.lookupEvent i (Home.homeStore h)], Just bytes <- [Sync.eventBytes i (Home.homeStore h)] ]
  where before = maybe Set.empty (Sync.storedIds . Home.homeStore) (clientHome old)
