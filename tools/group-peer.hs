{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | hTox's client without its UI, for testing groups against other Tox
-- clients. It adds a friend, founds a private group once the friend is
-- online and invites them, says something and shares a file when someone
-- joins, and downloads every file shared in the group. It prints what happens, a line
-- each, for scripts to wait on.
module Main (main) where

import           Control.Concurrent     (newChan, readChan, writeChan)
import           Control.Monad          (forM_, forever, (>=>))
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as Char8
import           Data.IORef             (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as Text
import           Data.Time              (getCurrentTime)
import           System.Environment     (getArgs)
import           System.FilePath        (takeFileName)
import           System.IO              (BufferMode (..), hSetBuffering, stdout)

import           Tox.Client
import qualified Tox.Client.Network     as Network
import           Tox.Client.Nodes       (Node (..))
import           Tox.Client.Types
import           Tox.Client.Update
import qualified Tox.Core.Lanes         as Lanes
import qualified Tox.Share.Info         as Info

data Config = Config
  { nodes    :: [Node]
  , friend   :: Maybe Text.Text
  , received :: FilePath
  , says     :: Maybe Text.Text
  , shares   :: Maybe FilePath
  , lan      :: Bool
  }

config :: [String] -> Config
config = go (Config [] Nothing "." Nothing Nothing True)
  where
    go c = \case
      "--node" : spec : rest -> go c { nodes = nodes c <> [node spec] } rest
      "--add" : address : rest -> go c { friend = Just (Text.pack address) } rest
      "--received" : dir : rest -> go c { received = dir } rest
      "--say" : text : rest -> go c { says = Just (Text.pack text) } rest
      "--share" : path : rest -> go c { shares = Just path } rest
      "--no-lan" : rest -> go c { lan = False } rest
      [] -> c
      arg : _ -> error ("unknown argument " <> arg)

-- | HOST:UDP:TCP:KEY, a node that is also a TCP relay (hTox bootstraps only
-- from those).
node :: String -> Node
node spec = case splitOn spec of
  [host, udp, tcp, key] | Right k <- Base16.decode (Char8.pack key) -> Node host (read udp) (Just (read tcp)) k True
  _ -> error ("bad node " <> spec)
  where
    splitOn s = case break (== ':') s of
      (part, _ : rest) -> part : splitOn rest
      (part, [])       -> [part]

hex :: BS.ByteString -> String
hex = Char8.unpack . Base16.encode

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  c <- config <$> getArgs
  lanes <- Lanes.newLanes
  let device = (clientDevice initialClient) { settingLan = lan c }
  network <- either (fail . show) pure
    =<< Network.start (putStrLn . ("LOG " <>) . Text.unpack) lanes Nothing device (received c) (nodes c)
  events <- newChan
  client <- newIORef initialClient
  _ <- Network.run network (writeChan events)
  forM_ (friend c) $ \address -> writeChan events (AddFriend address "group test")
  forever $ do
    event <- readChan events
    old <- readIORef client
    let (new, commands) = update (Viewing Nothing False False 50) event old
    writeIORef client new
    mapM_ (writeChan events) =<< reported c old new event
    forM_ commands $ Network.perform network >=> mapM_ (writeChan events)

-- | Prints the event, and what we do about it.
reported :: Config -> Client -> Client -> Event -> IO [Event]
reported c old new = \case
  Started address -> [] <$ putStrLn ("TOX_ID " <> Text.unpack address)
  Connected how -> [] <$ putStrLn ("CONNECTED " <> show how)
  Requested cid _ -> [] <$ putStrLn ("REQUESTED " <> show cid)
  FriendOnline cid True -> do
    putStrLn "FRIEND_ONLINE"
    -- Invited once the group exists.
    pure $ if Map.null (clientGroups new) then [Found PrivateGroup "htox-test"] else [InviteFriend gid cid | gid <- Map.keys (clientGroups new)]
  GroupCreated gid _ _ _ -> do
    putStrLn "GROUP_CREATED"
    pure [InviteFriend gid cid | (cid, k) <- Map.toList (clientContacts new), contactOnline k]
  InviteSent _ _ -> [] <$ putStrLn "INVITED"
  ToxRefused why -> [] <$ putStrLn ("REFUSED " <> show why)
  MemberJoined gid _ n m -> do
    putStrLn ("MEMBER_JOINED " <> show n <> " " <> Text.unpack (memberName m))
    now <- getCurrentTime
    -- Once in each group.
    let quiet = not (any ours (maybe [] groupEntries (Map.lookup gid (clientGroups old))))
    size <- maybe (pure 0) (\path -> fromIntegral . BS.length <$> BS.readFile path) (shares c)
    pure $ concat [[SetDraft (GroupChat gid) text, Send (GroupChat gid) now] | Just text <- [says c], quiet]
      ++ [WroteGroupFile gid now path (Text.pack (takeFileName path)) size | Just path <- [shares c], quiet]
  GroupSent _ _ mid -> [] <$ putStrLn ("SENT " <> show mid)
  GroupReceived _ _ _ _ text mid -> [] <$ putStrLn ("MESSAGE " <> show mid <> " " <> Text.unpack text)
  GroupHistoryWanted _ n range -> [] <$ putStrLn ("HISTORY_WANTED " <> show n <> " " <> show range)
  GroupFileShared _ _ _ mid fid -> [] <$ putStrLn ("SHARED " <> show mid <> " " <> hex fid)
  GroupFileOffered _ _ offered -> [] <$ putStrLn (maybe "NOT_OFFERED" (\(inf, mid) -> "OFFERED " <> show mid <> " " <> hex (Info.hash inf)) offered)
  GroupRecalled gid _ _ _ recalled -> do
    putStrLn ("RECALLED " <> show (length recalled))
    let entries = maybe [] groupEntries (Map.lookup gid (clientGroups new))
    [] <$ putStrLn ("ENTRIES " <> show (length entries))
  GroupFileLearned gid fid inf -> do
    putStrLn ("LEARNED " <> Char8.unpack (Info.name inf) <> " " <> show (Info.size inf))
    pure [DownloadGroupFile gid fid]
  GroupFileEnded gid fid whole -> do
    forM_ [attachmentPath a | GroupEntry _ (SharedFile _ (GroupFile f _ (Just a) _) _) _ <- maybe [] groupEntries (Map.lookup gid (clientGroups new)), f == fid] $ \path -> do
      bytes <- BS.readFile path
      putStrLn ((if whole then "DOWNLOADED " else "FAILED ") <> path <> " " <> show (BS.length bytes) <> " " <> hex (Info.sha1 bytes))
    pure []
  _ -> pure []
  where
    ours = \case
      GroupEntry _ (Said Nothing _ _ _ _) _ -> True
      _ -> False
