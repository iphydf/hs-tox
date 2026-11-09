{-# LANGUAGE OverloadedStrings #-}
module Tox.Client.LinkedSpec (spec) where

import qualified Crypto.Hash.SHA256  as SHA256
import qualified Data.ByteString     as BS
import qualified Data.Map.Strict     as Map
import qualified Data.Text           as Text
import           Data.Time           (UTCTime)
import           Test.Hspec
import qualified Tox.Application.Tox as Tox
import qualified Tox.Sync.Event      as Sync
import qualified Tox.Sync.Fact       as Sync
import qualified Tox.Sync.Frame      as Frame
import qualified Tox.Sync.Id         as Sync
import qualified Tox.Sync.Protocol   as Sync
import qualified Tox.Sync.Setting    as Sync
import qualified Tox.Sync.Store      as Sync

import           Tox.Client
import qualified Tox.Client.Linked   as Linked
import           Tox.Client.Sample   (at, looking, messageAt, unseen)
import           Tox.Client.Types
import           Tox.Client.Update

homeKey, device, bobKey, tabletKey :: Sync.PublicKey
homeKey = Sync.PublicKey (BS.replicate 32 0xaa)
device = Sync.PublicKey (BS.replicate 32 0xdd)
bobKey = Sync.PublicKey (BS.replicate 32 0x11)
tabletKey = Sync.PublicKey (BS.replicate 32 0x22)

home, bob :: ContactId
home = ContactId (Text.replicate 32 "AA")
bob = ContactId (Text.replicate 32 "11")

homeAddress :: Text.Text
homeAddress = Text.replicate 32 "AA" <> "000000000000"

pairing :: Text.Text
pairing = "pair:00112233445566778899aabbccddeeff"

-- | Linked 1 hour before the home's events.
linked :: Client
linked = initialClient
  { clientLinked = Just (Linked.newLinked homeAddress (Just pairing) (at 5 9 0) device Sync.emptyStore) }

account, bobChat :: Sync.GraphId
account = Sync.accountGraph homeKey
bobChat = Sync.friendGraph homeKey bobKey

-- | Bob as a friend with a name, and his message.
homeEvents :: [BS.ByteString]
homeEvents = writtenByHome
  [ (account, at 5 10 0, Sync.WantFriend bobKey Nothing Nothing)
  , (account, at 5 10 0, Sync.FactContent (Sync.FriendSubject bobKey) (Sync.Name "Bob"))
  , (bobChat, at 5 10 1, Sync.Received Nothing "Bob" "hi" Sync.Normal)
  -- Another device of the account (the home has it as a friend).
  , (account, at 5 10 2, Sync.WantFriend tabletKey Nothing Nothing)
  , (account, at 5 10 2, Sync.Device tabletKey "Tablet") ]

-- | Each event as the home writes it, in its graph at its time.
writtenByHome :: [(Sync.GraphId, UTCTime, Sync.Content)] -> [BS.ByteString]
writtenByHome = snd . foldl write (Sync.emptyStore, [])
  where
    write (store, written) (g, time, content) =
      let (_, bytes, store') = Sync.create g homeKey (Linked.toMillis time) content store
      in (store', written ++ [bytes])

-- | Each event as the home writes it at @time@.
writtenAt :: UTCTime -> [(Sync.GraphId, Sync.Content)] -> [BS.ByteString]
writtenAt time = writtenByHome . map (\(g, content) -> (g, time, content))

fromHome :: Sync.SessionMessage -> [Event]
fromHome = map (SyncReceived home (at 3 9 0)) . Frame.frames . Sync.encodeMessage

run :: Viewing -> [Event] -> Client -> (Client, [Command])
run viewing events client = foldl (\(c, cs) e -> let (c', cs') = update viewing e c in (c', cs ++ cs')) (client, []) events

step :: Event -> Client -> (Client, [Command])
step = update unseen

toHome :: [Command] -> [Sync.SessionMessage]
toHome commands = concat [messages packets | SendSync to packets <- commands, to == home]
  where
    messages = go Frame.reassembler
    go _ [] = []
    go r (p : ps) = case Frame.push p r of
      Right (Just bytes, r') | Right (Just m) <- Sync.decodeMessage bytes -> m : go r' ps
      Right (_, r') -> go r' ps
      Left (_, r') -> go r' ps

sent :: [Command] -> [Sync.Content]
sent commands = [Sync.eventContent e | Sync.Events es <- toHome commands, Right e <- map Sync.decodeEvent es]

synced :: Client
synced = fst (run unseen (fromHome Sync.hello ++ fromHome (Sync.Events homeEvents)) linked)

time' :: UTCTime
time' = at 5 11 0

spec :: Spec
spec = do
  it "pairs with the home by friend request, and greets it when it comes online" $ do
    snd (step (Started "") linked) `shouldBe` [RequestFriend homeAddress pairing]
    toHome (snd (step (FriendOnline home True) linked)) `shouldBe` [Sync.hello]

  it "counts the sync messages each way, and the events it holds" $ do
    let holds = maybe [] Linked.linkedHolds (clientLinked synced)
    mapM_ (\row -> holds `shouldContain` [row])
      [ ("sync in Hello", 1), ("sync out Summary", 1), ("sync in Events", 1), ("sync events", length homeEvents) ]

  it "shows the account's friends and messages from the home's events, and none of its devices" $ do
    let (client, commands) = run unseen (fromHome Sync.hello) linked
    [() | Sync.Summary _ <- toHome commands] `shouldBe` [()]
    snd (step (Started "") client) `shouldBe` [Befriend home]
    Map.keys (clientContacts synced) `shouldBe` [bob]
    (\c -> (contactName c, map messageText (contactMessages c), contactUnread c)) <$> Map.lookup bob (clientContacts synced)
      `shouldBe` Just (Just "Bob", ["hi"], 1)

  it "tells the other devices what the user read and sent" $ do
    let chat = FriendChat bob
        (opened, commands) = update (looking chat) (MarkRead chat) synced
    [rank | Sync.ReadUpTo g _ rank <- sent commands, g == bobChat] `shouldBe` [0]
    contactUnread <$> Map.lookup bob (clientContacts opened) `shouldBe` Just 0
    let (written, sending) = run (looking chat) [SetDraft chat "hello", Send chat time'] opened
    [text | Sync.Send _ text Sync.Normal <- sent sending] `shouldBe` ["hello"]
    map messageDelivery . contactMessages <$> Map.lookup bob (clientContacts written) `shouldBe` Just [Delivered, Pending]

  it "asks the home to found and join chats" $ do
    [c | c@Sync.NewChat {} <- sent (snd (step (Found PrivateGroup " Lab ") synced))]
      `shouldBe` [Sync.NewChat Sync.GroupKind "Lab" Sync.Private]
    let chat = BS.replicate 32 0x33
    [c | c@Sync.JoinChat {} <- sent (snd (step (JoinPublic (GroupId (Text.replicate 32 "33")) "pw") synced))]
      `shouldBe` [Sync.JoinChat chat (Just "pw")]

  it "shows private messages in groups, and asks the home to send ours" $ do
    let lab = Sync.GroupChat (BS.replicate 32 0x33)
        events = writtenAt (at 5 10 3)
          [ (account, Sync.FactContent (Sync.ChatSubject lab) (Sync.Joined True))
          , (account, Sync.FactContent (Sync.ChatSubject lab) (Sync.Name "Lab"))
          , (Sync.chatGraph lab, Sync.ReceivedPrivately bobKey "Bob" "psst" Sync.Normal) ]
        present = Sync.Facts [(Sync.PeerSubject lab bobKey, Sync.Present True), (Sync.PeerSubject lab bobKey, Sync.Name "Bob")] True
        client = fst (run unseen (fromHome (Sync.Events events) ++ fromHome present) synced)
        gid = GroupId (Text.replicate 32 "33")
    map entryWhat . groupEntries <$> Map.lookup gid (clientGroups client)
      `shouldBe` Just [PrivateFrom (Member (Text.replicate 32 "11") "Bob") Tox.MessageNormal "psst"]
    -- Bob keeps his name after leaving.
    let gone = fst (run unseen (fromHome (Sync.Events (events ++ [bye]))) synced)
        (_, bye, _) = Sync.create (Sync.chatGraph lab) homeKey (Linked.toMillis (at 5 10 4))
          (Sync.SendPrivately bobKey "" "bye" Sync.Normal) (foldl (\st b -> either (const st) snd (Sync.insert b st)) Sync.emptyStore events)
    [m | PrivateTo m _ _ _ <- maybe [] (map entryWhat . groupEntries) (Map.lookup gid (clientGroups gone))]
      `shouldBe` [Member (Text.replicate 32 "11") "Bob"]
    let chat = GroupChat gid
        privately = [MarkRead chat, SetDraft chat "hi Bob", SendPrivately gid (Text.replicate 32 "11") time']
    [c | c@Sync.SendPrivately {} <- sent (snd (run (looking chat) privately client))]
      `shouldBe` [Sync.SendPrivately bobKey "" "hi Bob" Sync.Normal]

  it "shows who came, went and got a role in groups" $ do
    let lab = Sync.GroupChat (BS.replicate 32 0x33)
        events = writtenAt (at 5 10 3)
          [ (account, Sync.FactContent (Sync.ChatSubject lab) (Sync.Joined True))
          , (Sync.chatGraph lab, Sync.PeerJoined bobKey "Bob")
          , (Sync.chatGraph lab, Sync.Moderated Nothing "" (Just bobKey) "Bob" (Just Sync.Moderator))
          , (Sync.chatGraph lab, Sync.PeerLeft bobKey "Bob") ]
        client = fst (run unseen (fromHome (Sync.Events events)) synced)
        member = Member (Text.replicate 32 "11") "Bob"
    map entryWhat . groupEntries <$> Map.lookup (GroupId (Text.replicate 32 "33")) (clientGroups client)
      `shouldBe` Just [Arrived member, Moderated (Just (Member "" "")) (Just member) Tox.ModModerator, Departed member]

  it "asks the home to moderate and change a group's settings" $ do
    let lab = Sync.GroupChat (BS.replicate 32 0x33)
        events = writtenAt (at 5 10 3)
          [ (account, Sync.FactContent (Sync.ChatSubject lab) v)
          | v <- [ Sync.Joined True, Sync.Name "Lab", Sync.PrivacyValue Sync.Public, Sync.PeerLimit 100
                 , Sync.VoiceStateValue Sync.VoiceEveryone, Sync.RoleValue Sync.Founder ] ]
        present = Sync.Facts [(Sync.PeerSubject lab bobKey, Sync.Present True)] True
        client = fst (run unseen (fromHome (Sync.Events events) ++ fromHome present) synced)
        gid = GroupId (Text.replicate 32 "33")
        wishes event = [(s', v) | Sync.Wish s' v <- sent (snd (step event client))]
    wishes (Moderate gid 0 Kick) `shouldBe` [(Sync.PeerSubject lab bobKey, Sync.Present False)]
    let private = maybe [] (\settings -> wishes (ChangeSettings gid settings { Tox.groupPrivate = True }))
          (groupSettings =<< Map.lookup gid (clientGroups client))
    private `shouldBe` [(Sync.ChatSubject lab, Sync.PrivacyValue Sync.Private)]
    -- We only show chats the account is in, so deleting one leaves it.
    wishes (DeleteGroup gid) `shouldBe` [(Sync.ChatSubject lab, Sync.Joined False)]

  it "offers the topic to whom Tox takes it from: not observers, and only moderators while locked" $ do
    let lab = Sync.GroupChat (BS.replicate 32 0x33)
        gid = GroupId (Text.replicate 32 "33")
        may role locked =
          let events = writtenAt (at 5 10 3)
                [ (account, Sync.FactContent (Sync.ChatSubject lab) v)
                | v <- [Sync.Joined True, Sync.Name "Lab", Sync.TopicLock locked, Sync.RoleValue role] ]
          in groupMayTopic <$> Map.lookup gid (clientGroups (fst (run unseen (fromHome (Sync.Events events)) synced)))
    map (uncurry may) [(Sync.User, False), (Sync.Observer, False), (Sync.User, True), (Sync.Moderator, True)]
      `shouldBe` map Just [True, False, False, True]

  it "fetches avatars from the home, serves it ours, and asks it to show ours" $ do
    let png = "a picture"
        blob = SHA256.hash png
        (_, bytes, _) = Sync.create account homeKey (Linked.toMillis (at 5 10 3))
          (Sync.FactContent (Sync.FriendSubject bobKey) (Sync.Avatar (Just (Sync.BlobId blob)))) Sync.emptyStore
        (fetched, commands) = run unseen (fromHome (Sync.Events [bytes])) synced
    [b | FetchBlob _ b _ _ <- commands] `shouldBe` [blob]
    -- Only once per session.
    [b | FetchBlob _ b _ _ <- snd (step (Tick time') fetched)] `shouldBe` []
    let shown = fst (step (BlobArrived blob "/blobs/b" (Just png)) fetched)
    (contactAvatar =<< Map.lookup bob (clientContacts shown)) `shouldBe` Just blob
    Map.lookup blob (clientAvatars shown) `shouldBe` Just png
    [b | ServeBlob _ b _ <- snd (run unseen (fromHome (Sync.WantBlob (Sync.BlobId blob))) shown)] `shouldBe` [blob]
    let (_, picked) = step (SetAvatar "ours") shown
    [p' | StoreBlob p' <- picked] `shouldBe` ["ours"]
    [v | Sync.Wish Sync.AccountSubject v <- sent picked] `shouldBe` [Sync.Avatar (Just (Sync.BlobId (SHA256.hash "ours")))]

  it "fetches friends' files from the home, small ones as they come and others when asked" $ do
    let small = Sync.BlobId (BS.replicate 32 0x44)
        big = Sync.BlobId (BS.replicate 32 0x55)
        create = Sync.create bobChat homeKey (Linked.toMillis (at 5 10 3))
        (note, b1, s1) = create (Sync.File "Bob" Nothing "note.txt" 10 True Nothing) Sync.emptyStore
        (_, b2, s2) = create (Sync.FileEnded note True (Just small)) s1
        (film, b3, s3) = create (Sync.File "Bob" Nothing "film.mkv" (10 ^ (12 :: Int)) True Nothing) s2
        (_, b4, s4) = create (Sync.FileEnded film True (Just big)) s3
        (_, b5, _) = create (Sync.File "Bob" Nothing "offer.zip" 20 True Nothing) s4
        events = [b1, b2, b3, b4, b5]
        (client, commands) = run unseen (fromHome (Sync.Events events)) synced
        files c = [ (messageText msg, attachmentState a) | Just contact <- [Map.lookup bob (clientContacts c)]
                                                         , msg <- contactMessages contact, Just a <- [messageFile msg] ]
        Sync.BlobId smallBytes = small
        Sync.BlobId bigBytes = big
    [(b, n) | FetchBlob _ b _ n <- commands] `shouldBe` [(smallBytes, Just "note.txt")]
    -- Files count as unread, like in the notification.
    contactUnread <$> Map.lookup bob (clientContacts client) `shouldBe` Just 4
    -- Cancelled if the home doesn't have the blob, or sends the wrong one.
    files (fst (run unseen (fromHome (Sync.NoBlob small)) client)) `shouldSatisfy` elem ("note.txt", Cancelled)
    files (fst (step (BlobMissing smallBytes) client)) `shouldSatisfy` elem ("note.txt", Cancelled)
    let arrived = fst (step (BlobArrived smallBytes "/received/note.txt" Nothing) client)
    files arrived `shouldBe` [("note.txt", Complete), ("film.mkv", Waiting), ("offer.zip", Waiting)]
    [(b, n) | FetchBlob _ b _ n <- snd (step (Download bob (messageAt bob 2 arrived)) arrived)] `shouldBe` [(bigBytes, Just "film.mkv")]
    [c | c@Sync.FileAnswered {} <- sent (snd (step (Download bob (messageAt bob 3 arrived)) arrived))] `shouldSatisfy` ((== 1) . length)

  it "counts a missed call unread, and one answered not" $ do
    let create = Sync.create bobChat homeKey (Linked.toMillis (at 5 10 3))
        (missed, b1, s1) = create (Sync.CallStarted "Bob" True False) Sync.emptyStore
        (_, b2, s2) = create (Sync.CallEnded missed Nothing False) s1
        (answered, b3, s3) = create (Sync.CallStarted "Bob" True False) s2
        (_, b4, _) = create (Sync.CallEnded answered (Just 5000) False) s3
        client = fst (run unseen (fromHome (Sync.Events [b1, b2, b3, b4])) synced)
    contactUnread <$> Map.lookup bob (clientContacts client) `shouldBe` Just 2

  it "asks the home to show the account away" $
    [v | Sync.Wish Sync.AccountSubject v <- sent (snd (step (SetAway Tox.Away) synced))]
      `shouldBe` [Sync.Status Sync.Away]

  it "keeps the way pictures go to a friend with the account" $ do
    let (client, commands) = step (SendPictures bob (Just "qoi")) synced
    [c | c@Sync.SettingContent {} <- sent commands]
      `shouldBe` [Sync.SettingContent (Sync.ConversationScope bobChat) (Sync.kept (Sync.PictureFormat Sync.Qoi))]
    contactPictures <$> Map.lookup bob (clientContacts client) `shouldBe` Just (Just "qoi")
    [c | c@Sync.ClearSetting {} <- sent (snd (step (SendPictures bob Nothing) client))]
      `shouldBe` [Sync.ClearSetting (Sync.ConversationScope bobChat) 1]

  it "keeps the way pictures go to a group with the account" $ do
    let lab = Sync.GroupChat (BS.replicate 32 0x33)
        events = writtenAt (at 5 10 3) [(account, Sync.FactContent (Sync.ChatSubject lab) v) | v <- [Sync.Joined True, Sync.Name "Lab"]]
        client = fst (run unseen (fromHome (Sync.Events events)) synced)
        gid = GroupId (Text.replicate 32 "33")
        (sending, commands) = step (SendGroupPictures gid (Just "qoi")) client
    [c | c@Sync.SettingContent {} <- sent commands]
      `shouldBe` [Sync.SettingContent (Sync.ConversationScope (Sync.chatGraph lab)) (Sync.kept (Sync.PictureFormat Sync.Qoi))]
    groupPictures <$> Map.lookup gid (clientGroups sending) `shouldBe` Just (Just "qoi")

  it "keeps our files as blobs and asks the home to send them" $ do
    let chat = FriendChat bob
        staged = run (looking chat) [MarkRead chat, Stage bob (StagedFile "/sent/a.png" "a.png" 10 "id"), SetDraft chat "look"] synced
        (sending, commands) = run (looking chat) [Send chat time'] (fst staged)
    [c | c@ShareFile {} <- commands] `shouldBe` [ShareFile bob "/sent/a.png" "a.png" 10]
    [t | Sync.Send _ t _ <- sent commands] `shouldBe` ["look"]
    let blob = BS.replicate 32 0x66
        (shared, more) = step (FileShared bob time' "a.png" 10 blob "/sent/a.png") sending
    [c | c@Sync.File {} <- sent more] `shouldBe` [Sync.File "" Nothing "a.png" 10 False (Just (Sync.BlobId blob))]
    [ (messageText m, attachmentPath a) | Just c <- [Map.lookup bob (clientContacts shared)]
                                        , m <- contactMessages c, Just a <- [messageFile m] ]
      `shouldBe` [("a.png", "/sent/a.png")]

  it "rings with the home, and takes the friend's call through it" $ do
    let ringing = fst (run unseen (fromHome (Sync.Facts [(Sync.FriendSubject bobKey, Sync.CallValue (Sync.Ringing False))] False)) synced)
    (callContact <$> clientCall ringing, callPhase <$> clientCall ringing) `shouldBe` (Just bob, Just Ringing)
    -- Rings as long as the home lets it.
    callPhase <$> clientCall (fst (step (Tick (at 6 0 0)) ringing)) `shouldBe` Just Ringing
    let (answering, asked) = step (AnswerCall time' False) ringing
    toHome asked `shouldBe` [Sync.Call bobKey (Sync.Connect False)]
    callPhase <$> clientCall answering `shouldBe` Just Calling
    -- The home relays the friend's call, so we answer the home.
    let (talking, answered) = step (CallInvited home time' False) answering
    answered `shouldBe` [AnswerFriend home False]
    (callPhase <$> clientCall talking, callTarget <$> clientCall talking) `shouldBe` (Just (Talking time'), Just home)
    let ended = snd (step (HangUp time') talking)
    [c | c@(HangUpCall _) <- ended] `shouldBe` [HangUpCall home]
    toHome ended `shouldBe` [Sync.Call bobKey Sync.HangUp]
    -- Stops ringing when answered elsewhere.
    clientCall (fst (run unseen (fromHome (Sync.Facts [(Sync.FriendSubject bobKey, Sync.CallValue Sync.Answered)] False)) ringing))
      `shouldBe` Nothing
