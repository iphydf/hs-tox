{-# LANGUAGE OverloadedStrings #-}
module Tox.Client.HomeSpec (spec) where

import qualified Data.ByteString     as BS
import qualified Data.Map.Strict     as Map
import qualified Data.Text           as Text
import           Test.Hspec
import qualified Tox.Application.Tox as Tox
import qualified Tox.Sync.Id         as Sync
import qualified Tox.Sync.Store      as Sync

import           Tox.Client
import qualified Tox.Client.Home     as Home
import qualified Tox.Client.Linked   as Linked
import           Tox.Client.Sample
import           Tox.Client.Types
import           Tox.Client.Update

-- | The sample's Tox ID: it's the home.
homeAddress :: Text.Text
homeAddress = toxId (Text.replicate 8 "5A17ED00") "0BADF00D"

home, tablet :: ContactId
home = ContactId (Text.take 64 homeAddress)
tablet = ContactId (Text.replicate 32 "DD")

tabletKey :: Sync.PublicKey
tabletKey = Sync.PublicKey (BS.replicate 32 0xdd)

code :: Text.Text
code = "00112233445566778899aabbccddeeff"

-- | A new profile linking to the sample's account.
linking :: Client
linking = initialClient
  { clientLinked = Just (Linked.newLinked homeAddress (Just ("pair:" <> code)) (at 1 0 0) tabletKey Sync.emptyStore) }

run :: [Event] -> Client -> (Client, [Command])
run events client = foldl (\(c, cs) e -> let (c', cs') = update unseen e c in (c', cs ++ cs')) (client, []) events

-- | Paired and in session, with each side's commands since.
paired :: ((Client, [Command]), (Client, [Command]))
paired =
  let (h1, _) = run [Pair code (at 3 9 0)] sample
      (h2, hs) = run [Requested tablet ("pair:" <> code), FriendOnline tablet True] h1
      (t1, ts) = run [Started "", FriendOnline home True] linking
  in settle (h2, hs) (t1, ts)

-- | Delivers sync packets until both are quiet, dropping them from the
-- commands.
settle :: (Client, [Command]) -> (Client, [Command]) -> ((Client, [Command]), (Client, [Command]))
settle a b = go (100 :: Int) a b
  where
    go 0 x y = (x, y)
    go n (h, hs) (t, ts)
      | null toTablet && null toHome = ((h, hs), (t, ts))
      | otherwise =
          let (h', hs') = run (map (SyncReceived tablet (at 3 9 0)) toHome) h
              (t', ts') = run (map (SyncReceived home (at 3 9 0)) toTablet) t
          in go (n - 1) (h', others hs ++ hs') (t', others ts ++ ts')
      where
        toTablet = [p | SendSync to packets <- hs, to == tablet, p <- packets]
        toHome = [p | SendSync to packets <- ts, to == home, p <- packets]
    others = filter (\c -> case c of SendSync {} -> False; _ -> True)

-- | Applies events on one side, then lets the sides sync.
on :: Bool -> [Event] -> ((Client, [Command]), (Client, [Command])) -> ((Client, [Command]), (Client, [Command]))
on atHome events ((h, _), (t, _))
  | atHome = settle (run events h) (t, [])
  | otherwise = settle (h, []) (run events t)

spec :: Spec
spec = do
  it "pairs a device with the code once, and keeps it out of the contacts" $ do
    let ((h, hs), _) = paired
    hs `shouldContain` [Befriend tablet]
    fmap (Map.keys . Home.devices) (clientHome h) `shouldBe` Just [tabletKey]
    Map.member tablet (clientContacts h) `shouldBe` False
    -- The code is used up: another request with it is a friend request.
    let (again, _) = run [Requested (ContactId (Text.replicate 32 "EE")) ("pair:" <> code)] h
    Map.size (clientRequests again) `shouldBe` Map.size (clientRequests h) + 1

  it "shows the device the account's profile, friends and chats" $ do
    let (_, (t, _)) = paired
    profileNickname (clientProfile t) `shouldBe` "Tox User"
    Map.map contactName (clientContacts t) `shouldBe` Map.fromList [(alice, Just "Alice"), (bob, Just "Bob")]
    map messageText (maybe [] contactMessages (Map.lookup alice (clientContacts t)))
      `shouldBe` ["Hi! Is this the Haskell client?", "It is, and it's on the network now.", "Looks good already."]
    fmap contactOnline (Map.lookup alice (clientContacts t)) `shouldBe` Just True
    Map.keys (clientRequests t) `shouldBe` [carol]

  it "sends the device's message from the home, and shows the device when it's delivered" $ do
    let written = on False [SetDraft (FriendChat alice) "From the tablet", Send (FriendChat alice) (at 3 9 5)] paired
        ((h, hs), _) = written
    m <- maybe (fail "not at the home") pure (lastMessage alice h)
    messageText m `shouldBe` "From the tablet"
    hs `shouldContain` [SendMessage alice (messageId m) "From the tablet" 0]
    let (_, (t, _)) = on True [Delivering alice (messageId m) Delivered] written
    fmap messageDelivery (lastMessage alice t) `shouldBe` Just Delivered

  it "shows the device what the home gets and sends" $ do
    let (_, (t, _)) = on True [ Received alice (at 3 9 6) Tox.MessageNormal "Hello, tablet?"
                              , SetDraft (FriendChat alice) "From the home", Send (FriendChat alice) (at 3 9 7) ] paired
    map messageText (take 2 (reverse (maybe [] contactMessages (Map.lookup alice (clientContacts t)))))
      `shouldBe` ["From the home", "Hello, tablet?"]

  it "takes the device's profile changes and friends" $ do
    let dave = ContactId (Text.replicate 32 "0D")
        daveAddress = toxId (Text.replicate 32 "0D") "12345678"
        ((h, hs), _) = on False [ChangeProfile "Tablet Tom" "On the go", AcceptRequest carol, AddFriend daveAddress "Hi Dave"] paired
    (profileNickname (clientProfile h), profileStatus (clientProfile h)) `shouldBe` ("Tablet Tom", "On the go")
    Map.member carol (clientContacts h) `shouldBe` True
    hs `shouldContain` [RequestFriend daveAddress "Hi Dave"]
    Map.member dave (clientContacts h) `shouldBe` True

  it "shows the device the groups, their members and messages, and sends its messages there" $ do
    let dana = Member (Text.replicate 8 "DA7A0000") "Dana"
        joined = on True [ Invited alice team NgcGroup "Team" "invite", AnswerInvite alice team (Just "")
                         , MemberJoined team (at 3 9 0) 1 dana, GroupConnected team (at 3 9 0)
                         , GroupReceived team (at 3 9 1) 1 Tox.MessageNormal "Hi team" (Just 7) ] paired
        (_, (t, _)) = joined
    g <- maybe (fail "no group on the device") pure (Map.lookup team (clientGroups t))
    groupName g `shouldBe` "Team"
    map memberName (Map.elems (groupMembers g)) `shouldBe` ["Dana"]
    [text | GroupEntry _ (Said (Just m) _ text _ _) _ <- groupEntries g, memberName m == "Dana"] `shouldBe` ["Hi team"]
    let ((_, hs), _) = on False [SetDraft (GroupChat team) "Hi from the tablet", Send (GroupChat team) (at 3 9 2)] joined
    [text | SendGroupMessage gid _ text <- hs, gid == team] `shouldBe` ["Hi from the tablet"]

  it "joins a public group a device asks for, which the device then shows" $ do
    let ((h, hs), (t, _)) = on False [JoinPublic team "secret"] paired
    hs `shouldContain` [JoinPublicGroup team "Tox User" "secret"]
    fmap groupPhase (Map.lookup team (clientGroups h)) `shouldBe` Just Joining
    Map.member team (clientGroups t) `shouldBe` True

  it "shows the device an invite, and joins when the device says so" $ do
    let invited = on True [Invited alice team NgcGroup "Team" "invite"] paired
        (_, (t, _)) = invited
    Map.keys (clientInvites t) `shouldSatisfy` (not . null)
    let answer = [AnswerInvite cid gid (Just "") | (cid, gid) <- Map.keys (clientInvites t)]
        ((h, hs), (t', _)) = on False answer invited
    hs `shouldContain` [JoinGroup alice team "invite" "Tox User" ""]
    Map.member team (clientGroups h) `shouldBe` True
    Map.member team (clientGroups t') `shouldBe` True

  it "shows the device a message from before it was linked as delivered, also after a restart" $ do
    let (waiting, _) = run [SetDraft (FriendChat bob) "For Bob, later", Send (FriendChat bob) (at 3 8 0)] sample
        (h1, _) = run [Pair code (at 3 9 0)] waiting
        (h2, hs) = run [Requested tablet ("pair:" <> code), FriendOnline tablet True] h1
        (t1, ts) = run [Started "", FriendOnline home True] linking
        ((h3, _), (t3, _)) = settle (h2, hs) (t1, ts)
        -- As loaded from the store: the home starts over from its events.
        restarted = h3 { clientHome = (\old -> Home.newHome (Home.homeAccount old) (Home.homeStore old)) <$> clientHome h3 }
    m <- maybe (fail "not at the home") pure (lastOf bob h3)
    fmap messageDelivery (lastOf bob t3) `shouldBe` Just Pending
    let (h4, hs4) = run [FriendOnline tablet True] restarted
        (t4, ts4) = run [FriendOnline home False, FriendOnline home True] t3
        (_, (t5, _)) = on True [FriendOnline bob True, Delivering bob (messageId m) Delivered] (settle (h4, hs4) (t4, ts4))
    fmap messageDelivery (lastOf bob t5) `shouldBe` Just Delivered

  it "rings a friend's call on the device, which answers it through the home" $ do
    let ringing = on True [CallInvited alice (at 3 9 3) False] paired
        (_, (t, _)) = ringing
    fmap callPhase (clientCall t) `shouldBe` Just Ringing
    let ((h, hs), (t', ts)) = on False [AnswerCall (at 3 9 4) False] ringing
    hs `shouldContain` [AnswerFriend alice False, RelayCall alice tablet False]
    (callDevice <$> clientCall h) `shouldBe` Just (Just tablet)
    (callVia <$> clientCall t') `shouldBe` Just (Just home)
    -- The home calls the device, which answers.
    snd (run [CallInvited home (at 3 9 4) False] t') `shouldContain` [AnswerFriend home False]
    ts `shouldBe` []

  it "places a device's call, and calls the device once the friend answers" $ do
    let placed = on False [StartCall alice (at 3 9 3) True] paired
        ((h, hs), _) = placed
    hs `shouldContain` [CallFriend alice True]
    (callDevice <$> clientCall h) `shouldBe` Just (Just tablet)
    snd (run [CallPeer alice (at 3 9 5) True] h) `shouldContain` [RelayCall alice tablet True]

  it "ends the friend's call with the device's, as a call that went on" $ do
    let talking = on False [AnswerCall (at 3 9 4) False] (on True [CallInvited alice (at 3 9 3) False] paired)
        ((h, hs), (t, _)) = on True [CallEnded tablet (at 3 9 9)] talking
    hs `shouldContain` [HangUpCall alice]
    clientCall h `shouldBe` Nothing
    [ r | m <- maybe [] contactMessages (Map.lookup alice (clientContacts t)), Just r <- [messageCall m] ]
      `shouldBe` [CallRecord False (Just 360) False]

  it "ends a ringing call a device declines, which isn't missed" $ do
    let ((h, hs), (t, _)) = on False [HangUp (at 3 9 4)] (on True [CallInvited alice (at 3 9 3) False] paired)
    hs `shouldContain` [HangUpCall alice]
    clientCall h `shouldBe` Nothing
    [ recordMissed r | m <- maybe [] contactMessages (Map.lookup alice (clientContacts t)), Just r <- [messageCall m] ]
      `shouldBe` [False]

  it "stops syncing with a device once it's removed" $ do
    let ((h, hs), _) = on True [RemoveDevice tablet] paired
    hs `shouldContain` [Unfriend tablet]
    fmap (Map.null . Home.devices) (clientHome h) `shouldBe` Just True
    let (_, hs') = run [SyncReceived tablet (at 3 9 0) p | SendSync _ ps <- snd (run [FriendOnline home True] linking), p <- ps] h
    [() | SendSync to _ <- hs', to == tablet] `shouldBe` []
  where
    lastOf = lastMessage
    lastMessage cid c = case reverse (maybe [] contactMessages (Map.lookup cid (clientContacts c))) of
      m : _ -> Just m
      []    -> Nothing
