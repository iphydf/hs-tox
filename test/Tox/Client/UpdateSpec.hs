{-# LANGUAGE OverloadedStrings #-}
module Tox.Client.UpdateSpec (spec) where

import qualified Data.ByteString     as BS
import           Data.List           (nub)
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as TextEncoding
import           Data.Time           (addUTCTime)
import           Test.Hspec
import qualified Tox.Application.Tox as Tox
import qualified Tox.Share.History   as History
import qualified Tox.Share.Info      as Info
import           Tox.Share.Packet    (sha1Info)

import           Tox.Client
import           Tox.Client.Sample
import           Tox.Client.Types
import           Tox.Client.Update

messagesOf :: ContactId -> Client -> [Message]
messagesOf cid = maybe [] contactMessages . Map.lookup cid . clientContacts

groupOf :: Client -> Maybe Group
groupOf = Map.lookup team . clientGroups

dana :: Member
dana = Member (Text.replicate 8 "DA7A0000") "Dana"

-- | In the team group with Dana.
inGroup :: [Event]
inGroup =
  [ Invited alice team NgcGroup "Team" "invite", AnswerInvite alice team (Just "")
  , MemberJoined team (at 3 9 0) 1 dana, GroupConnected team (at 3 9 0) ]

spec :: Spec
spec = do
  it "joins a friend's group invite with the password once the friend is online" $ do
    let invited = [Invited bob team NgcGroup "Team" "invite"]
        answered = invited ++ [AnswerInvite bob team (Just "pw")]
    commands invited (AnswerInvite bob team (Just "pw")) `shouldBe` []
    commands answered (FriendOnline bob True) `shouldContain` [JoinGroup bob team "invite" "Tox User" "pw"]
    let joined = applying (answered ++ [FriendOnline bob True]) sample
    clientInvites joined `shouldBe` Map.empty
    groupPhase <$> groupOf joined `shouldBe` Just Joining

  it "spends an invite a wrong password fails, leaving what Tox has of the group" $ do
    let events = [Invited alice team NgcGroup "Team" "invite", AnswerInvite alice team (Just "")]
    commands (take 1 events) (AnswerInvite alice team (Just "")) `shouldBe` [JoinGroup alice team "invite" "Tox User" ""]
    commands events (GroupRefused team Tox.JoinFailBadPassword) `shouldBe` [QuitGroup team]
    groupPhase <$> groupOf (applying (events ++ [GroupRefused team Tox.JoinFailBadPassword]) sample)
      `shouldBe` Just (Refused Tox.JoinFailBadPassword)

  it "tells a friend we're typing once, and that we stopped after a pause, on sending, or with the draft gone" $ do
    let second n = addUTCTime n (at 3 9 0)
        typed = [Typing alice (second 0)]
        again = typed ++ [Typing alice (second 1)]
    commands [] (Typing alice (second 0)) `shouldBe` [SetTyping alice True]
    commands typed (Typing alice (second 1)) `shouldBe` []
    commands again (Tick (second 3)) `shouldBe` []
    commands again (Tick (second 4)) `shouldBe` [SetTyping alice False]
    commands typed (SetDraft (FriendChat alice) " ") `shouldBe` [SetTyping alice False]
    commands (typed ++ [SetDraft (FriendChat alice) "hi"]) (Send (FriendChat alice) (second 1))
      `shouldContain` [SetTyping alice False]
    commands typed (CloseChat (FriendChat alice)) `shouldContain` [SetTyping alice False]
    commands [] (Typing bob (second 0)) `shouldBe` []

  it "shows a friend typing until they stop or go offline" $ do
    let typing = [FriendTyping alice True]
    clientTyping (applying typing sample) `shouldBe` Set.singleton alice
    clientTyping (applying (typing ++ [FriendTyping alice False]) sample) `shouldBe` Set.empty
    clientTyping (applying (typing ++ [FriendOnline alice False]) sample) `shouldBe` Set.empty
    commands [Typing alice (at 3 9 0)] (FriendOnline alice False) `shouldBe` []
    commands [Typing alice (at 3 9 0), FriendOnline alice False] (Tick (at 3 9 1)) `shouldBe` []

  it "neither tells nor shows typing with typing indicators off" $ do
    let off = [SetTypingIndicators False]
    commands off (Typing alice (at 3 9 0)) `shouldBe` []
    clientTyping (applying (off ++ [FriendTyping alice True]) sample) `shouldBe` Set.empty
    commands [Typing alice (at 3 9 0)] (SetTypingIndicators False) `shouldBe` [SetTyping alice False]
    clientTyping (applying [FriendTyping alice True, SetTypingIndicators False] sample) `shouldBe` Set.empty

  it "shows a file shared in the group, and downloads it only when asked" $ do
    let inf = Info.Info "cat.png" 1000 1000 (BS.replicate 20 1)
        fid = Info.hash inf
        shared = inGroup ++ [GroupFileShared team (at 3 9 1) 1 7 fid, GroupFileLearned team fid inf]
        fetched = shared ++ [DownloadGroupFile team fid, GroupFileMoved team fid "/files/cat.png" 1000 0, GroupFileEnded team fid True]
    concatMap (commands inGroup) (drop (length inGroup) shared) `shouldBe` []
    commands shared (DownloadGroupFile team fid) `shouldBe` [FetchGroupFile team inf]
    map entryWhat . groupEntries <$> groupOf (applying fetched sample)
      `shouldBe` Just [SharedFile (Just dana) (GroupFile fid (Just inf) (Just (Attachment 1000 "/files/cat.png" 1000 Complete (Just fid))) 0) (Just 7)]

  it "shares our file once it's hashed, and serves the group's whole files again once back in" $ do
    let inf = Info.Info "cat.png" 1000 1000 (BS.replicate 20 1)
        fid = Info.hash inf
        wrote = inGroup ++ [WroteGroupFile team (at 3 9 1) "/sent/cat.png" "cat.png" 1000]
        ours = entryAt team 0 (applying wrote sample)
        offered = wrote ++ [GroupFileOffered team ours (Just (inf, 9))]
        whole = Attachment 1000 "/sent/cat.png" 1000 Complete (Just fid)
    commands inGroup (WroteGroupFile team (at 3 9 1) "/sent/cat.png" "cat.png" 1000) `shouldBe` [ShareGroupFile team ours "/sent/cat.png" "cat.png"]
    map entryWhat . groupEntries <$> groupOf (applying offered sample)
      `shouldBe` Just [SharedFile Nothing (GroupFile fid (Just inf) (Just whole) 0) (Just 9)]
    commands offered (GroupHistoryWanted team 1 (History.Range maxBound 0))
      `shouldBe` [AnswerHistory team 1 (History.Range maxBound 0) [(at 3 9 1, Nothing, 9, History.Shared sha1Info fid)]]
    commands (offered ++ [Started ""]) (GroupConnected team (at 3 9 2))
      `shouldContain` [SeedGroupFiles team [(inf, "/sent/cat.png")]]

  it "pauses a download, and goes on with it when asked" $ do
    let inf = Info.Info "cat.png" 1000 1000 (BS.replicate 20 1)
        fid = Info.hash inf
        moving = inGroup ++ [GroupFileShared team (at 3 9 1) 1 7 fid, GroupFileLearned team fid inf, DownloadGroupFile team fid]
    commands moving (PauseGroupFile team fid) `shouldBe` [PauseGroupDownload team fid]
    commands (moving ++ [PauseGroupFile team fid]) (DownloadGroupFile team fid) `shouldBe` [FetchGroupFile team inf]

  it "asks each member that joins for history, and adds what it didn't have in time order" $ do
    let carl = Member (Text.replicate 8 "CA470000") "Carl"
        fid = BS.replicate 20 2
        said = inGroup ++ [GroupReceived team (at 3 9 5) 1 Tox.MessageNormal "hi" (Just 5)]
        recalled =
          [ (at 3 9 2, Just (memberKey carl), 3, History.Text False "early")
          , (at 3 9 5, Just (memberKey dana), 5, History.Text False "hi")
          , (at 3 9 3, Nothing, 4, History.Shared sha1Info fid)
          , (at 3 10 0, Just (memberKey dana), 6, History.Text False "from the future") ]
    commands inGroup (MemberJoined team (at 3 9 1) 2 carl) `shouldBe` [AskHistory team 2]
    commands said (GroupRecalled team (at 3 9 6) 1 Map.empty recalled) `shouldBe` [LearnGroupFile team 1 fid]
    map entryWhat . groupEntries <$> groupOf (applying (said ++ [GroupRecalled team (at 3 9 6) 1 Map.empty recalled]) sample)
      `shouldBe` Just
        [ Said (Just (Member (memberKey carl) "")) Tox.MessageNormal "early" Delivered (Just 3)
        , SharedFile Nothing (GroupFile fid Nothing Nothing 0) (Just 4)
        , Said (Just dana) Tox.MessageNormal "hi" Delivered (Just 5) ]

  it "names senders in history by what the chat knows, or once they join" $ do
    let carl = Member (Text.replicate 8 "CA470000") "Carl"
        eve = Member (Text.replicate 8 "E0E00000") "Eve"
        left = inGroup ++ [MemberJoined team (at 3 9 1) 2 carl, MemberLeft team (at 3 9 3) 2 Tox.ExitQuit]
        recalled =
          [ (at 3 9 2, Just (memberKey carl), 3, History.Text False "from carl")
          , (at 3 9 2, Just (memberKey eve), 4, History.Text False "from eve") ]
        senders = map (\e -> [memberName m | Said (Just m) _ _ _ _ <- [entryWhat e]]) . maybe [] groupEntries . groupOf
        heard = left ++ [GroupRecalled team (at 3 9 6) 1 Map.empty recalled]
    concat (senders (applying heard sample)) `shouldBe` ["Carl", ""]
    concat (senders (applying (heard ++ [MemberJoined team (at 3 9 7) 3 eve]) sample)) `shouldBe` ["Carl", "Eve"]

  it "answers a history request with what was said since the asker joined" $ do
    let carl = Member (Text.replicate 8 "CA470000") "Carl"
        events = inGroup ++
          [ GroupReceived team (at 3 9 1) 1 Tox.MessageNormal "before" (Just 1), MemberJoined team (at 3 9 2) 2 carl
          , GroupReceived team (at 3 9 3) 1 Tox.MessageNormal "after" (Just 2) ]
        everything = History.Range maxBound 0
    commands events (GroupHistoryWanted team 2 everything)
      `shouldBe` [AnswerHistory team 2 everything [(at 3 9 3, Just (memberKey dana), 2, History.Text False "after")]]

  it "names messages by their sender, and tells who came and went once in the group" $ do
    let carl = Member (Text.replicate 8 "CA470000") "Carl"
        events = inGroup ++
          -- Joined with Dana, while Tox is still meeting the peers.
          [ MemberJoined team (at 3 9 0) 3 (Member (Text.replicate 8 "E0E00000") "Eve")
          , MemberJoined team (at 3 9 1) 2 carl, GroupReceived team (at 3 9 2) 1 Tox.MessageNormal "hi" (Just 5)
          , MemberLeft team (at 3 9 3) 2 Tox.ExitQuit ]
    map entryWhat . groupEntries <$> groupOf (applying events sample)
      `shouldBe` Just [Arrived carl, Said (Just dana) Tox.MessageNormal "hi" Delivered (Just 5), Departed carl]

  it "finds mentions as whole words in any case" $ do
    map (mentions "Tox User") ["hi tox user!", "@Tox User: look", "Tox Users", "xTox User", "Tox"]
      `shouldBe` [True, True, False, False, False]
    mentions "  " "anything" `shouldBe` False

  it "tells of topics set after Tox got into the group, by whom, and sets ours where Tox lets us" $ do
    let events = [ Invited alice team NgcGroup "Team" "invite", AnswerInvite alice team (Just "")
                 , MemberJoined team (at 3 9 0) 1 dana
                 , TopicChanged team (at 3 9 0) Nothing "first", GroupConnected team (at 3 9 0)
                 , TopicChanged team (at 3 9 1) (Just 1) "second", TopicSetByUs team (at 3 9 2) "third" ]
        g = groupOf (applying events sample)
    groupTopic <$> g `shouldBe` Just "third"
    map entryWhat . groupEntries <$> g `shouldBe` Just [TopicSet (Just dana) "second", TopicSet Nothing "third"]
    commands events (SetTopic team " fourth ") `shouldBe` []
    commands (events ++ [TopicAllowed team True]) (SetTopic team " fourth ") `shouldBe` [SetGroupTopic team "fourth"]

  it "joins an audio conference for its text, resends what found no one, and rejoins it" $ do
    let talk = GroupId "01C0FFEE"
        carl = Member (Text.replicate 8 "CA470000") "Carl"
        joined = [ Invited alice talk AudioConference "" "cookie", AnswerInvite alice talk (Just "")
                 , GroupConnected talk (at 3 9 0), SetDraft (GroupChat talk) "early", Send (GroupChat talk) (at 3 9 1) ]
        early = entryAt talk 0 (applying joined sample)
    commands (take 1 joined) (AnswerInvite alice talk (Just "")) `shouldBe` [JoinConference alice talk "cookie"]
    commands (init joined) (Send (GroupChat talk) (at 3 9 1)) `shouldBe` [SendGroupMessage talk early "early"]
    commands joined (MembersListed talk (at 3 9 2) [(1, dana)]) `shouldBe` [SendGroupMessage talk early "early"]
    let later = joined ++ [GroupSent talk early (Just 6), MembersListed talk (at 3 9 2) [(1, dana)], MembersListed talk (at 3 9 3) [(2, carl)], GroupSaved talk "state"]
        g = Map.lookup talk (clientGroups (applying later sample))
    map entryWhat . drop 1 . groupEntries <$> g `shouldBe` Just [Arrived carl, Departed dana]
    commands later (Started "") `shouldContain` [RestoreGroup AudioConference talk "state"]

  it "founds groups, joins public ones by chat ID, and invites friends" $ do
    commands [] (Found PublicGroup " Lab ") `shouldBe` [FoundGroup PublicGroup "Lab" "Tox User"]
    groupPhase <$> groupOf (applying [GroupCreated team NgcGroup "Lab" (at 3 9 0)] sample) `shouldBe` Just InGroup
    commands [] (JoinPublic team "pw") `shouldBe` [JoinPublicGroup team "Tox User" "pw"]
    groupPhase <$> groupOf (applying [JoinPublic team "pw"] sample) `shouldBe` Just Joining
    commands [] (InviteFriend team alice) `shouldBe` [InviteToGroup team alice]

  it "lets the founder do anything to others, moderators only to users and observers" $ do
    memberActions (Just Tox.Founder) Tox.Moderator `shouldBe` [MakeUser, MakeObserver, Kick]
    memberActions (Just Tox.Founder) Tox.Founder `shouldBe` []
    memberActions (Just Tox.Moderator) Tox.User `shouldBe` [MakeObserver, Kick]
    memberActions (Just Tox.Moderator) Tox.Moderator `shouldBe` []
    memberActions (Just Tox.User) Tox.Observer `shouldBe` []

  it "says things to a member alone, and hears theirs" $ do
    let written = inGroup ++ [SetDraft (GroupChat team) "psst"]
        privately = SendPrivately team (memberKey dana) (at 3 9 1)
        psst = entryAt team 0 (applying (written ++ [privately]) sample)
    commands written privately `shouldBe` [SendPrivate team psst 1 "psst"]
    let both = written ++ [privately, GroupSent team psst Nothing, PrivateReceived team (at 3 9 2) 1 Tox.MessageNormal "hush"]
    map entryWhat . groupEntries <$> groupOf (applying both sample)
      `shouldBe` Just [PrivateTo dana Tox.MessageNormal "psst" Sent, PrivateFrom dana Tox.MessageNormal "hush"]
    -- Nothing once they're gone.
    let gone = applying (written ++ [MemberLeft team (at 3 9 1) 1 Tox.ExitQuit]) sample
    update unseen privately gone `shouldBe` (gone, [])

  it "tells who moderated whom, and leaves a group that removed us" $ do
    let carl = Member (Text.replicate 8 "CA470000") "Carl"
        moderated = take 3 inGroup ++
          [ MemberJoined team (at 3 9 0) 2 carl, GroupConnected team (at 3 9 0), GroupSaved team "state"
          , GroupModerated team (at 3 9 1) (Just 1) (Just 2) Tox.ModModerator ]
        kick = GroupModerated team (at 3 9 2) (Just 1) Nothing Tox.ModKick
    map entryWhat . groupEntries <$> groupOf (applying moderated sample)
      `shouldBe` Just [Moderated (Just dana) (Just carl) Tox.ModModerator]
    commands moderated kick `shouldBe` [QuitGroup team]
    let kicked = applying (moderated ++ [kick]) sample
    (groupPhase <$> groupOf kicked, groupSaved =<< groupOf kicked) `shouldBe` (Just Kicked, Nothing)
    commands [] (Moderate team 2 Kick) `shouldBe` [ModerateMember team 2 Kick]

  it "keeps what follows a message's zero byte, showing, quoting and forwarding only what comes before" $ do
    let bytes = "hi" <> BS.pack [0, 7, 7] <> "secret"
        received = [Received alice (at 3 9 0) Tox.MessageNormal bytes]
        got = last (messagesOf alice (applying received sample))
        forwarded = applying (received ++ [Forward (FriendChat alice) [messageId got] bob (at 3 9 1)]) sample
        answering = applying (received ++ [ReplyTo (FriendChat alice) (messageId got)]) sample
    (messageText got, messageRaw got) `shouldBe` (TextEncoding.decodeUtf8Lenient bytes, Just bytes)
    (shownText (messageText got), replied answering (FriendChat alice)) `shouldBe` ("hi", Just "> hi")
    messageText (last (messagesOf bob forwarded)) `shouldBe` "hi"

  it "sends what was written while joining, and rejoins from what Tox kept at a start" $ do
    let written = take 2 inGroup ++ [SetDraft (GroupChat team) "early", Send (GroupChat team) (at 3 9 0)]
        early = entryAt team 0 (applying written sample)
    commands written (Send (GroupChat team) (at 3 9 0)) `shouldBe` []
    commands written (GroupConnected team (at 3 9 0)) `shouldBe` [SendGroupMessage team early "early"]
    let kept = written ++ [GroupConnected team (at 3 9 0), GroupSent team early (Just 6), GroupSaved team "state"]
    commands kept (Started "") `shouldContain` [RestoreGroup NgcGroup team "state"]
    groupPhase <$> groupOf (applying (kept ++ [Started ""]) sample) `shouldBe` Just Joining

  it "sends the chat's draft at its time, and clears it" $ do
    let events = [SetDraft (FriendChat alice) "  hello  "]
        sent = applying (events ++ [Send (FriendChat alice) (at 3 9 0)]) sample
        i = messageAt alice 3 sent
    last (messagesOf alice sent) `shouldBe` Message True "hello" (at 3 9 0) Pending Nothing Nothing 0 Nothing Tox.MessageNormal Nothing i
    Map.lookup alice (clientDrafts sent) `shouldBe` Nothing
    commands events (Send (FriendChat alice) (at 3 9 0)) `shouldBe` [SendMessage alice i "hello" 0]

  it "gives each message its own ID, which stays as the message changes" $ do
    let sent = applying [SetDraft (FriendChat alice) "hi", Send (FriendChat alice) (at 3 9 0)] sample
        ids = map messageId . messagesOf alice
    nub (ids sent) `shouldBe` ids sent
    ids (applying [Delivering alice (last (ids sent)) Sent] sent) `shouldBe` ids sent

  it "keeps chats not open to their newest and what is still in use, and takes earlier ones from the store in front" $ do
    let small = unseen { viewingPage = 1 }
        trimmed = fst (update small (CloseChat (FriendChat alice)) sample)
        more = Received alice (at 3 9 0) Tox.MessageNormal "more"
        earlier = take 2 (messagesOf alice sample)
        back = applying [EarlierMessages alice earlier] trimmed
        earlierOf = fmap contactEarlier . Map.lookup alice . clientContacts
    (map messageText (messagesOf alice trimmed), earlierOf trimmed) `shouldBe` (["Looks good already."], Just True)
    -- Also when one scroll asks twice.
    messagesOf alice (applying [EarlierMessages alice earlier] back) `shouldBe` messagesOf alice sample
    earlierOf (applying [EarlierMessages alice []] back) `shouldBe` Just False
    -- What comes into a chat keeps it to its newest, unless it's open.
    map messageText (messagesOf alice (fst (update small more sample))) `shouldBe` ["more"]
    length (messagesOf alice (fst (update small { viewingChat = Just (FriendChat alice) } more sample))) `shouldBe` 4
    -- Ours waits for Bob, so it stays with what came after.
    let bobs = applying ([SetDraft (FriendChat bob) "waiting", Send (FriendChat bob) (at 3 9 0)]
                         ++ [Received bob (at 3 9 n) Tox.MessageNormal "more" | n <- [1 .. 3]]) sample
    messagesOf bob (fst (update small (CloseChat (FriendChat bob)) bobs)) `shouldBe` messagesOf bob bobs

  it "sends nothing blank" $ do
    let events = [SetDraft (FriendChat alice) "   "]
    applying (events ++ [Send (FriendChat alice) (at 3 9 0)]) sample `shouldBe` applying events sample
    commands events (Send (FriendChat alice) (at 3 9 0)) `shouldBe` []

  it "sends what a friend hasn't got once they are online" $ do
    let events = [SetDraft (FriendChat bob) "first", Send (FriendChat bob) (at 3 9 0)]
        both = events ++ [SetDraft (FriendChat bob) "second", Send (FriendChat bob) (at 3 9 1)]
        sent n = messageAt bob n (applying both sample)
    commands events (Send (FriendChat bob) (at 3 9 0)) `shouldBe` []
    commands (both ++ [Delivering bob (sent 1) Sent]) (FriendOnline bob True)
      `shouldBe` [SendMessage bob (sent 0) "first" 0, SendMessage bob (sent 1) "second" 0]

  it "marks a message sent, then delivered, and never back" $ do
    let delivery events = map messageDelivery (messagesOf alice (applying events sample))
    delivery [] `shouldBe` [Delivered, Delivered, Delivered]
    let sent = [SetDraft (FriendChat alice) "hi", Send (FriendChat alice) (at 3 9 0)]
        hi = messageAt alice 3 (applying sent sample)
    last (delivery sent) `shouldBe` Pending
    last (delivery (sent ++ [Delivering alice hi Sent])) `shouldBe` Sent
    last (delivery (sent ++ [Confirmed alice hi 1 (at 3 9 1), Delivering alice hi Sent])) `shouldBe` Delivered

  it "delivers a long message once the friend has all its parts" $ do
    let long = Text.replicate 3000 "a"
        written = [SetDraft (FriendChat alice) long]
        sent = written ++ [Send (FriendChat alice) (at 3 9 0)]
        last' events = last (messagesOf alice (applying (sent ++ events) sample))
        i = messageAt alice 3 (applying sent sample)
    commands written (Send (FriendChat alice) (at 3 9 0)) `shouldBe` [SendMessage alice i long 0]
    messageDelivery (last' [Confirmed alice i 1 (at 3 9 1), Confirmed alice i 2 (at 3 9 1)]) `shouldBe` Pending
    messageDelivery (last' [Confirmed alice i 2 (at 3 9 1), Confirmed alice i 3 (at 3 9 1)]) `shouldBe` Delivered
    commands (sent ++ [Confirmed alice i 1 (at 3 9 1), FriendOnline alice False]) (FriendOnline alice True)
      `shouldBe` [SendMessage alice i long 1]

  it "counts what is received while its chat isn't in front of the user" $ do
    let unread client = contactUnread <$> Map.lookup alice (clientContacts client)
        more = Received alice (at 3 9 0) Tox.MessageNormal "more"
        read' = applying [MarkRead (FriendChat alice)] sample
    unread sample `shouldBe` Just 1
    unread (applying [more] sample) `shouldBe` Just 2
    unread read' `shouldBe` Just 0
    unread (fst (update (looking (FriendChat alice)) more read')) `shouldBe` Just 0

  it "takes small files from friends, and asks before large ones" $ do
    let offer size = FileOffered alice (Receiving 0) (at 3 9 0) "a file" size "/received/a file" "id"
        files events = [ attachmentState f | m <- messagesOf alice (applying events sample), Just f <- [messageFile m] ]
        downloadIt = Download alice (messageAt alice 3 (applying [offer (autoAccepted + 1)] sample))
    commands [] (offer autoAccepted) `shouldBe` [AcceptFile alice (Receiving 0) "/received/a file"]
    commands [] (offer (autoAccepted + 1)) `shouldBe` []
    files [offer (autoAccepted + 1)] `shouldBe` [Waiting]
    commands [offer (autoAccepted + 1)] downloadIt `shouldBe` [AcceptFile alice (Receiving 0) "/received/a file"]
    files [offer 10, FileMoved alice (Receiving 0) 5, FileEnded alice (Receiving 0) True] `shouldBe` [Complete]

  it "takes files up to the size the user chose, none when never" $ do
    let offer = FileOffered alice (Receiving 0) (at 3 9 0) "a file" (1024 * 1024) "/received/a file" "id"
    commands [SetAutoAccept (1024 * 1024)] offer `shouldBe` [AcceptFile alice (Receiving 0) "/received/a file"]
    commands [SetAutoAccept (1024 * 1024 - 1)] offer `shouldBe` []
    commands [SetAutoAccept 0] offer `shouldBe` []

  it "offers our file again after the friend was offline, and goes on with theirs" $ do
    let ours = [Stage alice (StagedFile "/sent/a.png" "a.png" 10 "ours"), Send (FriendChat alice) (at 3 9 0)]
        i = messageAt alice 3 (applying ours sample)
        theirs n = FileOffered alice (Receiving n) (at 3 9 1) "b.txt" 20 "/received/b.txt"
        cut = ours ++ [ FileStarted alice i (Sending 0), FileMoved alice (Sending 0) 5
                      , theirs 1 "theirs", FileMoved alice (Receiving 1) 5, FriendOnline alice False ]
        files events = [ attachmentState f | m <- messagesOf alice (applying events sample), Just f <- [messageFile m] ]
    commands (init ours) (Send (FriendChat alice) (at 3 9 0)) `shouldBe` [SendFile alice i "/sent/a.png" "a.png" 10 (Just "ours")]
    files cut `shouldBe` [Waiting, Interrupted]
    commands cut (FriendOnline alice True) `shouldContain` [SendFile alice i "/sent/a.png" "a.png" 10 (Just "ours")]
    commands (cut ++ [FriendOnline alice True]) (FriendOnline alice True) `shouldBe` []
    -- Theirs resumes in its message when offered again. A new one gets its
    -- own.
    let returned = cut ++ [FriendOnline alice True]
    commands returned (theirs 2 "theirs") `shouldBe` [AcceptFile alice (Receiving 2) "/received/b.txt"]
    files (returned ++ [theirs 2 "theirs"]) `shouldBe` [Waiting, Moving]
    files (returned ++ [theirs 2 "other"]) `shouldBe` [Waiting, Interrupted, Moving]

  it "sends our cancelled file again from the start, now or when the friend is back" $ do
    let ours = [Stage alice (StagedFile "/sent/a.png" "a.png" 10 "ours"), Send (FriendChat alice) (at 3 9 0)]
        i = messageAt alice 3 (applying ours sample)
        cancelled = ours ++ [FileStarted alice i (Sending 0), FileMoved alice (Sending 0) 5, FileEnded alice (Sending 0) False]
        again = SendAgain alice i
        file events = [ (attachmentState f, attachmentDone f) | m <- messagesOf alice (applying events sample), Just f <- [messageFile m] ]
    commands cancelled again `shouldBe` [SendFile alice i "/sent/a.png" "a.png" 10 (Just "ours")]
    file (cancelled ++ [again]) `shouldBe` [(Waiting, 0)]
    commands (cancelled ++ [FriendOnline alice False]) again `shouldBe` []
    commands (cancelled ++ [FriendOnline alice False, again]) (FriendOnline alice True)
      `shouldContain` [SendFile alice i "/sent/a.png" "a.png" 10 (Just "ours")]
    -- Only once, and never theirs.
    commands (cancelled ++ [again]) again `shouldBe` []
    let offered = ours ++ [FileOffered alice (Receiving 1) (at 3 9 1) "b.txt" 20 "/received/b.txt" "b"]
        j = messageAt alice 4 (applying offered sample)
    commands (offered ++ [Cancel alice j]) (SendAgain alice j) `shouldBe` []

  it "calls an online friend, talks once they answer, and hangs up" $ do
    let calling = [StartCall alice (at 3 9 0) False]
        talking = calling ++ [CallPeer alice (at 3 9 0) True]
        phase events = callPhase <$> clientCall (applying events sample)
    commands [] (StartCall alice (at 3 9 0) False) `shouldBe` [CallFriend alice False]
    commands [] (StartCall bob (at 3 9 0) False) `shouldBe` []
    commands calling ToggleCamera `shouldBe` []
    phase talking `shouldBe` Just (Talking (at 3 9 0))
    callPeerVideo <$> clientCall (applying talking sample) `shouldBe` Just True
    commands talking ToggleCamera `shouldBe` [CameraCall alice True]
    -- Muting just drops our microphone frames (nothing for Tox to do).
    commands talking ToggleMute `shouldBe` []
    callMuted <$> clientCall (applying (talking ++ [ToggleMute]) sample) `shouldBe` Just True
    commands talking (HangUp (at 3 9 1)) `shouldBe` [HangUpCall alice]
    phase (talking ++ [HangUp (at 3 9 1)]) `shouldBe` Nothing

  it "answers a call, and turns away another during it" $ do
    let ringing = [CallInvited alice (at 3 9 0) False]
    callPhase <$> clientCall (applying ringing sample) `shouldBe` Just Ringing
    commands ringing (AnswerCall (at 3 9 0) True) `shouldBe` [AnswerFriend alice True]
    commands ringing (CallInvited bob (at 3 9 0) False) `shouldBe` [HangUpCall bob]
    clientCall (applying (ringing ++ [CallEnded bob (at 3 9 0)]) sample) `shouldBe` clientCall (applying ringing sample)
    clientCall (applying (ringing ++ [CallEnded alice (at 3 9 0)]) sample) `shouldBe` Nothing

  it "misses a call while another profile has one, and doesn't place ours" $ do
    let ringing = [CallInvited alice (at 3 9 0) False]
        missed = applying (ringing ++ [CallElsewhere (at 3 9 0)]) sample
    commands ringing (CallElsewhere (at 3 9 0)) `shouldBe` [HangUpCall alice]
    (clientCall missed, messageCall (last (messagesOf alice missed))) `shouldBe` (Nothing, Just (CallRecord False Nothing True))
    clientCall (applying [StartCall alice (at 3 9 0) False, CallElsewhere (at 3 9 0)] sample) `shouldBe` Nothing

  it "settles a call both ends placed at once" $ do
    -- Our key is between Alice's and Bob's.
    let placed cid = [FriendOnline bob True, StartCall cid (at 3 9 0) False, CallGlared cid (at 3 9 0)]
        phase events = callPhase <$> clientCall (applying events sample)
        later n = addUTCTime n (at 3 9 0)
    commands (placed alice) (Tick (at 3 9 0)) `shouldBe` []
    phase (placed alice ++ [Tick (later 2), CallGlared alice (later 3)]) `shouldBe` Nothing
    commands (placed alice) (Tick (later 2)) `shouldBe` [CallFriend alice False]
    commands (placed bob) (Tick (later 2)) `shouldBe` []
    commands (placed bob) (CallInvited bob (at 3 9 0) False) `shouldBe` [AnswerFriend bob False]
    phase (placed bob ++ [CallInvited bob (at 3 9 0) False]) `shouldBe` Just (Talking (at 3 9 0))
    phase (placed bob ++ [Tick (later 8)]) `shouldBe` Nothing
    commands (placed alice) (CallInvited carol (at 3 9 0) False) `shouldBe` [HangUpCall carol]
    -- Their call came before ours went out.
    commands [StartCall alice (at 3 9 0) False] (CallInvited alice (at 3 9 0) True) `shouldBe` [AnswerFriend alice False]

  it "gives up a call nobody answers" $ do
    let calling = [StartCall alice (at 3 9 0) False]
    commands calling (Tick (at 3 9 0)) `shouldBe` []
    commands calling (Tick (at 3 9 1)) `shouldBe` [HangUpCall alice]
    clientCall (applying (calling ++ [Tick (at 3 9 1)]) sample) `shouldBe` Nothing
    commands (calling ++ [CallPeer alice (at 3 9 0) False]) (Tick (at 3 10 0)) `shouldBe` []

  it "keeps each call in its chat, with missed ones unread" $ do
    let entries events = [ (messageMine m, messageTime m, c) | m <- messagesOf alice (applying events sample), Just c <- [messageCall m] ]
        unread events = contactUnread <$> Map.lookup alice (clientContacts (applying events sample))
        talked = [StartCall alice (at 3 9 0) True, CallPeer alice (at 3 9 0) True, HangUp (at 3 9 2)]
        missed = [CallInvited alice (at 3 9 0) False, CallEnded alice (at 3 9 1)]
    entries talked `shouldBe` [(True, at 3 9 0, CallRecord True (Just 120) False)]
    -- Already there during the call, so it stays if the app stops.
    entries (init talked) `shouldBe` [(True, at 3 9 0, CallRecord True Nothing False)]
    entries [CallInvited alice (at 3 9 0) False, HangUp (at 3 9 1)] `shouldBe` [(False, at 3 9 0, CallRecord False Nothing False)]
    entries missed `shouldBe` [(False, at 3 9 0, CallRecord False Nothing True)]
    unread missed `shouldBe` (succ <$> unread [])
    entries [CallInvited bob (at 3 9 0) False, CallInvited alice (at 3 9 1) True]
      `shouldBe` [(False, at 3 9 1, CallRecord True Nothing True)]

  it "keeps what is picked with the draft until sent, in order, or taken away" $ do
    let picked = [ Stage alice (StagedFile "/sent/a.png" "a.png" 10 "a"), Stage alice (StagedPlace 1 2)
                 , Stage alice (StagedFile "/sent/b.txt" "b.txt" 20 "b"), SetDraft (FriendChat alice) "look" ]
    commands picked (Unstage alice 0) `shouldBe` [ForgetFile "/sent/a.png"]
    let sent n = messageAt alice n (applying (picked ++ [Unstage alice 0, Send (FriendChat alice) (at 3 9 0)]) sample)
    commands (picked ++ [Unstage alice 0]) (Send (FriendChat alice) (at 3 9 0))
      `shouldBe` [ SendMessage alice (sent 3) "geo:1.000000,2.000000" 0
                 , SendFile alice (sent 4) "/sent/b.txt" "b.txt" 20 (Just "b"), SendMessage alice (sent 5) "look" 0 ]
    Map.lookup alice (clientStaged (applying (picked ++ [Send (FriendChat alice) (at 3 9 0)]) sample)) `shouldBe` Nothing
    commands picked (DeleteContact alice False) `shouldContain` [ForgetFile "/sent/a.png", ForgetFile "/sent/b.txt"]

  it "shows friends our picture, now and when they come back" $ do
    let picked = [SetAvatar "png"]
    commands [] (SetAvatar "png") `shouldBe` [SendAvatar alice (Just "png")]
    commands (picked ++ [FriendOnline alice False]) (FriendOnline alice True) `shouldSatisfy` elem (SendAvatar alice (Just "png"))
    commands picked RemoveAvatar `shouldBe` [SendAvatar alice Nothing]
    Map.size (clientAvatars (applying (picked ++ [RemoveAvatar]) sample)) `shouldBe` 0

  it "keeps the picture a friend shows while it shows it" $ do
    let shown' = applying [FriendAvatar alice (Just "theirs")] sample
    ((`Map.lookup` clientAvatars shown') =<< contactAvatar =<< Map.lookup alice (clientContacts shown'))
      `shouldBe` Just "theirs"
    clientAvatars (applying [FriendAvatar alice (Just "theirs"), FriendAvatar alice Nothing] sample) `shouldBe` Map.empty

  it "keeps a friend's status while online only" $ do
    let status events = contactStatus <$> Map.lookup alice (clientContacts (applying events sample))
    status [FriendStatus alice Tox.Busy] `shouldBe` Just Tox.Busy
    status [FriendStatus alice Tox.Busy, FriendOnline alice False] `shouldBe` Just Tox.Online

  it "deletes a contact and tells Tox, keeping its chat when asked" $ do
    let ContactId key = bob
        kept = applying [SetDraft (FriendChat bob) "hi", Send (FriendChat bob) (at 3 9 0), DeleteContact bob True] sample
    commands [] (DeleteContact bob False) `shouldBe` [Unfriend bob]
    Map.member bob (clientContacts kept) `shouldBe` False
    Map.member bob (clientFormer (applying [DeleteContact bob False] sample)) `shouldBe` False
    let again = applying [AddFriend (toxId key "DEADBEEF") "hi"] kept
    map messageText (messagesOf bob again) `shouldBe` ["hi"]
    Map.member bob (clientFormer again) `shouldBe` False

  it "ends a call with a contact it deletes" $ do
    let calling = [StartCall alice (at 3 9 0) False]
    commands calling (DeleteContact alice False) `shouldBe` [HangUpCall alice, Unfriend alice]
    clientCall (applying (calling ++ [DeleteContact alice False]) sample) `shouldBe` Nothing

  it "answers a message with its lines quoted" $ do
    let chat = FriendChat bob
        written = [SetDraft chat "two\nlines", Send chat (at 3 9 0)]
        answered = written ++ [ReplyTo chat (messageAt bob 0 (applying written sample)), SetDraft chat "ok"]
        sent = applying (answered ++ [Send chat (at 3 9 1)]) sample
    commands (answered ++ [FriendOnline bob True]) (Send chat (at 3 9 1)) `shouldBe` [SendMessage bob (messageAt bob 1 sent) "> two\n> lines\nok" 0]
    map messageText (messagesOf bob sent) `shouldBe` ["two\nlines", "> two\n> lines\nok"]
    Map.lookup bob (clientReplies sent) `shouldBe` Nothing

  it "answers what members said to everyone in a group, and forwards it" $ do
    let chat = GroupChat team
        said = inGroup ++ [ GroupReceived team (at 3 9 1) 1 Tox.MessageNormal "hello" Nothing
                          , PrivateReceived team (at 3 9 2) 1 Tox.MessageNormal "psst", MemberLeft team (at 3 9 2) 1 Tox.ExitQuit ]
        hello = entryAt team 0 (applying said sample)
        psst = entryAt team 1 (applying said sample)
        went = entryAt team 2 (applying said sample)
        answered = said ++ [ReplyTo chat went, ReplyTo chat hello, SetDraft chat "hi"]
        sent = applying (answered ++ [Send chat (at 3 9 3)]) sample
    commands answered (Send chat (at 3 9 3)) `shouldBe` [SendGroupMessage team (entryAt team 3 sent) "> hello\nhi"]
    Map.lookup team (clientGroupReplies sent) `shouldBe` Nothing
    -- Nor what was said privately, as the answer may go to everyone.
    mapM_ (\i -> replied (applying (said ++ [ReplyTo chat i]) sample) chat `shouldBe` Nothing) [went, psst]
    map messageText (messagesOf bob (applying (said ++ [Forward chat [went, hello] bob (at 3 9 3)]) sample))
      `shouldBe` ["hello"]

  it "forwards messages in order, sending them once the friend is online" $ do
    let chosen = [messageAt alice 2 sample, messageAt alice 0 sample]
        forwarded n = messageAt alice n (applying [Forward (FriendChat alice) chosen alice (at 3 9 0)] sample)
    commands [] (Forward (FriendChat alice) chosen alice (at 3 9 0))
      `shouldBe` [ SendMessage alice (forwarded 3) "Hi! Is this the Haskell client?" 0
                 , SendMessage alice (forwarded 4) "Looks good already." 0 ]
    commands [] (Forward (FriendChat alice) chosen bob (at 3 9 0)) `shouldBe` []
    map messageText (messagesOf bob (applying [Forward (FriendChat alice) chosen bob (at 3 9 0)] sample))
      `shouldBe` ["Hi! Is this the Haskell client?", "Looks good already."]

  it "accepts or rejects a friend request" $ do
    Map.member carol (clientRequests sample) `shouldBe` True
    let accepted = applying [AcceptRequest carol] sample
    Map.member carol (clientContacts accepted) `shouldBe` True
    Map.member carol (clientRequests accepted) `shouldBe` False
    commands [] (AcceptRequest carol) `shouldBe` [Befriend carol]
    let rejected = applying [RejectRequest carol] sample
    Map.member carol (clientContacts rejected) `shouldBe` False
    Map.member carol (clientRequests rejected) `shouldBe` False

  it "keeps a rejected request away while it says the same" $ do
    let again message = Map.member carol (clientRequests (applying [RejectRequest carol, Requested carol message] sample))
    again "Hey, I'm Carol, come chat with me on Tox!" `shouldBe` False
    again "It's me again" `shouldBe` True

  it "adds a contact by Tox ID" $ do
    let key = Text.replicate 8 "0123ABCD"
        address = toxId key "DEADBEEF"
    Map.member (ContactId key) (clientContacts (applying [AddFriend address "hi"] sample)) `shouldBe` True
    commands [] (AddFriend address " hi ") `shouldBe` [RequestFriend address "hi"]

  it "sends our request again after a restart, until the friend is online" $ do
    let key = Text.replicate 8 "0123ABCD"
        address = toxId key "DEADBEEF"
        added = [AddFriend address "hi"]
    commands added (Started "") `shouldSatisfy` elem (RequestFriend address "hi")
    commands (added ++ [FriendOnline (ContactId key) True]) (Started "") `shouldSatisfy` elem (Befriend (ContactId key))

  it "takes back a contact Tox refused" $ do
    let address = toxId (Text.replicate 8 "0123ABCD") "DEADBEEF"
    clientContacts (applying [AddFriend address "hi", AddFailed address Nothing] sample) `shouldBe` clientContacts sample

  it "shows us away to friends and in each group, also those joined later" $ do
    let away = (clientProfile sample) { profileAway = Tox.Away }
    commands [] (SetAway Tox.Away) `shouldBe` [SetProfile away]
    commands (SetAway Tox.Away : take 2 inGroup) (GroupConnected team (at 3 9 0)) `shouldBe` [SetGroupStatus team Tox.Away]

  it "tells a new Tox the profile and friends" $
    snd (update unseen (Started "") sample) `shouldBe` [SetProfile (clientProfile sample), Befriend alice, Befriend bob]
