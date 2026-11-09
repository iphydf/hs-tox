{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | Bootstrap nodes: nodes.tox.chat's list, built in before each release
-- by tools/update-nodes, and read for tools that fetch it.
module Tox.Client.Nodes
  ( Node (..)
  , builtIn
  , nodesUrl
  , readNodes
  , usable
  , bootstrapFrom
  ) where

import           Control.Monad            (forM_, guard)
import qualified Crypto.Saltine.Class     as Sodium
import           Data.Bifunctor           (bimap)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base16   as Base16
import qualified Data.ByteString.Char8    as Char8
import           Data.Char                (chr, isDigit, isHexDigit, isSpace)
import           Data.Either              (fromRight)
import           Data.Maybe               (listToMaybe, mapMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Data.Word                (Word16)
import           Numeric                  (readHex)

import qualified Tox.Application.Tox      as Tox
import qualified Tox.Client.Nodes.BuiltIn as BuiltIn

data Node = Node
  { nodeHost    :: String
  , nodePort    :: Word16
  , nodeTcpPort :: Maybe Word16
    -- ^ If it's a TCP relay.
  , nodeKey     :: BS.ByteString
  , nodeUp      :: Bool
    -- ^ The list's last check reached it over TCP.
  } deriving (Eq, Show)

nodesUrl :: Text
nodesUrl = "https://nodes.tox.chat/json"

-- | The nodes a client starts from: nodes.tox.chat's list when it was
-- released.
builtIn :: [Node]
builtIn = map node BuiltIn.nodes
  where
    node (host, port, tcp, key, up) =
      Node host port (listToMaybe tcp) (fromRight BS.empty (Base16.decode (Char8.pack key))) up

-- | Nothing if it doesn't parse or has no nodes (nodes.tox.chat serves an
-- empty list when its scan finds none, and that mustn't replace ours).
readNodes :: BS.ByteString -> Maybe [Node]
readNodes bytes = do
  (Object top, rest) <- value (Char8.dropWhile isSpace bytes)
  guard (Char8.all isSpace rest)
  Array entries <- lookup "nodes" top
  let nodes = mapMaybe node entries
  guard (not (null nodes))
  pure nodes
  where
    node (Object fields) = do
      String host <- lookup "ipv4" fields
      guard (host `notElem` ["", "-", "NONE"])
      port <- portOf =<< lookup "port" fields
      String key <- lookup "public_key" fields
      Right raw <- Just (Base16.decode (Text.encodeUtf8 key))
      guard (BS.length raw == 32)
      let tcp = case lookup "tcp_ports" fields of
            Just (Array (p : _)) -> portOf p
            _                    -> Nothing
      pure (Node (Text.unpack host) port tcp raw (lookup "status_tcp" fields /= Just (Bool False)))
    node _ = Nothing
    portOf (Number n) | n > 0 && n < 65536 && n == fromIntegral (round n :: Int) = Just (round n)
    portOf _ = Nothing

-- | Ones Tox can bootstrap from, the reachable ones first.
usable :: [Node] -> [Node]
usable nodes = filter nodeUp relaying ++ filter (not . nodeUp) relaying
  where relaying = [n | n <- nodes, Just _ <- [nodeTcpPort n]]

-- | Adds the node as a TCP relay if it is one, and bootstraps from it.
bootstrapFrom :: Tox.Tox -> Node -> IO ()
bootstrapFrom tox n = forM_ (Sodium.decode (nodeKey n)) $ \pk -> do
  forM_ (nodeTcpPort n) $ \tcp -> Tox.resolveNode (nodeHost n) tcp pk >>= mapM_ (Tox.addTcpRelay tox)
  Tox.resolveNode (nodeHost n) (nodePort n) pk >>= mapM_ (Tox.bootstrap tox)

-- | Just enough JSON for the list.
data Json
  = Object [(Text, Json)]
  | Array [Json]
  | String Text
  | Number Double
  | Bool Bool
  | Null
  deriving (Eq, Show)

-- | A value and the rest, without the spaces in between.
value :: BS.ByteString -> Maybe (Json, BS.ByteString)
value input = case Char8.uncons input of
  Just ('{', rest) -> members [] (skip rest)
  Just ('[', rest) -> items [] (skip rest)
  Just ('"', rest) -> bimap String skip <$> string [] rest
  Just ('t', _) | Just rest <- BS.stripPrefix "true" input -> Just (Bool True, skip rest)
  Just ('f', _) | Just rest <- BS.stripPrefix "false" input -> Just (Bool False, skip rest)
  Just ('n', _) | Just rest <- BS.stripPrefix "null" input -> Just (Null, skip rest)
  Just (c, _) | c == '-' || isDigit c ->
    let (digits, rest) = Char8.span (\d -> isDigit d || d `elem` ("+-.eE" :: String)) input
    in case reads (Char8.unpack (fixed digits)) of
      [(n, "")] -> Just (Number n, skip rest)
      _         -> Nothing
  _ -> Nothing
  where
    skip = Char8.dropWhile isSpace
    -- Haskell can't read "1e+5".
    fixed = Char8.filter (/= '+')
    members acc s = case Char8.uncons s of
      Just ('}', rest) | null acc -> Just (Object [], skip rest)
      Just ('"', rest) -> do
        (key, afterKey) <- string [] rest
        (':', afterColon) <- Char8.uncons (skip afterKey)
        (v, afterValue) <- value (skip afterColon)
        let acc' = (key, v) : acc
        case Char8.uncons afterValue of
          Just (',', more) -> members acc' (skip more)
          Just ('}', more) -> Just (Object (reverse acc'), skip more)
          _                -> Nothing
      _ -> Nothing
    items acc s = case Char8.uncons s of
      Just (']', rest) | null acc -> Just (Array [], skip rest)
      _ -> do
        (v, afterValue) <- value s
        case Char8.uncons afterValue of
          Just (',', more) -> items (v : acc) (skip more)
          Just (']', more) -> Just (Array (reverse (v : acc)), skip more)
          _                -> Nothing
    string acc s = do
      let (plain, rest) = BS.break (\b -> b == 0x22 || b == 0x5c) s
          acc' = Text.decodeUtf8Lenient plain : acc
      (c, more) <- Char8.uncons rest
      if c == '"'
        then Just (Text.concat (reverse acc'), more)
        else do
          (e, after) <- Char8.uncons more
          case e of
            'u' -> do
              let (code, after') = BS.splitAt 4 after
              guard (BS.length code == 4 && Char8.all isHexDigit code)
              [(n, "")] <- Just (readHex (Char8.unpack code))
              -- Lone surrogates mean nothing.
              let ch = if n >= 0xd800 && n < 0xe000 then '\xfffd' else chr n
              string (Text.singleton ch : acc') after'
            _ -> do
              ch <- lookup e [('"', '"'), ('\\', '\\'), ('/', '/'), ('b', '\b'), ('f', '\f'), ('n', '\n'), ('r', '\r'), ('t', '\t')]
              string (Text.singleton ch : acc') after
