{-# LANGUAGE OverloadedStrings #-}
-- | Bots that relay another network's chat into a group. They send "<name>
-- text" for each message, and the user can have those shown under the
-- name. Only this module knows the format.
module Tox.Client.Relays
  ( relayed
  , relayedNames
  ) where

import           Data.List (sortOn)
import qualified Data.Map  as Map
import           Data.Ord  (Down (..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (UTCTime)

-- | Sender and text. Names may contain "> ", so the longest known name
-- wins over the first "> ".
relayed :: [Text] -> Text -> Maybe (Text, Text)
relayed names text = do
  rest <- Text.stripPrefix "<" text
  let known = [n | n <- names, not (Text.null n), (n <> "> ") `Text.isPrefixOf` rest]
      (name, said) = case known of
        [] -> Text.breakOn "> " rest
        _  -> Text.splitAt (maximum (map Text.length known)) rest
  if Text.null name || Text.null said then Nothing else Just (name, Text.drop 2 said)

-- | The names relayed, with the last time each was, latest first.
relayedNames :: [Text] -> [(UTCTime, Text)] -> [(Text, UTCTime)]
relayedNames names said =
  sortOn (Down . snd) (Map.toList (Map.fromListWith max [(name, time) | (time, text) <- said, Just (name, _) <- [relayed names text]]))
