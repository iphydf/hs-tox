{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
-- | The echo bot's avatar, apart from the bot so that compiling it doesn't
-- load the bot's C libraries.
module Avatar (avatar) where

import qualified Data.ByteString as BS
import           Data.FileEmbed  (embedFile)

avatar :: BS.ByteString
avatar = $(embedFile AVATAR)
