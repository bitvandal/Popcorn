-- | Random collection of Vulkan renderer misc utility functions
module Popcorn.Engine.Renderer.Vulkan.Utils
    ( formatList
    ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

-- | Formats a vector of ByteStrings to Text
formatList :: T.Text -> V.Vector BS.ByteString -> T.Text
formatList desc xs = mconcat
    [ desc
    , ": ["
    , T.decodeUtf8 (BS.intercalate ", " (V.toList xs))
    , "]"
    ]