-- | GLFW platform Vulkan supporting code
module Popcorn.Engine.Platform.GLFW.Vulkan
    ( supportsVulkan
    , vulkanRequiredInstanceExtensions
    ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Foreign.C.String as Foreign
import qualified Graphics.UI.GLFW as GLFW

-- | True if the GLFW has a minimal Vulkan support
supportsVulkan :: IO Bool
supportsVulkan = GLFW.vulkanSupported

-- | Returns the list of Vulkan instance extensions required for GLFW Windows
vulkanRequiredInstanceExtensions :: IO (V.Vector BS.ByteString)
vulkanRequiredInstanceExtensions = do
    exts <- GLFW.getRequiredInstanceExtensions
    asStrings <- traverse Foreign.peekCString exts
    let encoded = T.encodeUtf8 . T.pack <$> asStrings
    return (V.fromList encoded)