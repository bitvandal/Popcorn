-- | GLFW platform Vulkan supporting code
module Popcorn.Engine.Platform.GLFW.Vulkan
    ( -- * Vulkan Setup
      supportsVulkan
    , vulkanRequiredInstanceExtensions
    , setupVulkanSurface
    ) where

import Control.Exception (throwIO)
import Control.Monad.Managed (MonadManaged)

import Popcorn.Common.Log.Logger (platformLog)
import Popcorn.Engine.Exception (EngineException(EngineException))
import Popcorn.Engine.Managed.Extra (bracketManaged)
import Popcorn.Engine.Platform.GLFW.Window (Window(..))
import Popcorn.Engine.Platform.GLFW.Utils (glfwLastErrorFriendlyDesc)
import Popcorn.Engine.Renderer.Vulkan (Renderer, getVulkanInstance)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Foreign
import qualified Foreign.C.String as Foreign
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as Vk

-- | True if the GLFW has a minimal Vulkan support
supportsVulkan :: IO Bool
supportsVulkan =
    GLFW.vulkanSupported >>= \case
        True -> return True
        False -> do
            err <- glfwLastErrorFriendlyDesc
            platformLog ("[Platform] Failed while checking Vulkan support. " <> err)
            return False

-- | Returns the list of Vulkan instance extensions required for GLFW Windows
vulkanRequiredInstanceExtensions :: IO (V.Vector BS.ByteString)
vulkanRequiredInstanceExtensions = do
    exts <- GLFW.getRequiredInstanceExtensions
    asStrings <- traverse Foreign.peekCString exts
    let encoded = T.encodeUtf8 . T.pack <$> asStrings
    return (V.fromList encoded)

-- | Creates a Vulkan WSI surface for a specific platform window
setupVulkanSurface
    :: MonadManaged m
    => Window
    -> Renderer
    -> m Window
setupVulkanSurface window renderer = 
    bracketManaged (createVulkanSurface window renderer) (destroyVulkanSurface renderer)

createVulkanSurface
    :: Window
    -> Renderer
    -> IO Window
createVulkanSurface window renderer =
    Foreign.alloca $
        \ptr -> do
            let inst = getVulkanInstance renderer
            vkResult <- GLFW.createWindowSurface
                            (Foreign.castPtr (Vk.instanceHandle inst))
                            (windowHandle window)
                            Foreign.nullPtr
                            ptr

            case Vk.Result vkResult of
                Vk.SUCCESS -> do
                    surface <- Foreign.peek ptr
                    return window { windowVulkanSurface = Just surface }
                vkError -> do
                    err <- glfwLastErrorFriendlyDesc
                    let vulkanError = "VK_" <> T.pack (show vkError)
                    let errorDesc = mconcat
                            [ "[Platform] Error creating platform Vulkan surface: "
                            , vulkanError
                            , ". "
                            , err
                            ]

                    throwIO (EngineException errorDesc)

destroyVulkanSurface :: Renderer -> Window -> IO ()
destroyVulkanSurface renderer Window{..} = case windowVulkanSurface of
    Just surface -> Vk.destroySurfaceKHR (getVulkanInstance renderer) surface Nothing
    Nothing -> return ()