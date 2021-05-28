-- | Vulkan surfaces (GLFW platform specific)
module Popcorn.Engine.Renderer.Vulkan.Platform.GLFW
    ( -- * Vulkan Setup
      vulkanRequiredInstanceExtensions
    
      -- * WSI Surface creation
    , createVulkanSurface

      -- * Surface queries
    , supportsPresentationPlatform
    ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Word (Word32)

import Popcorn.Common.Log.Logger (engineLog)
import Popcorn.Engine.Exception (EngineException(EngineException))
import Popcorn.Engine.Platform.GLFW.Utils (glfwLastErrorFriendlyDesc)
import Popcorn.Engine.Platform.GLFW.Window (Window(..))

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Foreign
import qualified Foreign.C.String as Foreign
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as Vk

-- | Returns the list of Vulkan instance extensions required for GLFW windows
vulkanRequiredInstanceExtensions :: IO (V.Vector BS.ByteString)
vulkanRequiredInstanceExtensions = do
    exts <- GLFW.getRequiredInstanceExtensions
    asStrings <- traverse Foreign.peekCString exts
    let encoded = T.encodeUtf8 . T.pack <$> asStrings
    return (V.fromList encoded)

-- | Creates a WSI Vulkan surface
createVulkanSurface
    :: Window               -- ^ Window to create the surface for
    -> Vk.Instance          -- ^ Vulkan instance to create the surface in
    -> IO Vk.SurfaceKHR     -- ^ Returns the Vulkan surface handle
createVulkanSurface window inst =
    Foreign.alloca $ \ptr -> do
        vkResult <- GLFW.createWindowSurface
                        (Foreign.castPtr (Vk.instanceHandle inst))
                        (windowHandle window)
                        Foreign.nullPtr
                        ptr

        case Vk.Result vkResult of
            Vk.SUCCESS -> Foreign.peek ptr
            vkError -> do
                err <- glfwLastErrorFriendlyDesc
                let vulkanError = "VK_" <> T.pack (show vkError)
                let errorDesc = mconcat
                        [ "[Renderer] Error creating Vulkan surface: "
                        , vulkanError
                        , ". "
                        , err
                        ]

                throwIO (EngineException errorDesc)

-- | Queries whether the specified queue family can present images to the current platform
supportsPresentationPlatform
    :: Vk.Instance
    -> Vk.PhysicalDevice
    -> (Word32, Vk.QueueFamilyProperties)
    -> IO Bool
supportsPresentationPlatform inst physicalDevice (queueFamilyIndex, _) = do
    result <- GLFW.getPhysicalDevicePresentationSupport
        (Foreign.castPtr (Vk.instanceHandle inst))
        (Foreign.castPtr (Vk.physicalDeviceHandle physicalDevice))
        queueFamilyIndex
        
    unless result $ do
        err <- glfwLastErrorFriendlyDesc
        engineLog ("Warning: while checking for presentation support="
            <> T.pack (show err))

    pure result