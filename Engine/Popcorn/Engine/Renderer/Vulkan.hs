-- | The Vulkan Renderer
module Popcorn.Engine.Renderer.Vulkan
    ( -- * Types
      Renderer

      -- * Initialization
    , withVulkanRenderer

      -- * Accessors
    , getVulkanInstance
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (MonadManaged)

import Popcorn.Common.Log.Logger (engineLog)
import Popcorn.Engine.Application (Application(..))
import Popcorn.Engine.Renderer.Vulkan.Instance (withInstance)
import Popcorn.Engine.Renderer.Vulkan.LogicalDevice (withLogicalDevice)
import Popcorn.Engine.Renderer.Vulkan.PhysicalDevice
    ( GraphicsDevice
    , selectGraphicsDevice
    , graphicsDeviceFriendlyDesc
    )
import Popcorn.Engine.Renderer.Vulkan.Utils (formatList)

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk

-- | The Vulkan renderer
data Renderer = Renderer
  { rInstance :: Vk.Instance
  , rGraphicsDevice :: GraphicsDevice
  , rLogicalDevice :: Vk.Device
  }

-- | Return the Vulkan renderer
withVulkanRenderer
    :: MonadManaged m
    => Application
    -> V.Vector BS.ByteString
    -> m Renderer
withVulkanRenderer app instanceExtensions = do
    rInstance <- withInstance app instanceExtensions
    rGraphicsDevice <- liftIO (selectGraphicsDevice rInstance)
    rLogicalDevice <- withLogicalDevice rGraphicsDevice

    liftIO (engineLog (formatList "Vulkan instance extensions" instanceExtensions)
        >>  engineLog "Initialized Renderer: Vulkan 1.0"
        >>  engineLog (graphicsDeviceFriendlyDesc rGraphicsDevice))

    pure Renderer{..}

-- | Returns Renderer's Vulkan instance
getVulkanInstance :: Renderer -> Vk.Instance
getVulkanInstance = rInstance