{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Vulkan instance
module Popcorn.Engine.Renderer.Vulkan.Instance
    ( -- * Resource creation/release
      withInstance
    ) where

import Control.Exception (catch, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (MonadManaged)
import Data.Version (Version(versionBranch))
import Data.Word (Word32)
import Paths_Engine (version)

import Popcorn.Common.Log.Logger (engineLog)
import Popcorn.Engine.Application (Application(..))
import Popcorn.Engine.Exception (fromVulkanException)
import Popcorn.Engine.Managed.Extra (bracketManaged)
import Popcorn.Engine.Renderer.Vulkan.Utils (formatList)

import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Exception as Vk
import qualified Vulkan.Version as Vk
import qualified Vulkan.Zero as Vk

-- | Bracket for creating/releasing the Vulkan instance
withInstance
    :: MonadManaged m
    => Application                     -- ^ Application properties
    -> V.Vector BS.ByteString          -- ^ Vulkan instance extensions
    -> m Vk.Instance                   -- ^ Managed Vulkan Instance
withInstance app instanceExtensions =
    bracketManaged (createInstance app instanceExtensions) destroyInstance

createInstance
    :: Application
    -> V.Vector BS.ByteString
    -> IO Vk.Instance
createInstance app instanceExtensions =
    (Vk.createInstance instanceCreateInfo Nothing
        <* engineLog (formatList "Vulkan layers" layers))
    `catch` handler
  where
    debugMode = applicationDebugEnabled app
    layers = if debugMode then debugLayers else []
    instanceCreateInfo = mkInstanceCreateInfo app instanceExtensions layers

    -- exception handler that falls back to non-debug mode if needed
    handler :: Vk.VulkanException -> IO Vk.Instance
    handler (Vk.VulkanException Vk.ERROR_LAYER_NOT_PRESENT) = do
        liftIO $ engineLog
          "Cannot load Vulkan validation layers, falling back to non-debug mode!"
        createInstance
            (app { applicationDebugEnabled = False }) instanceExtensions

    handler e@(Vk.VulkanException _) =
        liftIO (throwIO (fromVulkanException e))

destroyInstance :: Vk.Instance -> IO ()
destroyInstance vkInstance = Vk.destroyInstance vkInstance Nothing

debugLayers :: V.Vector BS.ByteString
debugLayers = ["VK_LAYER_KHRONOS_validation"]

mkInstanceCreateInfo
  :: Application
  -> V.Vector BS.ByteString
  -> V.Vector BS.ByteString
  -> Vk.InstanceCreateInfo '[]
mkInstanceCreateInfo Application{..} extensions layers = Vk.zero
    { Vk.applicationInfo = Just Vk.zero
        { Vk.applicationName = Just (T.encodeUtf8 applicationName)
        , Vk.applicationVersion = makeVulkanVersion applicationVersion
        , Vk.engineName = Just "Popcorn"
        , Vk.engineVersion = makeVulkanVersion version
        , Vk.apiVersion = Vk.API_VERSION_1_0
        }
    , Vk.enabledLayerNames = layers
    , Vk.enabledExtensionNames = extensions
    }

makeVulkanVersion :: Version -> Word32
makeVulkanVersion v = Vk.MAKE_VERSION versionA versionB versionC
  where
    versionA = fromIntegral (head (versionBranch v))
    versionB = fromIntegral (versionBranch v !! 1)
    versionC = fromIntegral (versionBranch v !! 2)