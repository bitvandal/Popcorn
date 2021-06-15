{-# LANGUAGE DataKinds #-}

-- | Vulkan Images and Image views
module Popcorn.Engine.Renderer.Vulkan.Image
    ( -- * Data types
      Image(..)

      -- * Resource creation/release
    , withImageView
    , mkImageSubresourceRangeWholeImage

    -- * Queries
    , canCreateImage
    )
    where

import Control.Exception (catch)
import Control.Monad.Managed (MonadManaged)

import Popcorn.Engine.Exception (showVulkanException)
import Popcorn.Engine.Managed.Extra (bracketManaged)
import Popcorn.Engine.Renderer.Vulkan.PhysicalDevice(VulkanDevice(..))

import qualified Data.Text as T
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Exception as Vk
import qualified Vulkan.Zero as Vk

-- | Vulkan image and associated data
data Image = Image
    { imageHandle :: Vk.Image
    , imageFormat :: Vk.Format
    }

-- | Bracket for creating/releasing a Vulkan Image View
withImageView
    :: MonadManaged m
    => Vk.Device
    -> Image
    -> m Vk.ImageView
withImageView device image =
    bracketManaged (createImageView device image) (destroyImageView device)

createImageView :: Vk.Device -> Image -> IO Vk.ImageView
createImageView device image = Vk.createImageView
    device (mkImageViewCreateInfo image) Nothing

destroyImageView :: Vk.Device -> Vk.ImageView -> IO ()
destroyImageView device imageView = Vk.destroyImageView device imageView Nothing

mkImageViewCreateInfo :: Image -> Vk.ImageViewCreateInfo '[]
mkImageViewCreateInfo Image{..} = 
    let componentMapping = Vk.zero
            { Vk.r = Vk.COMPONENT_SWIZZLE_IDENTITY
            , Vk.g = Vk.COMPONENT_SWIZZLE_IDENTITY
            , Vk.b = Vk.COMPONENT_SWIZZLE_IDENTITY
            , Vk.a = Vk.COMPONENT_SWIZZLE_IDENTITY
            }

        range = mkImageSubresourceRangeWholeImage Vk.IMAGE_ASPECT_COLOR_BIT
    in
        Vk.zero
            { Vk.image = imageHandle
            , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
            , Vk.format = imageFormat
            , Vk.components = componentMapping
            , Vk.subresourceRange = range
            }

-- | Checks that an image with the indicated properties can be created.
canCreateImage
    :: VulkanDevice
    -> Vk.Format
    -> Vk.ImageType
    -> Vk.ImageTiling
    -> Vk.ImageUsageFlags
    -> Vk.ImageCreateFlags
    -> Vk.Extent2D
    -> IO (Either T.Text ())
canCreateImage vd format imageType tiling usage flags (Vk.Extent2D w h) =
    check `catch` (\(e :: Vk.VulkanException) ->
        pure (Left (showVulkanException e)))
  where
    check = do
        properties <- queryImageFormatProperties
            (vdPhysicalDevice vd) format imageType tiling usage flags

        let Vk.Extent3D maxWidth maxHeight _ = Vk.maxExtent properties

        pure (if w > maxWidth || h > maxHeight
            then Left "Cannot create swapchain images of the requested size and format"
            else Right ())

-- Query for additional image capabilities
queryImageFormatProperties
    :: Vk.PhysicalDevice 
    -> Vk.Format
    -> Vk.ImageType
    -> Vk.ImageTiling
    -> Vk.ImageUsageFlags
    -> Vk.ImageCreateFlags
    -> IO Vk.ImageFormatProperties
queryImageFormatProperties physicalDevice format imageType tiling usage flags = do
    Vk.getPhysicalDeviceImageFormatProperties
        physicalDevice format imageType tiling usage flags

-- | Create an Image Subresource ranges for the whole image
mkImageSubresourceRangeWholeImage :: Vk.ImageAspectFlagBits -> Vk.ImageSubresourceRange
mkImageSubresourceRangeWholeImage aspectMask = Vk.zero
    { Vk.aspectMask = aspectMask
    , Vk.baseMipLevel = 0
    , Vk.levelCount = Vk.REMAINING_MIP_LEVELS 
    , Vk.baseArrayLayer = 0
    , Vk.layerCount = Vk.REMAINING_ARRAY_LAYERS 
    }