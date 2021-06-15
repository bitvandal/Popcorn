{-# LANGUAGE DataKinds #-}

-- | Vulkan Command Buffers
module Popcorn.Engine.Renderer.Vulkan.CommandBuffer
    ( -- * Resource creation/release
      withCommandPool
    , withCommandBuffers

      -- * Command Pools management
    , resetCommandPool
    , recycleCommandPool
    , recordSingleUseCommandBuffer

      -- * Command buffers allocation
    , allocateCommandBuffers
    , freeCommandBuffers
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Managed(MonadManaged)

import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Zero as Vk

import Popcorn.Engine.Managed.Extra (bracketManaged)
import Popcorn.Engine.Renderer.Vulkan.PhysicalDevice (QueueFamily(queueFamilyIndex))

-- | Bracket for creating/releasing a Vulkan Command Pool
withCommandPool
    :: MonadManaged m
    => Vk.Device
    -> QueueFamily
    -> m Vk.CommandPool
withCommandPool device queueFamily =
    bracketManaged
        (createCommandPool device queueFamily)
        (destroyCommandPool device)

-- Creates a new command pool
createCommandPool :: Vk.Device -> QueueFamily -> IO Vk.CommandPool
createCommandPool device queueFamily =
    Vk.createCommandPool device (mkCommandPoolCreateInfo queueFamily) Nothing

mkCommandPoolCreateInfo :: QueueFamily -> Vk.CommandPoolCreateInfo
mkCommandPoolCreateInfo queueFamily = Vk.zero
    { Vk.flags = Vk.zero
    , Vk.queueFamilyIndex = queueFamilyIndex queueFamily
    }

-- Destroys an existing command pool
destroyCommandPool :: Vk.Device -> Vk.CommandPool -> IO ()
destroyCommandPool device commandPool = Vk.destroyCommandPool device commandPool Nothing

-- | Resets a Vulkan Command Pool:
--
-- - recycles all of the resources from all of the command buffers allocated from the
--   command pool back to the command pool
-- - all command buffers that have been allocated from the command pool are put in the
--   initial state (note that they are not freed)
resetCommandPool :: Vk.Device -> Vk.CommandPool -> IO ()
resetCommandPool device commandPool =
    Vk.resetCommandPool device commandPool Vk.zero

-- | Resets a Vulkan Command Pool, and recycles all of the resources from the
--   command pool back to the system  
recycleCommandPool :: Vk.Device -> Vk.CommandPool -> IO ()
recycleCommandPool device commandPool =
    Vk.resetCommandPool device commandPool Vk.COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT

-- | Bracket for allocating/freeing a list of Vulkan Command Buffers
withCommandBuffers
    :: MonadManaged m
    => Vk.Device
    -> Vk.CommandPool
    -> Int
    -> m (V.Vector Vk.CommandBuffer)
withCommandBuffers device commandPool count =
    bracketManaged
        (allocateCommandBuffers device commandPool count)
        (freeCommandBuffers device commandPool)

-- | Allocates a list of Vulkan command buffers
allocateCommandBuffers
    :: Vk.Device
    -> Vk.CommandPool
    -> Int
    -> IO (V.Vector Vk.CommandBuffer)
allocateCommandBuffers device commandPool count =
    Vk.allocateCommandBuffers device (mkCommandBufferAllocateInfo commandPool count)

-- | Frees a list of Vulkan command buffers
freeCommandBuffers
    :: Vk.Device
    -> Vk.CommandPool
    -> V.Vector Vk.CommandBuffer
    -> IO ()
freeCommandBuffers = Vk.freeCommandBuffers

mkCommandBufferAllocateInfo :: Vk.CommandPool -> Int -> Vk.CommandBufferAllocateInfo
mkCommandBufferAllocateInfo commandPool count = Vk.zero
    { Vk.commandPool = commandPool
    , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
    , Vk.commandBufferCount = fromIntegral count
    }

-- | Wraps recording a single time use command buffer that, when it is executed, moves
-- to the Invalid state 
recordSingleUseCommandBuffer :: MonadIO io => Vk.CommandBuffer -> io r -> io r
recordSingleUseCommandBuffer cb = Vk.useCommandBuffer cb
    (mkCommandBufferBeginInfo Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)

mkCommandBufferBeginInfo :: Vk.CommandBufferUsageFlags -> Vk.CommandBufferBeginInfo '[]
mkCommandBufferBeginInfo flags = Vk.zero
    { Vk.flags = flags
    , Vk.inheritanceInfo = Nothing
    }