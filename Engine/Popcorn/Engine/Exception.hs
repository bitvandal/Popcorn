-- | Engine exceptions
module Popcorn.Engine.Exception
    ( -- * Types
      EngineException(..)

      -- * Exceptions Conversion
    , fromVulkanException
    )
    where

import Control.Exception (Exception, displayException)

import qualified Data.Text as T
import qualified Vulkan.Exception as Vk

-- | Engine Exception type
newtype EngineException = EngineException
    { eeDescription :: T.Text
    } deriving stock (Eq, Show)

instance Exception EngineException

-- | Converts a Vulkan Renderer exception to a Engine Exception
fromVulkanException :: Vk.VulkanException -> EngineException
fromVulkanException e = EngineException ("[Vulkan] VK_" <> T.pack (displayException e))
