{-# LANGUAGE DataKinds #-}

-- | Vulkan Shader Modules
module Popcorn.Engine.Renderer.Vulkan.Shader
    ( -- * Loading/unloading shader bytecode
      loadShaderBytecode

      -- * Resource creation/release
    , withShaderModule
    ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.Managed (MonadManaged)
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))

import Popcorn.Common.Log.Logger (engineLog)
import Popcorn.Engine.Exception (EngineException(EngineException))
import Popcorn.Engine.Managed.Extra (bracketManaged)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Zero as Vk

-- | Bracket for creating/releasing Vulkan Shader modules
withShaderModule
    :: MonadManaged m
    => Vk.Device
    -> T.Text
    -> BS.ByteString
    -> m Vk.ShaderModule
withShaderModule device name bytes = bracketManaged
    (createShaderModule device name bytes)
    (destroyShaderModule device name)

-- Creates a Vulkan Shader module
createShaderModule :: Vk.Device -> T.Text -> BS.ByteString -> IO Vk.ShaderModule
createShaderModule device name bytes = do
    shaderModule <- Vk.createShaderModule device
        (mkShaderModuleCreateInfo bytes) Nothing

    engineLog ("Loaded Shader: " <> name)

    return shaderModule

-- Destroys existing graphics Vulkan Shadre modules
destroyShaderModule :: Vk.Device -> T.Text -> Vk.ShaderModule -> IO ()
destroyShaderModule device name shaderModule =
    Vk.destroyShaderModule device shaderModule Nothing
        >> engineLog ("Unloaded shader " <> name)

mkShaderModuleCreateInfo :: BS.ByteString -> Vk.ShaderModuleCreateInfo '[]
mkShaderModuleCreateInfo bytes = Vk.zero { Vk.code = bytes }

-- | Loads a pre-compiled Shader program bytecode from disk 
loadShaderBytecode :: T.Text -> IO BS.ByteString 
loadShaderBytecode shaderName = do
    let filename = "shaders" </> T.unpack shaderName <.> "spv"

    exists <- doesFileExist filename

    unless exists $ throwIO (EngineException
        ("Could not load shader: " <> shaderName <> " at location: " <> T.pack filename))

    BS.readFile filename