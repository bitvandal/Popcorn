module Main
    ( main
    ) where

import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (runManaged)
import Paths_Sandbox (version)

import Popcorn.Common.Log.Logger (clientLog)
import Popcorn.Engine.Application (Application(..), CliArgs(..), parseArgs)

import qualified Popcorn.Engine.Engine as Engine
import qualified Popcorn.Engine.Exception as Engine
import qualified Popcorn.Engine.Platform.GLFW.Init as Platform
import qualified Popcorn.Engine.Platform.GLFW.Vulkan as Platform
import qualified Popcorn.Engine.Renderer.Vulkan.Init as R

main :: IO ()
main = start `catch` (\(e :: Engine.EngineException) -> notifyException e)

start :: IO ()
start = do
    clientLog "Popcorn Sandbox is starting..."

    args <- parseArgs

    let app = Application "Sandbox" version (cliArgsDebugMode args)

    runManaged $ do
        Platform.withGlfwForVulkanPlatform

        vkInstanceExtensions <- liftIO Platform.vulkanRequiredInstanceExtensions
        _ <- R.withVulkanRenderer app vkInstanceExtensions

        _ <- Engine.withEngine

        return ()

    clientLog "Popcorn Sandbox has ended succesfully :)"

notifyException :: Engine.EngineException -> IO ()
notifyException e = clientLog ("Engine fatal error >>> " <> Engine.eeDescription e)