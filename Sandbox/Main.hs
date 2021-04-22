module Main
    ( main
    ) where

import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (runManaged)
import Paths_Sandbox (version)

import Popcorn.Common.Log.Logger (clientLog)
import Popcorn.Engine.Application (Application(..), CliArgs(..), parseArgs)
import Popcorn.Engine.Engine (showEngineVersion)

import qualified Popcorn.Engine.Engine as Engine
import qualified Popcorn.Engine.Exception as Engine
import qualified Popcorn.Engine.Platform.GLFW as Platform
import qualified Popcorn.Engine.Platform.GLFW.Vulkan as Platform
import qualified Popcorn.Engine.Platform.GLFW.Window as Platform
import qualified Popcorn.Engine.Renderer.Vulkan as R

main :: IO ()
main = start `catch` (\(e :: Engine.EngineException) -> notifyException e)

start :: IO ()
start = do
    engineVersion <- showEngineVersion
    args <- parseArgs engineVersion

    clientLog "Popcorn Sandbox is starting..."

    let app = Application
                "Sandbox"
                version
                (cliArgsDebugMode args)
                (cliArgsAppMainWindowWidth args)
                (cliArgsAppMainWindowHeight args)

    runManaged $ do
        Platform.withGlfwForVulkanPlatform

        -- only needed for rendering to a Window
        vkInstanceExtensions <- liftIO Platform.vulkanRequiredInstanceExtensions

        renderer <- R.withVulkanRenderer app vkInstanceExtensions

        window <- Platform.withWindow app

        -- only needed for rendering to a Window
        vkWindow <- Platform.setupVulkanSurface window renderer

        _ <- Engine.withEngine

        return ()

    clientLog "Popcorn Sandbox has ended succesfully :)"

notifyException :: Engine.EngineException -> IO ()
notifyException e = clientLog ("Engine fatal error >>> " <> Engine.eeDescription e)