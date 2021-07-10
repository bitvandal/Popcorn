module Main (main) where

import Control.Exception (catch)
import Control.Monad.Managed (runManaged)
import Paths_Sandbox (version)

import Popcorn.Common.Log.Logger (clientLog)
import Popcorn.Engine.Application (Application(..), CliArgs(..), parseArgs)
import Popcorn.Engine.Settings
    ( Settings(..)
    , WindowMode(..)
    , defaultSettings
    )

import qualified Popcorn.Engine.Engine as Engine
import qualified Popcorn.Engine.Exception as Engine
import qualified Popcorn.Engine.Platform.GLFW as Platform
import qualified Popcorn.Engine.Platform.GLFW.Window as Platform

main :: IO ()
main = start `catch` (\(e :: Engine.EngineException) -> notifyException e)

start :: IO ()
start = do
    engineVersion <- Engine.showEngineVersion
    args <- parseArgs engineVersion

    clientLog "Popcorn Sandbox is starting..."

    let app = Application
            { applicationName = "Sandbox"
            , applicationVersion = version
            , applicationDebugEnabled = cliArgsDebugMode args
            , applicationMainWindowWidth = cliArgsAppMainWindowWidth args
            , applicationMainWindowHeight = cliArgsAppMainWindowHeight args
            }
        settings = defaultSettings { sWindowMode = WindowedResizable }

    runManaged $ do
        Platform.withGlfwForVulkanPlatform
        window <- Platform.withWindow app settings
        engine <- Engine.withEngineInteractive app window settings

        -- engine <- Engine.withEngineRenderStatic app settings

        Engine.run engine

    clientLog "Popcorn Sandbox has ended succesfully :)"

notifyException :: Engine.EngineException -> IO ()
notifyException e = clientLog ("Engine fatal error >>> " <> Engine.eeDescription e)