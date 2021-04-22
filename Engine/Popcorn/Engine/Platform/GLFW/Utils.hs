-- | Random collection of GLFW platform misc utility functions
module Popcorn.Engine.Platform.GLFW.Utils
    ( glfwLastErrorFriendlyDesc
    ) where

import qualified Data.Text as T
import qualified Graphics.UI.GLFW as GLFW

-- | Retrieves, unsets, and formats nicely the last GLFW error
glfwLastErrorFriendlyDesc :: IO T.Text
glfwLastErrorFriendlyDesc = do
    GLFW.getError >>= \case
        Nothing -> return "GLFW_NO_ERROR"
        Just (errorCode, errorDesc) ->
            return (mconcat
                [ "Platform error "
                , translateGLFWError errorCode
                , " \""
                , T.pack errorDesc
                , "\""
                ])

translateGLFWError :: GLFW.Error -> T.Text
translateGLFWError err = case err of
    GLFW.Error'NotInitialized     -> "GLFW_NOT_INITIALIZED"
    GLFW.Error'NoCurrentContext   -> "GLFW_NO_CURRENT_CONTEXT"
    GLFW.Error'InvalidEnum        -> "GLFW_INVALID_ENUM"
    GLFW.Error'InvalidValue       -> "GLFW_INVALID_VALUE"
    GLFW.Error'OutOfMemory        -> "GLFW_OUT_OF_MEMORY"
    GLFW.Error'ApiUnavailable     -> "GLFW_API_UNAVAILABLE"
    GLFW.Error'VersionUnavailable -> "GLFW_VERSION_UNAVAILABLE"
    GLFW.Error'PlatformError      -> "GLFW_PLATFORM_ERROR"
    GLFW.Error'FormatUnavailable  -> "GLFW_FORMAT_UNAVAILABLE"