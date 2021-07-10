module Popcorn.Engine.Platform.GLFW.Internal.Window
    ( -- * Types
      Window(..)
    )
    where

import qualified Graphics.UI.GLFW as GLFW

-- | The GLFW Platform window
newtype Window = Window
    { windowHandle :: GLFW.Window
    }