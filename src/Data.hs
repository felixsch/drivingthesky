module Data where

import Graphics.UI.GLFW
import System.FilePath.Posix

import Road


data Data = { glfwWindow  :: Window
            , currentRoad :: Maybe Road
            , runtimePath :: FilePath



