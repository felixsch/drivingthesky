module Menu
  ( Menu(..)
  , renderMenu
  ) where

import Control.Monad

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Graphics.GLUtil.ShaderProgram
import Graphics.UI.GLFW

import Texture
import Resource
import Util
import {-# SOURCE #-} State





data Menu = Menu { title   :: String
                 ,  entries :: [(String, (GameState -> GameState))] }


renderMenu :: GameState -> Resources -> IO ()
renderMenu st res = do
    withTexture res "bg" $ \tex -> do
       program <- linkShaderProgram =<< getShaders res ["menuVert", "menuFrag"]
       currentProgram $= Just program
       uTex <- get $ uniformLocation program "texture"
       aPos <- get $ attributeLocation program "position"

       activeTexture $= TextureUnit 0
       textureBinding Texture2D $= Just (texObject tex)
       uniform uTex $= Index1 (0::GLint)
       

       putStrLn "Drawing bg..."
 where
     w = toR $ gameWidth
     h = toR $ gameHeight








