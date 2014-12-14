module Menu
  ( Menu(..)
  , renderMenu
  ) where

import Control.Monad
import Control.Applicative
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Graphics.GLUtil.ShaderProgram
import Graphics.UI.GLFW

import Foreign.Storable (sizeOf)

import Texture
import Resource
import Util
import {-# SOURCE #-} State


data MenuShaders  = MenuShaders  { menuProgram  :: Program
                                 , menuTexUni   :: UniformLocation
                                 , posAttr      :: AttribLocation }

data MenuRenderer = MenuRenderer { menuVertBuf  :: BufferObject
                                 , menuElemBuf  :: BufferObject
                                 , menuTextures :: [Tex]
                                 , shaders      :: MenuShaders }

data Menu = Menu { title   :: String
                 ,  entries :: [(String, (GameState -> GameState))] }


vertBufData :: [GLfloat]
vertBufData = [-1,-1,1,-1,-1,1,1,1]


setupMenuShaders :: Resources -> IO MenuShaders
setupMenuShaders res = do
    prg <- linkShaderProgram =<< getShaders res ["menuVert", "menuFrag"]
    MenuShaders prg <$> get (uniformLocation prg "texture")
                    <*> get (attribLocation prg "position")


setupMenuRenderer :: Resources -> IO MenuRenderer
setupMenuRenderer res = MenuRenderer <$> makeBuffer ArrayBuffer vertBufData
                                     <*> makeBuffer ElementArrayBuffer [0..3 :: GLuint]
                                     <*> getTextures res ["bg"]
                                     <*> setupMenuShaders res



renderMenu :: GameState -> Resources -> IO ()
renderMenu st res = do
    (MenuRenderer vertBuf elemBuf [tex] sh) <- setupMenuRenderer res

    clearColor $= Color4 1 1 1 1
    clear [ColorBuffer]
    currentProgram $= Just (menuProgram sh)

    -- texture
    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just (texObject tex)
    uniform (menuTexUni sh) $= Index1 (0 :: GLint)

    -- buffer
    bindBuffer ArrayBuffer $= Just vertBuf
    vertexAttribPointer (posAttr sh) $= (ToFloat, VertexArrayDescriptor 2 Float stride offset0)
    vertexAttribArray   (posAttr sh) $= Enabled
    bindBuffer ElementArrayBuffer $= Just elemBuf

    drawElements TriangleStrip 4 UnsignedInt offset0

  where
      stride = fromIntegral $ sizeOf (undefined :: GLfloat) * 2






