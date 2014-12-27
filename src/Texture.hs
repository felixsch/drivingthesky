module Texture
  ( Tex (..)
  , loadTexture'
  ) where

import Control.Applicative


import Graphics.Rendering.OpenGL

import Graphics.GLUtil.JuicyTextures
import Graphics.GLUtil.Textures

import Util


data Tex = Tex { texW  :: GLsizei
               , texH :: GLsizei
               , texObject :: TextureObject }

loadTexture' :: FilePath -> IO Tex
loadTexture' path = checkIfLoaded =<< readTexInfo path 
  (\tex -> Tex (texWidth tex) (texHeight tex) <$> loadTexture tex)
  where
      checkIfLoaded (Left  err) = error $ "Could not load texture: " ++ err
      checkIfLoaded (Right tex) = return tex
