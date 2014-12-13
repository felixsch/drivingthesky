module Texture
  ( Tex (..)
  , loadTexture'
  , renderTexture
  ) where

import Control.Monad.IO.Class 
import Control.Applicative


import Graphics.Rendering.OpenGL

import Graphics.Rendering.OpenGL hiding (Level)

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

{-
loadPngTexture :: (MonadIO m) => FilePath -> m Tex
loadPngTexture path = liftIO $ do
 
  (ImageRGBA8 (Image w h dat)) <- loadPng path

  [obj] <- liftIO $ genObjectNames 1

  textureBinding Texture2D $= Just obj
  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
 
  unsafeWith dat $ \ptr ->
    texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D (fromIntegral w) (fromIntegral h)) 0 (PixelData RGBA UnsignedByte ptr)


  return $ Tex (toR w) (toR h) obj

freeTexture :: (MonadIO m) => Tex -> m ()
freeTexture tex = liftIO $ deleteObjectNames [texObject tex]
-}

renderTexture :: (MonadIO m) => Tex -> (R,R) -> (R,R) -> Alpha -> m ()
renderTexture tex (x,y) (w,h) alpha = liftIO $ do
  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just (texObject tex)

  renderPrimitive Quads $ do
    color'
    tC2 (TexCoord2 0 1) >> v3 (Vertex3 x y 0.0) >> color'
    tC2 (TexCoord2 0 0) >> v3 (Vertex3 x (y + h') 0.0) >> color'
    tC2 (TexCoord2 1 0) >> v3 (Vertex3 (x + w') (y + h') 0.0) >> color'
    tC2 (TexCoord2 1 1) >> v3 (Vertex3 (x + w') y 0.0) >> color'
  texture Texture2D $= Disabled

  where
    color' = c4 (Color4 1.0 1.0 1.0 alpha)
    w'     = toR $ texW tex
    h'     = toR $ texH tex
