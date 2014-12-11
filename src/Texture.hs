module Texture
  ( Tex (..)
  , loadTexture
  , freeTexture
  , renderTexture
  ) where

import Control.Monad.IO.Class 

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as SDL

import Graphics.Rendering.OpenGL

import Util


data Tex = Tex { texWidth  :: R
               , texHeight :: R
               , texObject :: TextureObject }


loadTexture :: (MonadIO m) => FilePath -> m Tex
loadTexture path = liftIO $ do
  surface <- SDL.loadTyped path SDL.PNG
  pixels  <- SDL.surfaceGetPixels surface
  
  let w = fromIntegral $ SDL.surfaceGetWidth surface
  let h = fromIntegral $ SDL.surfaceGetHeight surface

  [obj] <- liftIO $ genObjectNames 1

  textureBinding Texture2D $= Just obj
  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

  let pdata = PixelData RGBA UnsignedByte pixels

  texImage2D Texture2D NoProxy 0 RGBA' (TextureSize2D w h) 0 pdata

  SDL.freeSurface surface

  return $ Tex (toR w) (toR h) obj

freeTexture :: (MonadIO m) => Tex -> m ()
freeTexture tex = liftIO $ deleteObjectNames [texObject tex]


renderTexture :: (MonadIO m) => Tex -> (R,R) -> (R,R) -> Alpha -> m ()
renderTexture tex (x,y) (w,h) alpha = liftIO $ do
  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just (texObject tex)

  renderPrimitive Quads $ do
    tC2 (TexCoord2 0 1) >> v3 (Vertex3 x y 0.0) >> color'
    tC2 (TexCoord2 0 0) >> v3 (Vertex3 x (y + h') 0.0) >> color'
    tC2 (TexCoord2 1 0) >> v3 (Vertex3 (x + w') (y + h') 0.0) >> color'
    tC2 (TexCoord2 1 1) >> v3 (Vertex3 (x + w') y 0.0) >> color'
  texture Texture2D $= Disabled

  where
    color' = c4 (Color4 1.0 1.0 1.0 alpha)
    w'     = texWidth tex
    h'     = texHeight tex
