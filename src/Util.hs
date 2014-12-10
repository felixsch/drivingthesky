module Util
  ( whenM
  , R
  , Alpha
  , toR
  , fromR
  , tC2, v3, c4
  , begin, end
  , gameWidth, gameHeight
  ) where

import Data.Maybe
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL.Video
import Unsafe.Coerce


-- Just for testing purpose
gameWidth = 1366
gameHeight = 786

whenM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenM s f = maybe (return ()) f =<< s


type R = GLdouble
type Alpha = R

toR :: a -> R
toR = unsafeCoerce

fromR :: R -> a
fromR = unsafeCoerce


tC2 :: TexCoord2 R -> IO ()
tC2 = texCoord

v3 :: Vertex3 R -> IO ()
v3 = vertex

c4 :: Color4 R -> IO ()
c4 = color

begin :: IO ()
begin = clear [ColorBuffer, DepthBuffer]

end :: IO ()
end = glSwapBuffers >> flush
