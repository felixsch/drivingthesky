module Util
  ( whenM
  , R
  , Alpha
  , toR
  , fromR
  , vertex3d
  , vector3d
  , color4d
  , color4d_
  , tC2, v3, c4
  , begin, end
  , gameWidth, gameHeight
  ) where

import Data.Maybe
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Unsafe.Coerce
import Numeric (readHex)
import Data.Char (isHexDigit)


-- Just for testing purpose
gameWidth, gameHeight :: Int
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


vertex3d :: GLdouble -> GLdouble -> GLdouble -> Vertex3 GLdouble
vertex3d = Vertex3

vector3d :: GLdouble -> GLdouble -> GLdouble -> Vector3 GLdouble
vector3d = Vector3

color4d :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> Color4 GLdouble
color4d = Color4


tC2 :: TexCoord2 R -> IO ()
tC2 = texCoord

v3 :: Vertex3 R -> IO ()
v3 = vertex

c4 :: Color4 R -> IO ()
c4 = color


color4d_ :: String -> Color4 GLdouble
color4d_ ['#', r, g, b] = color4d_ ['#', r, r, b, b, g, g]
color4d_ ['#', r1, r2, g1, g2, b1, b2] = color4d_ ['#', r1, r2, g1, g2, b1, b2, 'f' , 'f']
color4d_ s@['#', r1, r2, g1, g2, b1, b2, a1, a2]
  | all isHexDigit (tail s) = Color4 (hexify [r1,r2] / 255)
                                     (hexify [g1, g2] / 255)
                                     (hexify [b1, b2] / 255)
                                     (hexify [a1, a2] / 255)
  | otherwise               = Color4 0.45 1.2 0.2 1.0
  where hexify = fst . head . readHex
color4d_ _              = Color4 0.45 1.2 0.2 1.0

begin :: IO ()
begin = clear [ColorBuffer, DepthBuffer]

end :: Window -> IO ()
end win = swapBuffers win >> flush
