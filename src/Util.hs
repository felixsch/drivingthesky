module Util
  ( whenM
  , GLf
  , Alpha
  , toGLf
  , fromGLf
  , vertex3f
  , vector3f
  , color4f
  , color4f_
  , vert3
  , norm3
  , col4

  ) where

import Graphics.Rendering.OpenGL
import Unsafe.Coerce
import Numeric (readHex)
import Data.Char (isHexDigit)



whenM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenM s f = maybe (return ()) f =<< s


type GLf = GLfloat
type Alpha = GLf

toGLf :: a -> GLf
toGLf = unsafeCoerce

fromGLf :: GLf -> a
fromGLf = unsafeCoerce


vertex3f :: GLf -> GLf -> GLf -> Vertex3 GLf
vertex3f = Vertex3

vector3f :: GLf -> GLf -> GLf -> Vector3 GLf
vector3f = Vector3

color4f :: GLf -> GLf -> GLf -> GLf -> Color4 GLf
color4f = Color4


vert3 :: GLf -> GLf -> GLf -> IO ()
vert3 x y z = vertex $ Vertex3 x y z

norm3 :: GLf -> GLf -> GLf -> IO ()
norm3 x y z = normal $ Normal3 x y z

col4 :: GLf -> GLf -> GLf -> GLf -> IO ()
col4 r g b a = color $ Color4 r g b a

color4f_ :: String -> Color4 GLfloat
color4f_ ['#', r, g, b] = color4f_ ['#', r, r, b, b, g, g]
color4f_ ['#', r1, r2, g1, g2, b1, b2] = color4f_ ['#', r1, r2, g1, g2, b1, b2, 'f' , 'f']
color4f_ s@['#', r1, r2, g1, g2, b1, b2, a1, a2]
  | all isHexDigit (tail s) = Color4 (hexify [r1,r2] / 255)
                                     (hexify [g1, g2] / 255)
                                     (hexify [b1, b2] / 255)
                                     (hexify [a1, a2] / 255)
  | otherwise               = Color4 0.45 1.2 0.2 1.0
  where hexify = fst . head . readHex
color4f_ _              = Color4 0.45 1.2 0.2 1.0
