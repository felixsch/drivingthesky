module Util
  ( fatal, warn, info
  , boxIntersect
  , renderAABB
  , toVertex, toVector
  , vertex3f, vector3f, normal3f
  , vert3, norm3, col4
  , color4f_
  , nullVector
  , liftIO, io
  ) where


import Control.Applicative
import Control.Lens
import Control.Monad.Error ( throwError )
import Control.Monad.IO.Class

import Graphics.Rendering.OpenGL

import Unsafe.Coerce
import Numeric (readHex)
import Data.Char (isHexDigit)

import Linear
import Types


-- alias

io :: (MonadIO m) => IO a -> m a
io = liftIO

-- Simple error handling

fatal :: String -> Runtime a
fatal = throwError . Fatal

warn :: String -> Runtime a
warn = throwError . Warning

info :: String -> Runtime a
info = throwError . Info


-- AABB
boxIntersect :: AABB -> AABB -> Bool
boxIntersect (AABB aMin aMax) (AABB bMin bMax) =
    aMax ^. _x > bMin ^. _x &&
    aMin ^. _x < bMax ^. _x &&

    aMax ^. _y > bMin ^. _y &&
    aMin ^. _y < bMax ^. _y &&

    aMax ^. _z < bMin ^. _z &&
    aMin ^. _z > bMax ^. _z

renderAABB :: String -> AABB -> Runtime ()
renderAABB c (AABB p1 p2) = liftIO $ do
    color $ color4f_ c
    renderPrimitive Lines $ do


        vert3 p1x p1y p1z
        vert3 p1x p1y p2z

        vert3 p1x p1y p2z
        vert3 p1x p2y p2z

        vert3 p1x p2y p2z
        vert3 p1x p2y p1z

        vert3 p1x p2y p1z
        vert3 p1x p1y p1z

        vert3 p1x p1y p1z
        vert3 p2x p1y p1z

        vert3 p1x p2y p1z
        vert3 p2x p2y p1z

        vert3 p2x p1y p1z
        vert3 p2x p2y p1z

        vert3 p2x p2y p1z
        vert3 p2x p2y p2z

        vert3 p2x p2y p2z
        vert3 p2x p1y p2z

        vert3 p2x p1y p2z
        vert3 p2x p1y p1z

        vert3 p2x p1y p2z
        vert3 p1x p1y p2z

        vert3 p2x p2y p2z
        vert3 p1x p2y p2z

  where
      p1x = p1 ^. _x
      p2x = p2 ^. _x

      p1y = p1 ^. _y
      p2y = p2 ^. _y

      p1z = p1 ^. _z
      p2z = p2 ^. _z

-- rendering basics

toVertex :: (R3 c) => c a -> Vertex3 a
toVertex x = Vertex3 (x ^. _x) (x ^. _y) (x ^. _z)

toVector :: (R3 c) => c a -> Vector3 a
toVector x = Vector3 (x ^. _x) (x ^. _y) (x ^. _z)

vertex3f :: GLf -> GLf -> GLf -> Vertex3 GLf
vertex3f = Vertex3

vector3f :: GLf -> GLf -> GLf -> Vector3 GLf
vector3f = Vector3

normal3f :: GLf -> GLf -> GLf -> Normal3 GLf
normal3f = Normal3

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


nullVector :: V3 GLf
nullVector = V3 0.0 0.0 0.0

