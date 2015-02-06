{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Util
  ( whenM
  , GLf
  , Alpha
  , toGLf
  , fromGLf
  , vertex3f
  , vector3f
  , normal3f
  , Vertex3(..)
  , Vector3(..)
  , Normal3(..)
  , Color4(..)
  , color4f
  , color4f_
  , vert3
  , norm3
  , col4

  , AABB(..)
  , boxIntersect 
  
  , Component3(..)
  , toVertex

  , _x
  , _y
  , _z

  , abZero
  , vNull

  ) where

import Control.Applicative
import Control.Lens

import Graphics.Rendering.OpenGL
import Unsafe.Coerce
import Numeric (readHex)
import Data.Char (isHexDigit)

import FRP.Yampa


data AABB = AABB { pMin :: Vertex3 GLf
                 , pMax :: Vertex3 GLf }
        deriving (Show)

boxIntersect :: AABB -> AABB -> Bool
boxIntersect (AABB aMin aMax) (AABB bMin bMax) =
    aMax ^. _x > bMin ^. _x &&
    aMin ^. _x < bMax ^. _x &&

    aMax ^. _y > bMin ^. _y &&
    aMin ^. _y < bMax ^. _y &&

    aMax ^. _z > bMin ^. _z &&
    aMin ^. _z < bMax ^. _z


class Component3 c where
  viewC1 :: c a -> a
  viewC2 :: c a -> a
  viewC3 :: c a -> a
  setC1 :: c a -> a -> c a
  setC2 :: c a -> a -> c a
  setC3 :: c a -> a -> c a


instance Component3 Vertex3 where
  viewC1 (Vertex3 c1 _ _) = c1
  viewC2 (Vertex3 _ c2 _) = c2
  viewC3 (Vertex3 _ _ c3) = c3

  setC1 (Vertex3 _ c2 c3) c1 = Vertex3 c1 c2 c3
  setC2 (Vertex3 c1 _ c3) c2 = Vertex3 c1 c2 c3
  setC3 (Vertex3 c1 c2 _) c3 = Vertex3 c1 c2 c3

instance Component3 Vector3 where
  viewC1 (Vector3 c1 _ _) = c1
  viewC2 (Vector3 _ c2 _) = c2
  viewC3 (Vector3 _ _ c3) = c3

  setC1 (Vector3 _ c2 c3) c1 = Vector3 c1 c2 c3
  setC2 (Vector3 c1 _ c3) c2 = Vector3 c1 c2 c3
  setC3 (Vector3 c1 c2 _) c3 = Vector3 c1 c2 c3

instance Component3 Normal3 where
  viewC1 (Normal3 c1 _ _) = c1
  viewC2 (Normal3 _ c2 _) = c2
  viewC3 (Normal3 _ _ c3) = c3

  setC1 (Normal3 _ c2 c3) c1 = Normal3 c1 c2 c3
  setC2 (Normal3 c1 _ c3) c2 = Normal3 c1 c2 c3
  setC3 (Normal3 c1 c2 _) c3 = Normal3 c1 c2 c3

_x :: (Component3 c) => Lens (c a) (c a) a a
_x inj comp = setC1 comp <$> inj (viewC1 comp)

_y :: (Component3 c) => Lens (c a) (c a) a a
_y inj comp = setC2 comp <$> inj (viewC2 comp)

_z :: (Component3 c) => Lens (c a) (c a) a a
_z inj comp = setC3 comp <$> inj (viewC3 comp)

whenM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenM s f = maybe (return ()) f =<< s


abZero :: GLf -> GLf
abZero x
  | x > 0.0   = x
  | otherwise = 0.0

vNull :: Vector3 GLf
vNull = Vector3 0.0 0.0 0.0


type GLf = GLfloat
type Alpha = GLf

toGLf :: a -> GLf
toGLf = unsafeCoerce

fromGLf :: GLf -> a
fromGLf = unsafeCoerce


instance VectorSpace GLf GLf where
  zeroVector     = 0
  negateVector x = -x

  a *^ x         = a * x
  x ^/ a         = x / a

  x1 ^+^ x2      = x1 + x2
  x1 ^-^ x2      = x1 - x2
  x1 `dot` x2    = x1 * x2


toVertex :: (Component3 c) => c a -> Vertex3 a
toVertex x = Vertex3 (x ^. _x) (x ^. _y) (x ^. _z)

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

