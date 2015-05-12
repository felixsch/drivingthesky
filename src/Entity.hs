{-# LANGUAGE TemplateHaskell, Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}

module Entity where

import Control.Lens

import Control.Wire.Core
import FRP.Netwire

import Data.Monoid

import Runtime
import Util


class Entity a where
    wire :: (Monoid e) => Wire s e Runtime () (Object a)
    aabb :: Object a -> AABB
    canCollide :: Wire s e Runtime (Object a) Bool
    canCollide = mkConst (Right False)
    collide :: Wire s e Runtime (Object b, Object a) (Object a)

class Renderable a where
    render :: Object a -> Runtime ()

data Object a = (Entity a) => Object { _position :: Vector3 GLf
                       , _velocity :: Vector3 GLf
                       , _object   :: a }

makeLenses ''Object




    


{-

class Entity a where
    run     :: (Monoid e, HasTime t s) => Wire s e Runtime () (Object a)

    canCollide :: a -> Bool
    canCollide _ = False

    aabb       :: Object a -> AABB

    collide    :: (Monoid e, HasTime t s, Entity o) => Wire s e Runtime (Object o, Object a) (Object a)
    collide    = mkSF_ $ \(_, a) -> a


class Renderable a where
    render :: Object a -> IO () -}

