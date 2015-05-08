{-# LANGUAGE TemplateHaskell, Arrows #-}

module Entity where

import Control.Lens

import Control.Wire.Core
import FRP.Netwire

import Data.Monoid
import {-# SOURCE #-} DTS


import Util


data Object a = Object { _pos :: Vector3 GLf
                       , _velo :: Vector3 GLf
                       , _obj   :: a }
    deriving (Show)

makeLenses ''Object



class Entity a where
    run     :: (Monoid e, HasTime t s) => Wire s e DTS () (Object a)

    canCollide :: a -> Bool
    canCollide _ = False

    aabb       :: Object a -> AABB

    collide    :: (Monoid e, HasTime t s, Entity o) => Wire s e DTS (Object o, Object a) (Object a)
    collide    = mkSF_ $ \(_, a) -> a


class Renderable a where
    render :: Object a -> IO ()

