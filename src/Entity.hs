{-# LANGUAGE TemplateHaskell, Arrows #-}

module Entity where

import Control.Lens
import FRP.Yampa

import Util
import Input



data Object a = Object { _pos :: Vector3 GLf
                       , _velo :: Vector3 GLf
                       , _obj   :: a }
    deriving (Show)

makeLenses ''Object



class Entity a where
    update     :: SF Input (Object a)
    canCollide :: a -> Bool
    aabb       :: Object a -> AABB
    collide    :: (Entity b) => Object b -> Object a -> Object a
    collide _ a = a

class Renderable a where
    render :: Object a -> IO ()

