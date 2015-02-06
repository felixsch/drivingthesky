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

data Status a  = Alive (Object a)
               | Dead
    deriving (Show)

alive :: Object a -> Status a
alive = Alive

dead :: Object a -> Status a
dead _ = Dead

whenAlive :: (Monad m) => Status a -> (Object a -> m ()) -> m ()
whenAlive (Alive obj) f = f obj
whenAlive Dead _        = return ()


class Entity a where
    update     :: SF Input (Object a)
    canCollide :: a -> Bool
    aabb       :: Object a -> AABB
    collide    :: (Entity b) => SF (Object b, Object a) (Status b, Status a)
    collide = proc (b,a) -> do
        returnA -< (alive b, alive a)

class Renderable a where
    render :: Object a -> IO ()

