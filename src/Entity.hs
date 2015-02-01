module Entity where


import FRP.Yampa

import Util
import State

class Entity a where
    renderEntity  :: a -> Game -> m ()
    updateEntity  :: SF (a, Game) (a, Game)
    entityGetAABB :: a -> AABB


class Moveable a where
    move :: Game -> SF a a
    


