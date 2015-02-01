module Ship where


import FRP.Yampa
import Entity
import Util

import Graphics.Rendering.OpenGL




data Ship = Ship { _shipVelo :: Vector3 GLf
                 , _shipPos  :: Vector3 GLf
                 }

instance Entity Ship where
    renderEntity = renderShip
    updateEntity = updateShip
    entityGetAABB = shipGetAABB

instance Moveable Ship where
    move = moveShip


renderShip :: Ship -> Game -> IO ()
renderShip s _ = do
  preservingMatrix $ do
    scale shipSize shipSize shipSize
    translate s ^. shipPos
    renderPrimitive Triangles $ do

        color $ color4f_ "#383b53"
        vert3 0.0   0.0  (-z)
        vert3 (-x)  0.0  0.0
        vert3 0.0   y    (-iz)

        color $ color4f_ "#66717e"
        vert3 0.0   0.0  (-z)
        vert3 x     0.0  0.0
        vert3 0.0   y    (-iz)

  
        color $ color4f_ "#66717e"
        vert3 0.0   0.0  (-z)
        vert3 (-x)  0.0  0.0
        vert3 0.0   (-y) (-iz)

        color $ color4f_ "#383b53"
        vert3 0.0   0.0  (-z)
        vert3 x     0.0  0.0
        vert3 0.0   (-y) (-iz)

        color $ color4f_ "#32213a"
        vert3 0.0   (-y) (-iz)   
        vert3 0.0   y    (-iz)
        vert3 (-x)  0.0  0.0

        vert3 0.0   (-y) (-iz)   
        vert3 0.0   y    (-iz)
        vert3 x     0.0  0.0
  where

    iz = 0.2 -- inner space
    x = 1.0
    z = 4.0
    y = 0.2

updateShip :: SF (Ship, Game) (Ship, Game)
updateShip = undefined

shipGetAABB :: Ship -> AABB
shipGetAABB = undefined

moveShip :: Game -> SF Ship Ship
moveShip = undefined

