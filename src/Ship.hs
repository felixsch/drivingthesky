{-# LANGUAGE Arrows, TemplateHaskell #-}
module Ship where

import Control.Lens

import FRP.Yampa
import Entity
import Util
import Input
import Globals
import Block

import Graphics.Rendering.OpenGL


shipSize :: Vector3 GLf
shipSize = Vector3 0.1 0.05 1.05

accel :: GLf
accel = 50

data Ship = Ship { _hasWon :: Bool
                 , _isDead :: Bool
                 , _startPosition :: Int }
            deriving (Show)

makeLenses ''Ship


instance Renderable Ship where
    render = renderShip

instance Entity Ship where
    update       = updateShip
    canCollide _ = True
    aabb         = shipGetAABB


renderShip :: Object Ship -> IO ()
renderShip (Object pos _ _) = do
  preservingMatrix $ do
    translate pos
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
    x = shipSize ^. _x
    z = shipSize ^. _z
    y = shipSize ^. _y

updateShip :: SF Input (Object Ship)
updateShip = proc i -> do
    speed <- integral <<^ speedValue -< i
    posX  <- integral <<^ xValue -< i
    posY  <- accumHoldBy (-) 0.7 -< Event 0.00005
    posZ  <- accumHoldBy (-) 0.0 -< Event speed

    shipObject <- checkBounds -< Object (Vector3 posX posY posZ) velocity (Ship False False (start posZ))
    returnA -< shipObject

--    returnA -< Object (Vector3 (realToFrac posX) 0.5 posZ) (Vector3 0.0 0.0 0.0) (Ship 100 100)

  where
    velocity = Vector3 0.0 (-1.0) 0.0
    speedValue :: Input -> GLf
    speedValue i = abZero (i ^. ioUp   - i ^. ioDown) * (accel / 10)
    xValue :: Input -> GLf
    xValue     i = (i ^. ioLeft - i ^. ioRight) * accel

    start z = round $ abZero (((-1) * z - blockHeight) / blockHeight)



checkBounds :: SF (Object Ship) (Object Ship)
checkBounds = arr $ \o ->
    o & obj .  isDead .~ outOfBounds (o ^. pos)
  where
      outOfBounds (Vector3 x y _)
        | x < (-3.5) || x > 3.5 = True
        | y < (-2.0)            = True
        | otherwise             = False

  


shipGetAABB :: Object Ship -> AABB
shipGetAABB (Object minPos@(Vector3 x y z) _ _) = AABB (Vertex3 minX minY minZ)
                                                       (Vertex3 maxX maxY maxZ)
  where
      minX = x - (shipSize ^. _x)
      minY = y - (shipSize ^. _y)
      minZ = z
      maxX = x + (shipSize ^. _x) 
      maxY = y + (shipSize ^. _y)
      maxZ = z - (shipSize ^. _z)
