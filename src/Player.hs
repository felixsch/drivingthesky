{-# LANGUAGE Arrows, TemplateHaskell #-}
module Player where

import Control.Lens

import Prelude hiding ((.))
import DTS
import Entity
import Util
import Input
import Globals
import Block

import Control.Wire.Core
import FRP.Netwire 

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

shipSize :: Vector3 GLf
shipSize = Vector3 0.1 0.05 1.05

playerAccel :: GLf
playerAccel = 50

data Player = Player { _hasWon :: Bool
                     , _isDead :: Bool
                     , _startPosition :: Int }
               deriving (Show)

makeLenses ''Player


instance Renderable Player where
    render = renderShip

instance Entity Player where
    run          = runPlayer
    canCollide _ = True
    collide      = collideWithPlayer
    aabb         = shipGetAABB
    


renderShip :: Object Player -> IO ()
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


runPlayer :: (Monoid e, HasTime t s) => Wire s e DTS () (Object Player)
runPlayer = proc () -> do
   speed <- calcSpeed  -< ()
   x     <- calcPosX   -< ()
   y     <- calcPosY   -< ()
   z     <- integral 0 -< speed
   velo  <- velocity   -< (x,y,z)

   returnA -< Object (Vector3 x y z) velo (Player False False (blockStartLoadingAt z))
 where
   blockStartLoadingAt z = round $ abZero $ ((-1) * z - blockHeight) / blockHeight

 
calcSpeed :: (Monoid e, HasTime t s) => Wire s e DTS () GLf
calcSpeed = integral 0 . delta (playerAccel / 10) Key'Up Key'Down

calcPosX :: (Monoid e, HasTime t s) => Wire s e DTS () GLf
calcPosX = integral 0 . delta playerAccel Key'Left Key'Right

calcPosY :: (Monoid e, HasTime t s) => Wire s e DTS () GLf
calcPosY = pure 0


velocity :: Wire s e DTS (GLf, GLf, GLf) (Vector3 GLf)
velocity = mkSF_ $ \(x,y,z) -> Vector3 x y z

delta :: (Monoid e) => GLf -> Key -> Key ->  Wire s e DTS () GLf
delta accel k1 k2 = pure 0 . isKeyDown k1 . isKeyDown k2
    <|> pure (accel) . isKeyDown k1
    <|> pure (-accel) . isKeyDown k2
    <|> pure 0



collideWithPlayer :: (Monoid e, Entity a) => Wire s e DTS (Object a, Object Player) (Object Player)
collideWithPlayer = mkSF_ makeACollition
  where
    makeACollition (Object _ _ EmptyBlock, p) = p
    makeACollition (_                , p) = p






{-
updateShip :: Wire s e DTS () (Object Player)
update


updateShip :: SF Input (Object Player)
updateShip = proc i -> do
    speed <- integral <<^ speedValue -< i
    posX  <- integral <<^ xValue -< i
    posY  <- accumHoldBy (-) 0.7 -< Event 0.00005
    posZ  <- accumHoldBy (-) 0.0 -< Event speed

    shipObject <- checkBounds -< Object (Vector3 posX posY posZ) velocity (Player False False (start posZ))
    returnA -< shipObject

--    returnA -< Object (Vector3 (realToFrac posX) 0.5 posZ) (Vector3 0.0 0.0 0.0) (Ship 100 100)

  where
    velocity = Vector3 0.0 (-1.0) 0.0
    speedValue :: Input -> GLf
    speedValue i = abZero (i ^. ioUp   - i ^. ioDown) * (accel / 10)
    xValue :: Input -> GLf
    xValue     i = (i ^. ioLeft - i ^. ioRight) * accel

    start z = round $ abZero (((-1) * z - blockHeight) / blockHeight)



checkBounds :: SF (Object Player) (Object Player)
checkBounds = arr $ \o ->
    o & obj .  isDead .~ outOfBounds (o ^. pos)
  where
      outOfBounds (Vector3 x y _)
        | x < (-3.5) || x > 3.5 = True
        | y < (-2.0)            = True
        | otherwise             = False
-}
 




shipGetAABB :: Object Player -> AABB
shipGetAABB (Object minPos@(Vector3 x y z) _ _) = AABB (Vertex3 minX minY minZ)
                                                       (Vertex3 maxX maxY maxZ)
  where
      minX = x - (shipSize ^. _x)
      minY = y - (shipSize ^. _y)
      minZ = z
      maxX = x + (shipSize ^. _x) 
      maxY = y + (shipSize ^. _y)
      maxZ = z - (shipSize ^. _z)
