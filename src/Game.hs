{-# LANGUAGE TemplateHaskell, Arrows, BangPatterns #-}
module Game 
  ( drivingthesky
  ) where

import Control.Lens

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import FRP.Yampa
import FRP.Yampa.GLFW
import Data.Maybe

import Util
import Block
import Road
import Globals
import State
import Ship
import Input
import Entity

import qualified Data.Sequence as S
import qualified Data.Foldable as F


drivingthesky :: SF (Event GLFW) Game
drivingthesky = collectInput >>> playGame




playGame :: SF Input Game
playGame = proc i -> do

  ship'  <- update -< i

  speed <- integral <<^ calcSpeed -< i
  z     <- accumHoldBy (-) 0.0 -< Event speed

  ship'' <- runCollision (collidingBlocks ship') withBlock -< ship

  returnA -< Game { _input  = i
                  , _status = Running
                  , _state  = Playing { _ship  = ship''
                                      , _road  = testRoad
                                      , _speed = (toGLf speed) }}
  where
      testRoad    = fromJust $ generateRoad testRoadDefinition
      speed' i    = (i ^. ioUp - i ^. ioDown) * accel
      calcSpeed i = if speed' i > 0 then speed' i else 0
      collidingBlocks ship = intersectingBlocks (aabb ship) (testRoad ^. blocks)


    

runCollision :: (SF (Object b, Object Ship) (Object Ship)) -> SF ([Object b], Object Ship) (Object Ship)





runCollision []     f = arr id
runCollision (x:xs) f = proc ship -> do
    new <- f -< (x, ship)
    returnA -< new
  


withBlock :: SF (Object Block, Object Ship) (Object Ship)
withBlock  = arr $ \(block, ship) -> check ship (aabb block) block
  where
      check (Object sp sv ship) (AABB bmin bmax) (Object _ _ (Block _ _))
        | sv ^. _y < 0 && sp ^. _y > bmax ^. _y = Object (sp & _y .~ bmax ^. _y) sv ship
        | otherwise = Object sp sv ship



