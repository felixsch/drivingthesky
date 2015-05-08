{-# LANGUAGE TemplateHaskell, Arrows, BangPatterns #-}
module Game 
  ( drivingthesky
  ) where

import Control.Lens

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Data.Maybe

import Util
import Block
import Road
import Globals
import State
import Player
import Input
import Entity

import qualified Data.Sequence as S
import qualified Data.Foldable as F


drivingthesky :: SF (Event GLFW) Game
drivingthesky = collectInput >>> playGame


-- Running a game until the player is dead. If the player is dead
-- restart the game
game :: SF Input Game
game = switch (runGame >>> (arr id &&& isPlayerDead))
              (\_ -> restartGame)

-- Generate a Event if the player is marked as dead
isPlayerDead :: SF Game (Event ())
isPlayerDead = arr (\game -> game ^. state ^?! player ^. isDead) >>> edge


-- set gameOver state and after 2 seconds generate event () to restart the game
restartGame :: SF Input Game
restartGame = switch (gameOver &&& after 2 ())
                     (\_ -> game)


runGame :: SF Input Game
runGame = 



playGame :: SF Input Game
playGame = proc i -> do

  ship'  <- update -< i

  speed <- integral <<^ calcSpeed -< i
  z     <- accumHoldBy (-) 0.0 -< Event speed

     
  ship'' <- collide' withBlock -< (ship', collidingBlocks ship')


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



collide' :: (Entity b) => (Object b -> Object Ship -> Object Ship)
         -> SF (Object Ship, [Object b]) (Object Ship)
collide' f = arr $ \(ship, objs) -> foldr f ship objs
 

withBlock :: Object Block -> Object Ship -> Object Ship
withBlock block ship = check ship (aabb block) block
  where
      check (Object sp sv ship) (AABB bmin bmax) (Object _ _ (Block _ _))
        | sv ^. _y < 0 && sp ^. _y > bmax ^. _y = Object (sp & _y .~ bmax ^. _y) sv ship
        | otherwise = Object sp sv ship
      check (Object sp sv ship) (AABB bmin bmax) (Object _ _ (Start _ _))
        | sv ^. _y < 0 && sp ^. _y > bmax ^. _y = Object (sp & _y .~ (bmax ^. _y + 0.5)) sv ship
        | otherwise = Object sp sv ship
      check ship _ _ = ship



