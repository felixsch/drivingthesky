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
import Road
import Globals
import State
import Ship
import Input
import Entity


drivingthesky :: SF (Event GLFW) Game
drivingthesky = collectInput >>> playGame


playGame :: SF Input Game
playGame = proc i -> do

  ship  <- update -< i

  speed <- integral <<^ calcSpeed -< i
  z     <- accumHoldBy (-) 0.0 -< Event speed


  returnA -< Game { _input  = i
                  , _status = Running
                  , _state  = Playing { _ship  = ship
                                      , _road  = fromJust $ generateRoad testRoadDefinition
                                      , _speed = (toGLf speed) }}
                                       where
      speed' :: Input -> GLf
      speed' i    = (i ^. ioUp - i ^. ioDown) * accel
      calcSpeed i = if speed' i > 0 then speed' i else 0

