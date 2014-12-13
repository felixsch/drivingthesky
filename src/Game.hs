module Game 
  ( drivingthesky
  ) where

import FRP.Yampa
import FRP.Yampa.GLFW

import State

drivingthesky :: SF (Event GLFW) GameState
drivingthesky = arr $ const initialGameState
