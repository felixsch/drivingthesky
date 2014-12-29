{-# LANGUAGE TemplateHaskell #-}
module Game 
  ( GameStatus(..)
  , GameState(..)
  , Game(..)
  , status
  , state
  , ship, road, speed
  , drivingthesky
  , initGame
  ) where

import Control.Lens

import Graphics.Rendering.OpenGL
import FRP.Yampa
import FRP.Yampa.GLFW

import Util
import Road

data GameStatus = Running
                | Pause
                | MainMenu
                | SelectLevel
                | Quit
                deriving (Show, Eq)

data GameState = Playing     { _ship   :: Vector3 GLf
                             , _road   :: Road
                             , _speed  :: GLf }
               | Paused      {}
               | Menu        {}
               | LevelSelect {}


data Game = Game { _status :: GameStatus
                 , _state  :: GameState }

makeLenses ''GameState
makeLenses ''Game

initGame :: Game
initGame = Game Running (Playing (vector3f 0.0 0.0 0.0) testRoad 0.0)
  
                          

drivingthesky :: SF (Event GLFW) Game
drivingthesky = arr $ const initGame
