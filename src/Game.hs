{-# LANGUAGE TemplateHaskell, Arrows, BangPatterns #-}
module Game 
  ( GameStatus(..)
  , GameState(..)
  , Game(..)
  , status
  , state
  , input
  , ship, road, speed
  , Input(..)
  , ioUp, ioDown, ioRight, ioLeft, ioJump, ioEsc
  , drivingthesky
  ) where

import Control.Lens

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import FRP.Yampa
import FRP.Yampa.GLFW

import Util
import Road
import Globals

data GameStatus = Running
                | Pause
                | MainMenu
                | SelectLevel
                | Quit
                deriving (Show, Eq)

data GameState = Playing     { _ship   :: Vector3 GLf
                             , _road   :: !Road
                             , _speed  :: !GLf }
               | Paused      {}
               | Menu        {}
               | LevelSelect {}


data Input = Input { _ioUp :: !GLf
                   , _ioDown :: !GLf
                   , _ioLeft :: !GLf
                   , _ioRight :: !GLf
                   , _ioJump :: !GLf
                   , _ioEsc      :: !Bool }
                   deriving (Show)

neutralInput :: Input
neutralInput = Input 0.0 0.0 0.0 0.0 0.0 False

data Game = Game { _input  :: Input 
                 , _status :: GameStatus
                 , _state  :: GameState }

makeLenses ''GameState
makeLenses ''Input
makeLenses ''Game



collectInput :: SF (Event GLFW) Input
collectInput = proc i -> do
  keyDown      <- keyPress         -< i
  up           <- countK Key'Up    -< keyDown
  down         <- countK Key'Down  -< keyDown
  right        <- countK Key'Right -< keyDown
  left         <- countK Key'Left  -< keyDown
  jump         <- countK Key'Space -< keyDown

  returnA -< Input up down right left jump False
  where
      countK k = key k >>^ event 0.0 (\_ -> 1.0) 

drivingthesky :: SF (Event GLFW) Game
drivingthesky = collectInput >>> playGame

playGame :: SF Input Game
playGame = proc i -> do

  speed <- integral <<^ calcSpeed -< i
  movX  <- integral <<^ calcX -< i
  movY  <- integral <<^ calcY -< i
  z     <- accumHoldBy (-) 0.0 -< Event speed

  returnA -< Game { _input  = i
                  , _status = Running
                  , _state  = Playing { _ship  = Vector3 movX
                                                         movY
                                                         z
                                      , _road  = testRoad
                                      , _speed = (toGLf speed) } }
  where
      speed' :: Input -> GLf
      speed' i    = (i ^. ioUp - i ^. ioDown) * accel
      calcSpeed i = if speed' i > 0 then speed' i else 0

      calcX :: Input -> GLf
      calcX i            = (i ^. ioLeft- i ^. ioRight) * movementPerStep
      calcY :: Input -> GLf
      calcY i            = 0.0 
