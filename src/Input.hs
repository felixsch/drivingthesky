{-# LANGUAGE BangPatterns, TemplateHaskell, Arrows #-}

module Input where

import Control.Lens
import FRP.Yampa
import FRP.Yampa.GLFW

import Graphics.UI.GLFW

import Util

data Input = Input { _ioUp :: !GLf
                   , _ioDown :: !GLf
                   , _ioLeft :: !GLf
                   , _ioRight :: !GLf
                   , _ioJump :: !GLf
                   , _ioEsc      :: !Bool }
                   deriving (Show)

makeLenses ''Input

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

