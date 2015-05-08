{-# LANGUAGE BangPatterns, TemplateHaskell, Arrows #-}

module Input where

import Control.Applicative
import Control.Wire.Core
import Control.Lens

import Data.Monoid

import Graphics.UI.GLFW


import DTS


isKeyDown :: (Monoid e) => Key -> Wire s e DTS a a
isKeyDown key = mkGen_ $ \a -> do
  window <- glfwWindow <$> get
  state  <- liftIO $ getKey window key
  return $ case state of
             KeyState'Released -> Left mempty
             KeyState'Pressed  -> Right a


{-
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
-}

