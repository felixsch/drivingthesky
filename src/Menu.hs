module Menu
  ( Menu(..)
  , renderMenu
  ) where

import Control.Monad

import Texture
import Resource
import Input
import Util
import {-# SOURCE #-} State
import Graphics.UI.SDL


data Menu = Menu { title   :: String
                 ,  entries :: [(String, (GameState -> GameState))] }



renderMenu :: GameState -> Resources -> IO ()
renderMenu st mgr = do
    whenM (getR mgr "bg") $ \tex ->
       renderTexture tex (0.0,0.0) (w,h) 1.0

 where
     w = toR $ gameWidth
     h = toR $ gameHeight








