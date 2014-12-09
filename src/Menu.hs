module Menu
  ( Menu(..)
  ) where

import Control.Monad

import Texture
import Resource
import Input
import State
import Util

import Graphics.UI.SDL


data Menu = Menu { title   :: String
                 ,  entries :: [(String, (GameState -> GameState))]
                 ,  pos     :: Int }


renderMenu :: ResourceMgr -> GameState -> IO ()
renderMenu mgr st = do
    scr <- getVideoSurface
    
    whenM (getR "bg" mgr) $ \bg -> do
      let bgRect = Rect 0 0 (gameWidth st) (gameHeight st)
      r <- blitSurface bg Nothing scr $ Just bgRect
      when (not r) $ putStrLn "Could not draw menu background"








