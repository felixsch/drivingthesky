module Input 
  ( Input(..)
  , noInput
  , input
  ) where

import Graphics.UI.SDL


data Input = Input { jump  :: Bool
                   , left  :: Bool
                   , right :: Bool
                   , menu  :: Bool }

noInput :: Input 
noInput = Input False False False False

input :: IO Input
input = loop noInput
  where
    loop i = do
        ev <- pollEvent
        if isEvent ev
          then loop $ handleEv i ev
          else return i

isEvent :: Event -> Bool
isEvent NoEvent = False
isEvent _       = True


handleEv :: Input -> Event -> Input
handleEv i (KeyDown (Keysym { symKey = SDLK_SPACE }))  = i { jump = True }
handleEv i (KeyDown (Keysym { symKey = SDLK_RIGHT }))  = i { right = True }
handleEv i (KeyDown (Keysym { symKey = SDLK_a }))      = i { right = True }
handleEv i (KeyDown (Keysym { symKey = SDLK_LEFT }))   = i { right = True }
handleEv i (KeyDown (Keysym { symKey = SDLK_d }))      = i { right = True }
handleEv i (KeyDown (Keysym { symKey = SDLK_ESCAPE })) = i { menu = not $ menu i }
handleEv i (LostFocus _)                               = i { menu = True }
handleEv i _                                           = i
