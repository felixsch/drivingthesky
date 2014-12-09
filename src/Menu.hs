module Menu
  (
  ) where

import Resource
import Input


data Menu = Menu { entries :: (String, (GameState -> IO GameState))
                 }



