module State 
  ( GameStatus(..)
  , GameState(..)
  , initialGameState
  ) where

import Menu
import Level

data GameStatus = GameMainMenu
                | GamePausedMenu
                | GameSelectLevel
                | GameRunning
                | GameRestart
                | GameQuit


data GameState = GameState { gameStatus :: GameStatus
                           , gamePos    :: Int
                           , gameLevel  :: Maybe Level
                           }


initialGameState :: GameState
initialGameState = GameState 
  { gameStatus = GameMainMenu
  , gamePos    = 0
  , gameLevel  = Nothing
  }


