module State 
  ( GameStatus(..)
  , GameState(..)
  , initialGameState
  ) where
      

data GameStatus = GameMainMenu
                | GamePausedMenu
                | GameSelectLevel
                | GameRunning
                | GameRestart
                | GameQuit


data GameState = GameState { gameStatus :: GameStatus
                           }


initialGameState :: GameState
initialGameState = GameState 
  { gameStatus = GameMainMenu
  }


