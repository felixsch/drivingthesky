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
                           , gameHeight :: Int
                           , gameWidth  :: Int
                           }


initialGameState :: GameState
initialGameState = GameState 
  { gameStatus = GameMainMenu
  , gameWidth = 1366
  , gameHeight = 786
  }


