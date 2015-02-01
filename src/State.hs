{-# LANGUAGE TemplateHaskell #-}

module State where

import Control.Lens

import Util
import Road
    
data Game = Game { _input  :: Input 
                 , _status :: GameStatus
                 , _state  :: GameState }

data GameStatus = Running
                | Pause
                | MainMenu
                | SelectLevel
                | Quit
                deriving (Show, Eq)

data GameState = Playing     { _ship   :: Vector3 GLf
                             , _road   :: !Road
                             , _speed  :: !GLf 
                             , _currentBlock :: Maybe (AABB, Block) }
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



makeLenses ''GameState
makeLenses ''Input
makeLenses ''Game
