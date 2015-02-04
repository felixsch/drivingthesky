{-# LANGUAGE TemplateHaskell #-}

module State where

import Control.Lens

import Ship
import Util
import Road
import Input
import Entity
    
data Game = Game { _input  :: Input 
                 , _status :: GameStatus
                 , _state  :: GameState }

data GameStatus = Running
                | Pause
                | MainMenu
                | SelectLevel
                | Quit
                deriving (Show, Eq)

data GameState = Playing     { _ship   :: Object Ship
                             , _road   :: !Road
                             , _speed  :: !GLf 
                             , _currentBlock :: Maybe (AABB, Block) }
               | Paused      {}
               | Menu        {}
               | LevelSelect {}


makeLenses ''GameState
makeLenses ''Game
