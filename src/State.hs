{-# LANGUAGE TemplateHaskell #-}

module State where

import Control.Lens

import Player
import Util
import Road
import Input
import Entity


data Game = Playing     { _player   :: Object Player
                        , _road   :: Road
                        , _speed  :: !GLf }
          | Paused      {}
          | Menu        {}
          | LevelSelect {}
          deriving (Show)

makeLenses ''Game
