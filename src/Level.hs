
module Level 
  ( HColor
  , Height
  , Define(..)
  , LevelSettings(..)
  , Level(..)
  , loadLevel 
  ) where

import Control.Applicative

import qualified Data.Map as M
import qualified Data.Sequence as S


type HColor = String
type Height = Double

data Define = EmptyBlock
            | Block HColor Height
            | Start HColor Height
            | Goal HColor Height
            deriving (Show, Read)



data LevelSettings = DefaultGravity
                   | DefaultSpeed
                   | Gravity Double
    deriving (Show, Read)


data Level = Level { lvlName         :: String
                   , lvlBackground   :: Maybe FilePath
                   , lvlMusic        :: Maybe FilePath
                   , lvlDefines      :: M.Map Int Define
                   , lvlSettings :: [((Int,Int), LevelSettings)]
                   , lvlRoad         :: S.Seq [Int] }
                   deriving (Show, Read)

loadLevel :: FilePath -> IO Level
loadLevel path = read <$> readFile path

    
