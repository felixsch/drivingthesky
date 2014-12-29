{-# LANGUAGE TemplateHaskell #-}
module Road
  ( BlockColor
  , BlockHeight
  , BlockAlpha
  , Block(..)
  , Road(..)
  , loadRoad
  , testRoad
  , roadBlocks
  , roadDef
  , roadName
  ) where

import Control.Applicative
import Control.Lens

import qualified Data.Map as M
import qualified Data.Sequence as S

import Util


type BlockColor  = String
type BlockHeight = GLf
type BlockAlpha  = GLf

data Block  = Start BlockColor BlockHeight
            | Block BlockColor BlockHeight
            | EmptyBlock
            | Goal BlockColor BlockHeight
            deriving (Show, Read, Eq)

data Road = Road { _roadName :: String
                 , _roadBlocks :: M.Map Int Block
                 , _roadDef :: S.Seq [Int] }
                 deriving (Show, Read, Eq)

makeLenses ''Road


loadRoad :: FilePath -> IO Road
loadRoad path = read <$> readFile path


testRoad :: Road
testRoad = Road
  { _roadName = "Test Level"
  , _roadBlocks = M.fromList 
    [ (0, EmptyBlock)
    , (1, Block "#2288FF" 0.2)
    , (2, Block "#11447F" 0.2)
    , (3, Block "#ff0000" 0.5)
    , (4, Start "#ffff00" 0.2)
    , (5, Goal  "#ff0000" 0.2) ]
  , _roadDef = S.fromList
    [ [0,0,0,4,0,0,0]
    , [0,0,0,1,0,0,0]
    , [0,0,0,0,0,0,0]
    , [0,0,0,1,0,0,0]
    , [0,1,0,1,0,1,0]
    , [0,1,2,1,2,1,0]
    , [0,0,1,2,1,0,0]
    , [0,0,0,1,0,0,0]
    , [0,0,0,1,0,0,0]
    , [0,0,0,1,0,0,0]
    , [0,0,0,1,1,0,0]
    , [0,3,0,0,1,1,0]
    , [0,3,0,0,0,1,0]
    , [0,3,0,0,0,1,0]
    , [0,3,0,0,0,0,0]
    , [0,0,0,1,0,0,0]
    , [0,0,0,1,0,0,0]
    , [0,0,0,1,0,0,0]
    , [0,0,0,5,0,0,0]] }
