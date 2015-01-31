{-# LANGUAGE TemplateHaskell #-}
module Road
  ( BlockColor
  , BlockHeight
  , BlockAlpha
  , Block(..)
  , Road(..)
  , loadRoad
  , getBlock
  , getBlockAt
  , testRoad
  , roadBlocks
  , roadDef
  , roadName
  ) where

import Control.Applicative
import Control.Lens

import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.List as L


import Graphics.Rendering.OpenGL
import Util
import Globals


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


getBlockY :: Block -> GLf
getBlockY (Start _ h) = h
getBlockY (Block _ h) = h
getBlockY (Goal _ h)  = h

getBlock :: Int -> Road -> Block
getBlock b road = case getBlockType road b of
   Just x  -> x
   Nothing -> error $ "Failed to get block definition for blocktype: " ++ show b

getBlockType :: Road -> Int -> Maybe Block
getBlockType road i = M.lookup i (road ^. roadBlocks)


getBlockAt :: Road -> Vector3 GLf -> Maybe (AABB, Block)
getBlockAt road (Vector3 x y z) = do
    block <- getBlockType road =<< getBlockNumber road posX posZ
    aabb  <- getAABBFromBlock (posX, posZ) block
    return (aabb, block)  
 where
    posZ = truncate $ abs $ z / blockHeight
    posX = 3 + (truncate $ x / blockWidth)
    
getBlockNumber :: Road -> Int -> Int -> Maybe Int
getBlockNumber road x z = findColumn x =<< findRow z (S.viewl $ road ^. roadDef)
  where
      findRow n (S.EmptyL)  = Nothing
      findRow 0 (x S.:< xs) = Just x
      findRow n (x S.:< xs) = findRow (n - 1) (S.viewl xs)

      findColumn n []     = Nothing
      findColumn 0 (x:xs) = Just x
      findColumn n (x:xs) = findColumn (n-1) xs

  

getAABBFromBlock :: (Int,Int) -> Block -> Maybe AABB
getAABBFromBlock _     EmptyBlock = Nothing
getAABBFromBlock (x,z) block      = Just $ AABB minP maxP
 where
     minP = vertex3f posX 0.0 (-posZ)
     maxP = vertex3f (posX + blockHeight) (getBlockY block) (-(posZ + blockWidth))
     posX = renderStartPos + ((toGLf x) * blockHeight)
     posZ = (toGLf z) * blockWidth
 


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
    [ [1,0,0,4,0,0,1]
    , [0,0,0,1,0,0,0]
    , [0,0,0,0,0,0,0]
    , [0,0,0,1,0,0,0]
    , [0,1,0,1,0,1,0]
    , [1,1,2,1,2,1,1]
    , [1,0,1,2,1,0,1]
    , [1,0,0,1,0,0,1]
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
