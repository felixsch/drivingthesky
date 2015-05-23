module Road where

import Control.Applicative
import Control.Lens
import Control.Exception

import System.FilePath.Posix

import Linear

import Types
import Resource
import Util
import Block

import Graphics.Rendering.OpenGL
import Graphics.GLUtil

import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.List as L

blocksPerLine :: GLf
blocksPerLine = 7

roadStartX :: GLf
roadStartX = -((blocksPerLine * defaultBlockWidth) / 2)


renderRoad :: Int -> M44 GLf -> Road -> Runtime ()
renderRoad start vp road = do

    shader <- getShader "block"
    io $ do
      bufVertices <- fromSource ArrayBuffer blockVertices
      bufNormals  <- fromSource ArrayBuffer blockNormals
      bufIndicies <- fromSource ElementArrayBuffer blockIndices

      currentProgram $= (Just $ program $ shader)
    
      enableAttrib shader "v_coord"
      enableAttrib shader "v_normal"

      bindBuffer ArrayBuffer $= Just bufVertices
      setAttrib shader "v_coord"
        ToFloat $ VertexArrayDescriptor 3 Float 0 offset0

      bindBuffer ArrayBuffer $= Just bufNormals
      setAttrib shader "n_normal"
        ToFloat $ VertexArrayDescriptor 3 Float 0 offset0

      bindBuffer ElementArrayBuffer $= Just bufIndicies


    mapM_ (renderBlock count vp shader) $ F.foldl (++) [] subset
  where
    subset = S.drop start (road ^. blocks)
    count  = fromIntegral $ length blockIndices

mkRoad :: RoadDefinition -> Maybe Road
mkRoad def = do
    blockRows <- buildBlockRows def (S.viewl (roadDef def)) 0
    return $ Road blockRows def
  where
    boundCheckedIndex s i
      | S.length s >= i = Just (S.index s i)
      | otherwise       = Nothing

buildBlockRows :: RoadDefinition -> S.ViewL [Int] -> Int -> Maybe (S.Seq [Object Block])
buildBlockRows def (S.EmptyL)   _  = Just $ empty
buildBlockRows def (x S.:< xs) i  = (S.<|) <$> buildRow def x 0 i <*> buildBlockRows def (S.viewl xs) (i+1)
  where
    buildRow def []     _ _ = Just []
    buildRow def (x:xs) j i = (++) <$> buildRow def xs (j+1) i <*> (buildBlockObject def i j x) 

buildBlockObject :: RoadDefinition -> Int -> Int -> Int -> Maybe [Object Block]
buildBlockObject def i j typ = Just $ maybe [] (\x -> [Object pos nullVector x]) $ getBlockType def typ
  where
    pos = Vector3 x 0.0 z
    x   = roadStartX + (fromIntegral j * defaultBlockWidth)
    z   = - (fromIntegral i * defaultBlockHeight)

getBlockType :: RoadDefinition -> Int -> Maybe Block
getBlockType def i = filterEmpty =<< M.lookup i (roadBlocks def)
  where
      filterEmpty EmptyBlock   = Nothing
      filterEmpty x            = Just x

{-
renderRoad :: M44 GLf -> Int -> Road -> Runtime ()
renderRoad mvp i road = mapM_ render' $ F.foldl (++) [] subset
  where
    subset = S.drop i (road ^. blocks)
    render' obj = render mvp obj >> renderAABB "#fafaff" (aabb obj)
-}   

intersectingBlocks :: AABB -> S.Seq [Object Block] -> [Object Block]
intersectingBlocks ship = filter (boxIntersect ship . aabb ) . F.foldr (++) []


loadRoad :: String -> Runtime ()
loadRoad name = do
  path <- use runtimePath
  def <- liftIO (try $ readFile (path </> "roads" </> name) :: IO (Either IOError String))
  case def of
    Left _ -> fatal ("Could not load road (road =" ++ path </> "roads" </> name ++ ")")
    Right d -> currentRoad .= (mkRoad $ read d)
{-

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
-} 


testRoadDefinition :: RoadDefinition
testRoadDefinition = RoadDefinition
  { roadName = "Test Level"
  , roadBlocks = M.fromList 
    [ (0, EmptyBlock)
    , (1, Block "#2288FF" 0.2 BlockRenderNormal)
    , (2, Block "#11447F" 0.2 BlockRenderNormal)
    , (3, Block "#ff0000" 0.5 BlockRenderNormal)
    , (4, Start "#ffff00" 0.2 BlockRenderNormal)
    , (5, Goal  "#ff0000" 0.2 BlockRenderNormal) ]
  , roadDef = S.fromList
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
