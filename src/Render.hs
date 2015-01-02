module Render
  ( render
  , roadShunk
  ) where

import Prelude hiding (foldr)
import Control.Lens

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL

import qualified Data.Sequence as S
import qualified Data.Map as M
import Data.Foldable

import Util
import Game
import Road
import Resource
import Globals




render :: GameStatus -> GLFW.Window -> Game -> Resources -> IO Bool
render status
  | status == Running    = renderGame
  | status == Pause      = renderPause
  | status == MainMenu   = renderMainMenu


roadShunk :: Int -> GameState -> S.Seq [Block]
roadShunk start state = foldr buildRoad S.empty $
    S.drop start (road' ^. roadDef)
  where
     road' = state ^?! road
     buildRoad row s = foldr buildRow [] row S.<| s
     buildRow block row = row ++ [getBlock block (road' ^. roadBlocks)]

getBlock :: Int -> M.Map Int Block -> Block
getBlock b def = case M.lookup b def of
   Just x  -> x
   Nothing -> error $ "Failed to get block definition for blocktype: " ++ show b


renderLevel :: Int -> S.ViewL [Block] -> IO ()
renderLevel _     (S.EmptyL)      = return ()
renderLevel depth (row S.:< road) = renderRow row renderStartPos z
                                  >> renderLevel (depth + 1) (S.viewl road)
  where
      z = -(fromIntegral depth) * blockHeight

renderRow :: [Block] -> GLf -> GLf -> IO ()
renderRow (i:is) x z = do
  preservingMatrix $ do
    translate $ vector3f x 0.0 z
    renderBlock i
  renderRow is (x + blockWidth) z
renderRow []     _ _ = return ()

renderBlock :: Block -> IO ()
renderBlock (EmptyBlock) = return ()
renderBlock (Block c h)  = drawNormalBlock c h
renderBlock b            = drawNormalBlock "#ffff00" 0.2
-- renderBlock b            = (putStrLn $ "Not implemented block: " ++ show b)
--                           >> drawNormalBlock "#ffff00" 0.2


drawNormalBlock :: String -> GLf -> IO ()
drawNormalBlock c h = do
    color $ color4f_ c
    renderPrimitive Quads $ do 

      norm3 0.0    1.0  0.0
      vert3 0.0    h     0.0
      vert3 blockWidth      h    0.0
      vert3 blockWidth      h    (-blockHeight)
      vert3 0.0    h     (-blockHeight)

      color $ darken 0.2 $ color4f_ c

      norm3 0.0    0.0   (-1.0)
      vert3 0.0    h     0.0
      vert3 blockWidth      h     0.0
      vert3 blockWidth      d     0.0
      vert3 0.0    d     0.0

      norm3 (-1.0) 0.0   0.0
      vert3 0.0    d     0.0
      vert3 0.0    d     (-blockHeight)
      vert3 0.0    h     (-blockHeight)
      vert3 0.0    h     0.0

      norm3 1.0    0.0   0.0
      vert3 blockWidth      d     0.0
      vert3 blockWidth      h     0.0
      vert3 blockWidth      h     (-blockHeight)
      vert3 blockWidth      d     (-blockHeight)
  where
      d = 0.0
      darken f (Color4 r g b a) = Color4 (r-f) (g-f) (b-f) a

renderGame :: GLFW.Window -> Game -> Resources -> IO Bool
renderGame win game res = do

    clear [ColorBuffer, DepthBuffer]
    depthFunc $= Just Less
    loadIdentity

    putStrLn $ "input = " ++ show (game ^. input)
    putStrLn $ "ship  = " ++ show (state' ^?! ship)
    lookAt eye (Vertex3 0.0 4.0 (-200.0)) (Vector3 0.0 1.0 0.0)
    renderLevel start $ S.viewl $ roadShunk start (game ^. state) 
   
    get errors >>= Prelude.mapM_ (\e -> putStrLn $ "GL Error: " ++ show e)

    GLFW.swapBuffers win
    flush
    GLFW.pollEvents

    return False
  where
      eye :: Vertex3 GLdouble
      eye    = Vertex3 0.0 0.8 $ fromGLf (state' ^?! ship ^. _z + 1.0)
      state' = game ^. state
      length_ = S.length $ state' ^?! road ^. roadDef
      start   = round $ ab (state' ^?! ship ^. _z / blockHeight)
      ab  x   = if x >= 0 then x else 0


renderPause :: GLFW.Window -> Game -> Resources -> IO Bool
renderPause win st res = do
    return False


renderMainMenu :: GLFW.Window -> Game -> Resources -> IO Bool
renderMainMenu win st res = do
    return False

renderLevelSelect :: GLFW.Window -> Game -> Resources -> IO Bool
renderLevelSelect win st res = do
    return False


{-



renderLevel :: [[Int]] -> GLdouble -> IO ()
renderLevel (row:xs) d = renderRow row (-levelStart) d >> renderLevel xs (d+l)
renderLevel []       _ = return ()

renderRow :: [Int] -> GLdouble -> GLdouble -> IO ()
renderRow (i:is) x z = do
    preservingMatrix $ do
        translate $ vector3d x 0.0 (-z)
        renderBlock i
    renderRow is (x+b) z
renderRow [] _ _ = return ()

renderBlock :: Int -> IO ()
renderBlock b = case lookup b testDef of
                     Just block -> render block
                     Nothing    -> error $ "Unknown blocktype: " ++ (show b)
  where
      render EmptyBlock           = return ()
      render (Block color height) = drawBasicBlock color height
      




render :: GLFW.Window -> IORef GLdouble -> GameStatus -> GameState -> Resources -> IO Bool
render win camZref (GameMainMenu) st mgr = do

    camZ <- get camZref



    clear [ColorBuffer, DepthBuffer]
    depthFunc $= Just Less
    loadIdentity

    lookAt (Vertex3 0.0 0.8 camZ) (Vertex3 0.0 4.0 (-200.0)) (Vector3 0.0 1.0 0.0)

    renderLevel testLevel 2.0

    preservingMatrix $ do
      color $ color4d_ "#ffff00"
      translate $ vector3d 0.0 0.5 ((-5)-camZ)
      cube 0.5

    
    get errors >>= mapM_ (\e -> putStrLn $ "GL Error: " ++ show e)
    GLFW.swapBuffers win
    flush
    GLFW.pollEvents
    camZref $= (camZ - 0.01)
    return False









cube :: GLdouble -> IO ()
cube w = do
  renderPrimitive Quads $ do
      v3 0.0 0.0 (0.0 :: GLdouble)
      v3 0.0 w   0.0
      v3 w   w   0.0
      v3 0.0 w   0.0

      v3 0.0 w   0.0
      v3 w   0.0 w
      v3 w   w   w
      v3 w   w   0.0

      v3 w   w   0.0
      v3 w   w   w
      v3 0.0 w   w
      v3 0.0 w   0.0
  where
      v3 :: GLdouble -> GLdouble -> GLdouble -> IO ()
      v3 x y z = vertex $ Vertex3 x y z

    -}
