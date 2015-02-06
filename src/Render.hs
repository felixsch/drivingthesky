module Render where
import Control.Monad
import Prelude hiding (foldr)
import Control.Lens

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL

import qualified Data.Sequence as S
import qualified Data.Map as M
import Data.Foldable
import Data.Maybe

import Util
import State
import Game
import Road
import Resource
import Globals
import Entity
import Ship
import Block


renderScene :: GameStatus -> GLFW.Window -> Game -> Resources -> IO Bool
renderScene status
  | status == Running    = renderGame
  | status == Pause      = renderPause
  | status == MainMenu   = renderMainMenu

{-
roadShunk :: Int -> GameState -> S.Seq [Block]
roadShunk start state = foldr buildRoad S.empty $
    S.drop start (road' ^. roadDef)
  where
     road' = state ^?! road
     buildRoad row s = foldr buildRow [] row S.<| s
     buildRow block row = row ++ [getBlock block road']


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
-}

renderAABB :: String -> AABB -> IO ()
renderAABB c (AABB p1 p2) = do
    color $ color4f_ c
    renderPrimitive Lines $ do


        vert3 p1x p1y p1z
        vert3 p1x p1y p2z

        vert3 p1x p1y p2z
        vert3 p1x p2y p2z

        vert3 p1x p2y p2z
        vert3 p1x p2y p1z

        vert3 p1x p2y p1z
        vert3 p1x p1y p1z

        vert3 p1x p1y p1z
        vert3 p2x p1y p1z

        vert3 p1x p2y p1z
        vert3 p2x p2y p1z

        vert3 p2x p1y p1z
        vert3 p2x p2y p1z

        vert3 p2x p2y p1z
        vert3 p2x p2y p2z

        vert3 p2x p2y p2z
        vert3 p2x p1y p2z

        vert3 p2x p1y p2z
        vert3 p2x p1y p1z

        vert3 p2x p1y p2z
        vert3 p1x p1y p2z

        vert3 p2x p2y p2z
        vert3 p1x p2y p2z

  where
      p1x = p1 ^. _x
      p2x = p2 ^. _x

      p1y = p1 ^. _y
      p2y = p2 ^. _y

      p1z = p1 ^. _z
      p2z = p2 ^. _z

{-
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
-}










drawShip :: IO ()
drawShip = do
  preservingMatrix $ do
    scale shipSize shipSize shipSize
    renderPrimitive Triangles $ do

        color $ color4f_ "#383b53"
        vert3 0.0   0.0  (-z)
        vert3 (-x)  0.0  0.0
        vert3 0.0   y    (-iz)

        color $ color4f_ "#66717e"
        vert3 0.0   0.0  (-z)
        vert3 x     0.0  0.0
        vert3 0.0   y    (-iz)

  
        color $ color4f_ "#66717e"
        vert3 0.0   0.0  (-z)
        vert3 (-x)  0.0  0.0
        vert3 0.0   (-y) (-iz)

        color $ color4f_ "#383b53"
        vert3 0.0   0.0  (-z)
        vert3 x     0.0  0.0
        vert3 0.0   (-y) (-iz)

        color $ color4f_ "#32213a"
        vert3 0.0   (-y) (-iz)   
        vert3 0.0   y    (-iz)
        vert3 (-x)  0.0  0.0

        vert3 0.0   (-y) (-iz)   
        vert3 0.0   y    (-iz)
        vert3 x     0.0  0.0




{-
        vert3 (-x)    0.0    0.0    -- left wing
        vert3 0.0     0.0    (-z)
        vert3 (-i)    0.0    (-iz)

        vert3 (-x)    0.0    0.0
        vert3 (-i)    0.0    (-iz)
        vert3 (-i2)   0.0    0.0

        vert3 x       0.0    0.0    -- right wing
        vert3 0.0     0.0    (-z)
        vert3 (i)    0.0    (-iz)

        vert3 (x)     0.0    0.0
        vert3 i       0.0    (-iz)
        vert3 i2      0.0    0.0

        vert3 0.0     0.0    (-z)   -- middle bottom
        vert3 (-i)    0.0    (-iz)
        vert3 i       0.0    (-iz) -}
  where

    iz = 0.2 -- inner space
    x = 1.0
    z = 4.0
    y = 0.2
    shipSize = 0.15 :: GLf
    --i = x - 0.3      -- inner wing x
    --iz = 0.4     -- inner wing z
    --i2 = x - 0.1 -- wing back x

      
    

renderShip :: Vector3 GLf -> IO ()
renderShip ship = preservingMatrix $ do
                    translate vec
                    drawShip 
                    putStrLn $ "shipD = " ++ show vec
                    return ()
  where
      x = ship ^. _x
      y = ship ^. _y + 0.4
      z = ship ^. _z
      vec = Vector3 x y z



renderGame :: GLFW.Window -> Game -> Resources -> IO Bool
renderGame win game res = do

    clear [ColorBuffer, DepthBuffer]
    depthFunc $= Just Less
    loadIdentity

    --putStrLn $ "input  = " ++ show (game ^. input)
    putStrLn $ "ship   = " ++ show (ship')
    --putStrLn $ "eye    = " ++ show eye
    --putStrLn $ "startP = " ++ show renderStartPos
    --putStrLn $ "blockS = (" ++ show blockWidth ++ ", " ++ show blockHeight ++ ")"
    --putStrLn $ "(aabb, block)  = " ++ show (game ^. state ^?! currentBlock)
    lookAt eye view (Vector3 0.0 1.0 0.0)
    --putStrLn $ "start rendering with blockrow = " ++ show start
    renderRoad start road'
    render ship'
    renderAABB "#ff0000" (aabb ship')
   
    get errors >>= Prelude.mapM_ (\e -> putStrLn $ "GL Error: " ++ show e)

    GLFW.swapBuffers win
    flush
    GLFW.pollEvents

    return False
  where
      road'  = game ^. state ^?! road
      ship'  = game ^. state ^?! ship
      shipPos = ship' ^. pos
      eye :: Vertex3 GLdouble
      eye    = Vertex3 0.0 1.4 $ realToFrac (shipPos ^. _z + 3.6)
      view   = Vertex3 0.0 2.0 $ realToFrac ((-10) + shipPos ^. _z)
      start   = round $ abZero (((-1) * shipPos ^. _x - blockHeight) / blockHeight)


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
