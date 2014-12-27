module Render
  ( render
  ) where


import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL

import Util
import Game
import Road
import Resource




render :: GameStatus -> GLFW.Window -> Game -> Resources -> IO Bool
render status
  | status == Running    = renderGame
  | status == Pause      = renderPause
  | status == MainMenu   = renderMainMenu




renderGame :: GLFW.Window -> Game -> Resources -> IO Bool
renderGame win st res = do
    return False


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

drawBasicBlock :: String -> GLdouble -> IO ()
drawBasicBlock c h = do
    color $ color4d_ c
    renderPrimitive Quads $ do 

      n3 0.0    1.0  (0.0 :: GLdouble)
      v3 0.0    h     0.0
      v3 b      h    0.0
      v3 b      h    (-l)
      v3 0.0    h     (-l)


      color $ darken 0.2 $ color4d_ c

      n3 0.0    0.0   (-1.0 :: GLdouble)
      v3 0.0    h     0.0
      v3 b      h     0.0
      v3 b      d     0.0
      v3 0.0    d     (0.0 :: GLdouble)

      n3 (-1.0) 0.0   (0.0 :: GLdouble)
      v3 0.0    d     (0.0 :: GLdouble)
      v3 0.0    d     (-l)
      v3 0.0    h     (-l)
      v3 0.0    h     0.0

      n3 1.0    0.0   (0.0 :: GLdouble)
      v3 b      d     0.0
      v3 b      h     0.0
      v3 b      h     (-l)
      v3 b      d     (-l)
  where
      v3 x y z = vertex $ Vertex3 x y z
      n3 x y z = normal $ Normal3 x y z
      darken f (Color4 r g b a) = Color4 (r-f) (g-f) (b-f) a








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
