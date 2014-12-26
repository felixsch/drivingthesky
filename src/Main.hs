
import Control.Monad
import Control.Applicative

import Data.IORef

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW


import FRP.Yampa
import FRP.Yampa.GLFW

import Resource
import Level
import Game
import Menu
import State
import Util

import Paths_drivingthesky

import Debug.Trace



render :: GLFW.Window -> GameStatus -> GameState -> Resources -> IO Bool
render win (GameMainMenu) st mgr = do

    clear [ColorBuffer, DepthBuffer]
    depthFunc $= Just Less
    loadIdentity

    lookAt (Vertex3 0.0 0.0 (10.0)) (Vertex3 0.0 0.0 (-1.0)) (Vector3 0.0 1.0 0.0)
    --c4 (Color4 0.5 0.5 0.5 1.0) 

    preservingMatrix $ do
      rotate 15.0 $ vector3d 1.0 0.0 0.0
      drawBasicBlock (BasicBlock "#ff0000" 1.0) Flying 0.0 0.0
    
    get errors >>= mapM_ (\e -> putStrLn $ "GL Error: " ++ show e)
    GLFW.swapBuffers win
    flush
    GLFW.pollEvents
    return False

resize :: GLFW.Window -> Int -> Int -> IO ()
resize win w h = do
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    matrixMode $= Projection
    loadIdentity
    perspective 45.0 (fromIntegral w/ fromIntegral h) 0.1 100.0
    matrixMode $= Modelview 0
    loadIdentity
    flush
    putStrLn "Resize..."



data DrawMode = Flying
              | Tower


class Drawable a where
    draw :: a -> DrawMode -> GLdouble -> GLdouble -> IO ()




levelStart :: GLdouble
levelStart = -1.75

l :: GLdouble
l = 10.0

b :: GLdouble
b = 4.0

data Block = BasicBlock String GLdouble 


drawBasicBlock :: Block -> DrawMode -> GLdouble -> GLdouble -> IO ()
drawBasicBlock (BasicBlock c h) mode x z = do
    color $ color4d_ c
    renderPrimitive Quads $ do 
      v3 x     h     (-z)
      v3 (x+b) h     (-z) 
      v3 (x+b) 0.0   (-z)
      v3 x     0.0   (-z)

      v3 x     h     (-z)
      v3 (x+b) h     (-z)
      v3 (x+b) h     (-(z+l))
      v3 x     h     (-(z+l))
  where
      v3 x y z = vertex $ Vertex3 x y z







cube :: GLfloat -> IO ()
cube w = do
  renderPrimitive Quads $ do
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) w (-w)
                            

updateResources :: Resources -> GameState -> IO Resources
updateResources res st = return res





main :: IO ()
main = do

    path  <- getDataDir
    res   <- initResources path


    hasInit <- GLFW.init

    unless hasInit $ error "Could not intialize GLFW"

    GLFW.setErrorCallback $ Just $ (\_ err -> putStrLn $ "GLFW ERROR: " ++ err)
    m@(Just window) <- GLFW.createWindow 1376 786 "DrivingTheSky!" Nothing Nothing
    GLFW.makeContextCurrent m
    resize window 1376 786

    runGLFW window resize (\st -> render window (gameStatus st) st =<< updateResources res st) drivingthesky 

    GLFW.destroyWindow window

    putStrLn "bye!"
