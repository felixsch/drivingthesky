
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
    loadIdentity

    lookAt (Vertex3 0.0 0.0 (5.0)) (Vertex3 0.0 0.0 (-1.0)) (Vector3 0.0 1.0 0.0)
    c4 (Color4 0.5 0.5 0.5 1.0) 

    preservingMatrix $ do
      cube 0.5
    
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
