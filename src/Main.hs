
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





render :: GLFW.Window -> GameStatus -> GameState -> Resources -> IO Bool
render win (GameMainMenu) st mgr = do
    clear [ColorBuffer, DepthBuffer]
    c4 (Color4 0.5 0.5 0.5 1.0)

    cube 0.5
    GLFW.swapBuffers win
    GLFW.pollEvents
    return False

resize :: GLFW.Window -> Int -> Int -> IO ()
resize win w h = do
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    perspective 45.0 (fromIntegral w/ fromIntegral h) 0.1 100.0
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
    m@(Just window) <- GLFW.createWindow 640 360 "DrivingTheSky!" Nothing Nothing

    runGLFW window resize (\st -> render window (gameStatus st) st =<< updateResources res st) drivingthesky 

    GLFW.destroyWindow window

    putStrLn "bye!"
