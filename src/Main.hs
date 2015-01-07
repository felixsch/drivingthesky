
import Control.Monad

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW

import Control.Lens

import FRP.Yampa.GLFW

import Render
import Game
import Resource

import Paths_drivingthesky



resize :: GLFW.Window -> Int -> Int -> IO ()
resize _ w h = do
    viewport   $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    matrixMode $= Projection

    loadIdentity
    perspective 50.0 (fromIntegral w/ fromIntegral h) 0.1 200.0

    matrixMode $= Modelview 0
    loadIdentity
    flush

    putStrLn "Resize..."


updateResources :: Resources -> Game -> IO Resources
updateResources res st = return res


main :: IO ()
main = do

    path  <- getDataDir
    res   <- initResources path


    hasInit <- GLFW.init

    unless hasInit $ error "Could not intialize GLFW"

    GLFW.setErrorCallback $ Just (\_ err -> putStrLn $ "GLFW ERROR: " ++ err)
    m@(Just window) <- GLFW.createWindow 1376 786 "DrivingTheSky!" Nothing Nothing
    GLFW.makeContextCurrent m
    resize window 1376 786


    runGLFW window resize (\game -> 
        render (game ^. status) window game =<< updateResources res game) drivingthesky 

    GLFW.destroyWindow window

    putStrLn "bye!"


