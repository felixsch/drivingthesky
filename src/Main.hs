
import Control.Monad

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import Control.Lens

import Data.IORef

import Paths_drivingthesky
import DTS
import Input
import Entity
import Road
import Util
import Block

import Player


resize :: Window -> Int -> Int -> IO ()
resize _ w h = do
    viewport   $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    matrixMode $= Projection

    loadIdentity
    perspective 50.0 (fromIntegral w/ fromIntegral h) 0.1 200.0

    matrixMode $= Modelview 0
    loadIdentity
    flush

    putStrLn "Resize..."


--updateResources :: Resources -> Game -> IO Resources
--updateResources res st = return res


main :: IO ()
main = do

    path  <- getDataDir
    --res   <- initResources path
    close <- newIORef False
    
    hasInit <- Graphics.UI.GLFW.init

    unless hasInit $ error "Could not intialize GLFW"

    setErrorCallback $ Just (\_ err -> putStrLn $ "GLFW ERROR: " ++ err)

    m@(Just window) <- createWindow 1376 786 "DrivingTheSky!" Nothing Nothing

    setWindowCloseCallback window (Just $ \_ -> void $ writeIORef close True)
    setWindowSizeCallback  window (Just $ resize)
 
    makeContextCurrent m

    resize window 1376 786

    -- run netwire

    destroyWindow window

    putStrLn "bye!"


