
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


initGL :: IO ()
initGL = do
    matrixMode $= Projection
    loadIdentity
    ortho 0 (fromIntegral gameWidth) (fromIntegral gameHeight) 0 (-1) 0
    matrixMode $= Modelview 0
    loadIdentity

initGL2 :: IO ()
initGL2 = do
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    shadeModel $= Flat
    matrixMode $= Projection
    loadIdentity
    ortho 0.0 (toR gameWidth) 0.0 (toR gameHeight) (-1.0) 0.0
    matrixMode $= Modelview 0
    return ()



render :: GLFW.Window -> GameStatus -> GameState -> Resources -> IO Bool
render win (GameMainMenu) st mgr = begin 
                             >> renderMenu st mgr
                             >> end win
                             >> return False
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

    (Just window) <- GLFW.createWindow gameWidth gameHeight "DrivingTheSky!" Nothing Nothing

    initGL2

    runGLFW window (\st -> render window (gameStatus st) st =<< updateResources res st) drivingthesky 

    GLFW.destroyWindow window

    putStrLn "bye!"
