
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
render win (GameMainMenu) st mgr = do

  viewport $= (Position 0 0, Size (fromIntegral gameWidth) (fromIntegral gameHeight))
  matrixMode $= Projection
  loadIdentity
  matrixMode $= Modelview 0
  loadIdentity
  clearColor $= Color4 1 1 1 1
  clear [ColorBuffer]
  c4 (Color4 0.5 0.5 0.5 1.0)
  cube 0.5 
  GLFW.swapBuffers win
  return False

{-
renderPrimitive Quads $ do
    clearColor $= Color4 1 1 1 1
    clear [ColorBuffer]
    color'
    v3 (Vertex3  0.0 0.0 0.0) >> color'
    v3 (Vertex3  2.0 0.0 0.0) >> color'
    v3 (Vertex3  2.0 2.0 0.0) >> color'
    v3 (Vertex3  0.0 2.0 0.0) >> color'
    GLFW.swapBuffers win
    return False
  where
      color' = c4 (Color4 0.5 0.3 0.3 1.0)
      w' = toR gameWidth
      h' = toR gameHeight -}

{-
  renderPrimitive Quads $ do
    color'
    tC2 (TexCoord2 0 1) >> v3 (Vertex3 x y 0.0) >> color'
    tC2 (TexCoord2 0 0) >> v3 (Vertex3 x (y + h') 0.0) >> color'
    tC2 (TexCoord2 1 0) >> v3 (Vertex3 (x + w') (y + h') 0.0) >> color'
    tC2 (TexCoord2 1 1) >> v3 (Vertex3 (x + w') y 0.0) >> color'
  texture Texture2D $= Disabled

color' = c4 (Color4 1.0 1.0 1.0 alpha)
    w'     = toR $ texW tex
    h'     = toR $ texH tex
    
                             >> renderMenu st mgr
                             >> end win
                             >> return False
-}
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

    -- GLFW.setWindowSizeCallback window (Just $ \_ w h -> putStrLn "Resized..." >> viewport $= (Position 0 0, (Size (fromIntegral w) (fromIntegral h))))
    --initGL2
    --viewport $= (Position 0 0, (Size 1377 768))
    GLFW.makeContextCurrent m




    runGLFW window (\st -> render window (gameStatus st) st =<< updateResources res st) drivingthesky 

    GLFW.destroyWindow window

    putStrLn "bye!"
