
import Control.Applicative

import Data.IORef

import qualified Graphics.UI.SDL as SDL
import Graphics.Rendering.OpenGL

import FRP.Yampa

import Resource
import Level
import Input
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

initDisplay :: IO ()
initDisplay = do
    SDL.init [SDL.InitEverything]
    SDL.glSetAttribute SDL.glDoubleBuffer 1

    -- SDL.initAudio


initWindow :: IO ()
initWindow = do
    screen <- SDL.setVideoMode w h bbp [SDL.OpenGL]

    SDL.setCaption "Driving the sky" ""
    SDL.enableUnicode True
    SDL.showCursor False

    initGL
  where
      w = fromIntegral gameWidth
      h = fromIntegral gameHeight
      bbp = 24


render :: GameStatus -> GameState -> Resources -> IO Bool
render (GameMainMenu) st mgr = 
                             begin 
                             >> renderMenu st mgr
                             >> cube 0.2

                             >> end
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


initTime :: IO (IORef Double)
initTime = newIORef (0.0 :: Double)

updateTime :: IORef Double -> IO Double
updateTime ref = do
    old   <- readIORef ref
    new <- fromIntegral <$> SDL.getTicks
    atomicWriteIORef ref new
    return $ (new - old) / 1000




main :: IO ()
main = do

    path  <- getDataDir
    time  <- initTime
    input <- initInput
    res   <- initResources path

    reactimate (initWindow  >> updateInput input)
               (\_ -> (,) <$> updateTime time <*> (Just <$> updateInput input))
               (\_ st -> render (gameStatus st) st =<< updateResources res st) 
               drivingthesky
