
import Control.Monad
import Control.Applicative

import Data.IORef

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as GLFW


import FRP.Yampa
import FRP.Yampa.GLFW

import Resource
--import Level
import Game
import Menu
import State
import Util

import Paths_drivingthesky

import Debug.Trace

type BlockColor  = String
type BlockHeight = GLdouble
type BlockAlpha = GLdouble

data Block  = Block BlockColor BlockHeight
            | EmptyBlock

testLevel :: [[Int]]
testLevel =
    [ [0,0,0,1,0,0,0]
    , [0,0,0,1,0,0,0]
    , [0,0,0,0,0,0,0]
    , [0,0,0,1,0,0,0]
    , [0,1,0,1,0,1,0]
    , [0,1,2,1,2,1,0]
    , [0,0,1,2,1,0,0]
    , [0,0,0,1,0,0,0]
    , [0,0,0,1,0,0,0]
    , [0,0,0,1,0,0,0]
    , [0,0,0,1,1,0,0]
    , [0,3,0,0,1,1,0]
    , [0,3,0,0,0,1,0]
    , [0,3,0,0,0,1,0]
    , [0,3,0,0,0,0,0]
    , [0,0,0,1,0,0,0]
    , [0,0,0,1,0,0,0]
    , [0,0,0,1,0,0,0]
    , [0,0,0,1,0,0,0]]

testDef :: [(Int, Block)]
testDef =
    [ (0, EmptyBlock)
    , (1, Block "#2288FF" 0.2)
    , (2, Block "#11447F" 0.2)
    , (3, Block "#ff0000" 0.5) ]



renderLevel :: [[Int]] -> GLdouble -> IO ()
renderLevel (row:xs) d = renderRow row (-levelStart) d >> renderLevel xs (d+l)
renderLevel []       _ = return ()

renderRow :: [Int] -> GLdouble -> GLdouble -> IO ()
renderRow (i:is) x z = do
    preservingMatrix $ do
        putStrLn  $ "Render block at " ++ (show x) ++ ", 0.0, " ++ (show z)
        translate $ vector3d x 0.0 (-z)
        renderBlock i
    renderRow is (x+b) z
renderRow [] _ _ = return ()

renderBlock :: Int -> IO ()
renderBlock b = case lookup b testDef of
                     Just block -> render block
                     Nothing    -> error $ "Unknown blocktype: " ++ (show b)
  where
      render EmptyBlock           = putStrLn "  :: EmptyBlock"
      render (Block color height) = drawBasicBlock color height >> (putStrLn $ "  :: Block " ++ color)
      




render :: GLFW.Window -> IORef GLdouble -> GameStatus -> GameState -> Resources -> IO Bool
render win camZref (GameMainMenu) st mgr = do

    camZ <- get camZref



    clear [ColorBuffer, DepthBuffer]
    depthFunc $= Just Less
    loadIdentity

    lookAt (Vertex3 0.0 0.8 camZ) (Vertex3 0.0 4.0 (-200.0)) (Vector3 0.0 1.0 0.0)

    renderLevel testLevel 2.0

    
    get errors >>= mapM_ (\e -> putStrLn $ "GL Error: " ++ show e)
    GLFW.swapBuffers win
    flush
    GLFW.pollEvents
    camZref $= (camZ - 0.04)
    return False

resize :: GLFW.Window -> Int -> Int -> IO ()
resize win w h = do
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    matrixMode $= Projection
    loadIdentity
    perspective 45.0 (fromIntegral w/ fromIntegral h) 0.1 2000.0
    matrixMode $= Modelview 0
    loadIdentity
    flush
    putStrLn "Resize..."




levelStart :: GLdouble
levelStart = 3.5 * b

l :: GLdouble
l = 2.5

b :: GLdouble
b = 1.0



drawBasicBlock :: String -> GLdouble -> IO ()
drawBasicBlock c h = do
    color $ color4d_ c
    diffuse (Light 0) $= color4f_ c
    renderPrimitive Quads $ do 
      n3  0.0   0.0   (-1.0 :: GLdouble)
      v3 0.0   h     0.0
      v3 b h     0.0
      v3 b 0.0   0.0
      v3 0.0     0.0   (0.0 :: GLdouble)

      n3 0.0     1.0  (0.0 :: GLdouble)
      v3 0.0     h     0.0
      v3 b h     0.0
      v3 b h     (-l)
      v3 0.0     h     (-l)

      n3 (-1.0) 0.0 (0.0 :: GLdouble)
      v3 0.0 0.0 (0.0 :: GLdouble)
      v3 0.0 0.0 (-l)
      v3 0.0 h   (-l)
      v3 0.0 h   0.0

      n3 1.0 0.0 (0.0 :: GLdouble)
      v3 b 0.0 0.0
      v3 b h 0.0
      v3 b h (-l)
      v3 b 0.0 (-l)
  where
      v3 x y z = vertex $ Vertex3 x y z
      n3 x y z = normal $ Normal3 x y z







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

 
lightPosition :: Vertex4 GLfloat
lightPosition = Vertex4 1.0 1.0 1.0 0.0 



main :: IO ()
main = do

    path  <- getDataDir
    res   <- initResources path
    camZ  <- newIORef (0.0 :: GLdouble)


    hasInit <- GLFW.init

    unless hasInit $ error "Could not intialize GLFW"

    GLFW.setErrorCallback $ Just $ (\_ err -> putStrLn $ "GLFW ERROR: " ++ err)
    m@(Just window) <- GLFW.createWindow 1376 786 "DrivingTheSky!" Nothing Nothing
    GLFW.makeContextCurrent m
    resize window 1376 786
    position (Light 0) $= lightPosition
    light (Light 0)    $= Enabled
    lighting $= Enabled 
    depthFunc $= Just Lequal




    runGLFW window resize (\st -> render window camZ (gameStatus st) st =<< updateResources res st) drivingthesky 

    GLFW.destroyWindow window

    putStrLn "bye!"
