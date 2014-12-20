module FRP.Yampa.GLFW
  ( GLFW(..)
  , runGLFW
  , redraw
  , resized
  , keysOnly
  , keyPress
  , keyPressed
  ) where

import Control.Monad
import Data.IORef

import Graphics.UI.GLFW

import FRP.Yampa



data GLFW = Redraw
          | Resize Int Int
          | KeyInput Key KeyState ModifierKeys
          deriving (Eq, Show)


runGLFW :: Window -> (st -> IO Bool) -> SF (Event GLFW) st -> IO ()
runGLFW win io sf = do
    tm  <- newIORef (0.0 :: Double)
    close <- newIORef False

    handle <- reactInit (return noEvent) (\_ _ st -> io st) sf


    let event' ev = do
            old <- readIORef tm
            (Just new) <- getTime

            atomicWriteIORef tm new

            stopped <- react handle (new - old, Just (Event ev))
            when stopped $ writeIORef close True >> putStrLn "Stop was triggered"
            return ()

    setWindowCloseCallback win (Just $ \_ -> void $ writeIORef close True)
  --  setWindowSizeCallback win (Just $ \_ w h ->  event' $ Resize w h)
    setKeyCallback win (Just $ \_ k _ s m -> event' $ KeyInput k s m)

    unlessM $ do
        event' Redraw
        readIORef close

unlessM :: (Monad m) => m Bool -> m ()
unlessM f = do
    run <- f
    unless run $ unlessM f


resized :: SF (Event GLFW) (Event (Int, Int))
resized = arr (mapFilterE f)
  where
      f (Resize w h) = Just (w,h)
      f _            = Nothing


redraw :: SF (Event GLFW) (Event ())
redraw = arr $ tagWith () . filterE (Redraw ==)


keysOnly :: SF (Event GLFW) (Event GLFW)
keysOnly = arr $ filterE isKeyInput
  where
      isKeyInput (KeyInput _ _ _) = True
      isKeyInput _                = False


keyPress :: SF (Event GLFW) (Event (Key, ModifierKeys))
keyPress = keysOnly >>^ mapFilterE whenPressed
  where
      whenPressed (KeyInput k KeyState'Pressed modifier) = Just (k, modifier)


keyPressed :: Key -> SF (Event GLFW) Bool
keyPressed key = keyPress >>^ fromEvent . mapFilterE (isKey key) 
  where
      isKey k (k', _) = Just (k == k')

