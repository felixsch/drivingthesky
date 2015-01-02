module FRP.Yampa.GLFW
  ( GLFW(..)
  , runGLFW
  , redraw
  , keysOnly
  , keyPress
  , key
  , keyPressed
  ) where

import Control.Monad
import Data.IORef

import Graphics.UI.GLFW

import FRP.Yampa



data GLFW = Redraw
          | KeyInput Key KeyState ModifierKeys
          deriving (Eq, Show)


type ResizeFunc = (Window -> Int -> Int -> IO ())


runGLFW :: Window -> ResizeFunc -> (st -> IO Bool) -> SF (Event GLFW) st -> IO ()
runGLFW win resize io sf = do
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

    makeContextCurrent $ Just win

    setWindowCloseCallback win (Just $ \_ -> void $ writeIORef close True)
    setWindowSizeCallback win (Just $ resize)
    setKeyCallback win (Just $ \_ k _ s m -> event' $ KeyInput k s m)

    unlessM $ do
        event' Redraw
        readIORef close

unlessM :: (Monad m) => m Bool -> m ()
unlessM f = do
    run <- f
    unless run $ unlessM f




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
      whenPressed (KeyInput k KeyState'Pressed modifier)   = Just (k, modifier)
      whenPressed (KeyInput k KeyState'Repeating modifier) = Just (k, modifier)
      whenPressed _                                      = Nothing

key :: Key -> SF (Event (Key, ModifierKeys)) (Event (Key, ModifierKeys))
key k = arr $ filterE (\(k', _) -> k == k')


keyPressed :: Key -> SF (Event GLFW) Bool
keyPressed k = keyPress >>^ fromEvent . mapFilterE isKey
  where
      isKey (k', _) = Just (k == k')

