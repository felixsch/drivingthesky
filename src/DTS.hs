{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DTS 
  ( DTSData(..)
  , DTSError(..)
  , DTSRuntime
  , DTS(..)
  
  , fatal, warn, info
  , get, put, modify
  , mempty, mappend
  , liftIO

  , loadRoad
  ) where



import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Data.Monoid

-- Error Handling
import Control.Monad.Error
import Control.Exception
import System.IO.Error


-- TVar
import Control.Monad.STM
import Control.Concurrent.STM.TVar

-- GLFW
import qualified Graphics.UI.GLFW as GLFW


import System.FilePath.Posix
import Road



data DTSData = DTSData
  { dataPath   :: FilePath
  , glfwWindow :: GLFW.Window
  , loadedRoad :: Maybe Road
  }

data DTSError = Fatal String
              | Warning String
              | Info String

instance Error DTSError where
  strMsg = Info


fatal :: String -> DTS a
fatal = throwError . Fatal

warn :: String -> DTS a
warn = throwError . Warning

info :: String -> DTS a
info = throwError . Info

type DTSRuntime = TVar DTSData

newtype DTS a = DTS { runDTS :: ReaderT DTSRuntime (ErrorT DTSError IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader DTSRuntime, MonadError DTSError)

instance (Monoid a) => Monoid (DTS a) where
  mempty = return mempty
  mappend = liftM2 mappend

instance MonadState DTSData DTS where
  get   = liftIO . readTVarIO =<< ask
  put d = do
    ref <- ask
    liftIO $ atomically $ writeTVar ref d

dtsMkRuntime :: FilePath -> GLFW.Window -> IO DTSRuntime
dtsMkRuntime p w = newTVarIO $ DTSData p w Nothing

dts :: DTSRuntime -> DTS a -> IO (Either DTSError a)
dts r (DTS f) = runErrorT $ runReaderT f r


loadRoad :: String -> DTS ()
loadRoad name = do
  path <- dataPath <$> get
  def <- liftIO (try $ readFile (path </> "roads" </> name) :: IO (Either IOError String))
  case def of
    Left _ -> fatal ("Could not load road (road =" ++ path </> "roads" </> name ++ ")")
    Right d -> modify (\dat -> dat { loadedRoad = mkRoad $ read d })
      where







 








