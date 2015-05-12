{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Runtime
  ( Error
  , Runtime(..)
  
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
import System.FilePath.Posix
import qualified Graphics.UI.GLFW as GLFW


import {-# SOURCE #-} Data


data RuntimeError = Fatal String
              | Warning String
              | Info String

instance Error RuntimeError where
  strMsg = Info


fatal :: String -> Runtime st a
fatal = throwError . Fatal

warn :: String -> Runtime st a
warn = throwError . Warning

info :: String -> Runtime st a
info = throwError . Info

type Ref st = TVar st

newtype Runtime st a = Runtime { runRuntime :: ReaderT (Ref st) (ErrorT RuntimeError IO) a}
  deriving (Functor, Monad, MonadIO, MonadReader (Ref st), MonadError RuntimeError)

instance Applicative (Runtime st) where
    pure  = return
    (<*>) = ap

instance (Monoid a) => Monoid (Runtime st a) where
  mempty = return mempty
  mappend = liftM2 mappend

instance MonadState st (Runtime st) where
  get   = liftIO . readTVarIO =<< ask
  put d = do
    ref <- ask
    liftIO $ atomically $ writeTVar ref d

run :: Ref st -> Runtime st a -> IO (Either RuntimeError a)
run r (Runtime f) = runErrorT $ runReaderT f r
