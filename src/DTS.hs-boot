module DTS
  ( DTS (..)
  ) where

import Control.Monad.Reader
import Control.Monad.Error
import Control.Concurrent.STM

data DTSData
data DTSError

type DTSRuntime = TVar DTSData

newtype DTS a = DTS { runDTS :: ReaderT DTSRuntime (ErrorT DTSError IO) a }

