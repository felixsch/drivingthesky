
module Runtime
  ( Runtime (..)
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Error
import Control.Concurrent.STM
import Control.Monad.IO.Class


type Ref st = TVar st

newtype Runtime st a = Runtime { runRuntime :: ReaderT (Ref st) (ErrorT Error IO) a }

