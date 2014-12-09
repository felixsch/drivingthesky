module Util
  ( whenM
  ) where

import Data.Maybe

whenM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenM s f = maybe (return ()) f =<< s
