module Globals
  ( renderStartPos
  , blockWidth
  , blockHeight
  ) where

import Util


renderStartPos :: GLf
renderStartPos = 3.5 * blockWidth

blockWidth :: GLf
blockWidth = 1.0

blockHeight :: GLf
blockHeight = 2.5
