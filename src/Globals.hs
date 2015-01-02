module Globals
  ( renderStartPos
  , blockWidth
  , blockHeight
  , movementPerStep
  , accel
  ) where

import Util


renderStartPos :: GLf
renderStartPos = -(3.5 * blockWidth)

blockWidth :: GLf
blockWidth = 1.0

blockHeight :: GLf
blockHeight = 2.5

movementPerStep :: GLf
movementPerStep = 0.05

accel :: GLf
accel = 0.5
