module Globals
  ( renderStartPos
  , blockWidth
  , blockHeight
  , shipWidth, shipHeight
  , movementPerStep
  , accel
  ) where

import Util


blocksPerLine :: GLf
blocksPerLine = 7

renderStartPos :: GLf
renderStartPos = -((blocksPerLine * blockWidth) / 2)

blockWidth :: GLf
blockWidth = 0.8

blockHeight :: GLf
blockHeight = 2.0

movementPerStep :: GLf
movementPerStep = 100.0

accel :: GLf
accel = 0.3


shipWidth :: GLf
shipWidth = 0.5

shipHeight :: GLf
shipHeight = 1.0
