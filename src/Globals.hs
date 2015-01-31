module Globals
  ( renderStartPos
  , blockWidth, blockHeight
  , movementPerStep
  , accel
  , shipSize
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
movementPerStep = 10.0

accel :: GLf
accel = 0.3

shipSize :: GLf
shipSize = 0.15

