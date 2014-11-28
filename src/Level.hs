

import Data.Attoparsec.ByteString.Char8
import qualified Data.Sequence as S 

type Color = String
type Height = Double


data Level = Level 
  { name :: String
  , bg   :: String
  , defines :: [Defines]
  , road :: S.Seq Step }

data Defines = StepType
             | BlockType

data Step = EmptyBlock
          | Road Color Height
          | Accelerator Height
          | Brake Height
          | Tunnel Color Color Height Height
          | Start Height
          | Goal Height
          | Harmfull Height



