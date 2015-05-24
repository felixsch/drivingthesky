module Block
    ( defaultBlockHeight
    , defaultBlockLength
    , defaultBlockWidth
    , blockHeight
    , blockColor
    , blockType
    , blockVertices
    , blockIndices
    , blockNormals
    , renderBlock
    ) where

import Control.Applicative
import Data.Monoid
import Control.Lens
import Control.Wire.Core
import Graphics.Rendering.OpenGL
import Graphics.GLUtil

import Linear

import Resource
import Types
import Util


defaultBlockLength :: GLf
defaultBlockLength = 1.0

defaultBlockWidth :: GLf
defaultBlockWidth = 4.0

defaultBlockHeight :: GLf
defaultBlockHeight = 2.0


blockVertices :: [V3 GLf]
blockVertices = V3 <$> [l, -l] <*> [h, -h] <*> [w, -w]
  where
    l = defaultBlockLength / 2.0
    w = defaultBlockWidth / 2.0
    h = defaultBlockHeight / 2.0

blockIndices :: [V3 GLuint]
blockIndices = [ V3 5 3 1 -- front
               , V3 5 7 2

               , V3 4 1 0 -- top
               , V3 4 5 1
               
               , V3 4 2 0 -- back
               , V3 4 6 2

               , V3 6 7 3 -- bottom
               , V3 3 2 6

               , V3 0 1 2 -- left
               , V3 1 3 2

               , V3 5 7 6 -- right
               , V3 6 4 5 ]

blockNormals :: [V3 GLf]
blockNormals = blockVertices


blockHeight :: Block -> GLf
blockHeight (Start _ h _)  = h
blockHeight (Block _ h _)  = h
blockHeight (EmptyBlock) = 0.0
blockHeight (Goal _ h _)   = h

blockColor :: Block -> String
blockColor (Start c _ _) = c
blockColor (Block c _ _) = c
blockColor (Goal  c _ _) = c
blockColor (EmptyBlock ) = "#0000ff"

blockType :: Block -> BlockRenderType
blockType (Start _ _ t)  = t
blockType (Block _ _ t)  = t
blockType (Goal  _ _ t)  = t
blockType (EmptyBlock )  = BlockRenderWire


instance Entity Block where
   wire    = mkConst (Left mempty)
   collide = mkSF_ snd
   aabb    = aabbBlock


aabbBlock :: Object Block -> AABB
aabbBlock (Object pos@(V3 x y z) _ block) = AABB pos (V3 maxX maxY maxZ)
  where
    h = blockHeight block
    maxX = x + defaultBlockWidth
    maxY = y + h
    maxZ = z - defaultBlockHeight

renderBlock :: GLsizei -> M44 GLf -> ShaderProgram -> Object Block -> Runtime ()
renderBlock i vp _  (Object _ _ EmptyBlock) = return ()
renderBlock i vp prg (Object pos vel block) = io $ do
  asUniform mvp $ getUniform prg "mvp"
  asUniform color $ getUniform prg "color"
  drawIndexedTris i
 where
   mvp = vp !*! model
   model = mkTransformationMat im33 pos
   im33 :: V3 (V3 GLf)
   im33 = identity
   color = color4f_ $ blockColor block
