module Block where


import Graphics.Rendering.OpenGL

import Control.Monad.IO.Class
import Control.Wire.Core
import Data.Monoid
import Util
import Entity

import Runtime

type BlockColor  = String
type BlockHeight = GLf
type BlockAlpha  = GLf


blockWidth :: GLf
blockWidth = 0.8

blockHeight :: GLf
blockHeight = 2.0

data Block  = Start BlockColor BlockHeight
            | Block BlockColor BlockHeight
            | EmptyBlock
            | Goal BlockColor BlockHeight
            deriving (Show, Read, Eq)

blockGetHeight :: Block -> GLf
blockGetHeight (Start _ h)  = h
blockGetHeight (Block _ h)  = h
blockGetHeight (EmptyBlock) = 0.0
blockGetHeight (Goal _ h)   = h


isEmptyBlock :: Block -> Bool
isEmptyBlock (EmptyBlock) = True
isEmptyBlock _            = False



instance Entity Block where
    wire          = mkConst (Left mempty)
    aabb          = blockAABB

instance Renderable Block where
    render       = blockRender




blockAABB :: Object Block -> AABB
blockAABB (Object pos@(Vector3 x y z) _ block) = AABB (toVertex pos)
                                                      (Vertex3 maxX maxY maxZ)
  where
    h = blockGetHeight block
    maxX = x + blockWidth
    maxY = y + h
    maxZ = z - blockHeight


blockRender :: Object Block -> DTS ()
blockRender (Object pos _ (Block color height)) = liftIO $ renderBasicBlock pos color height
blockRender (Object pos _ _                   ) = liftIO $renderBasicBlock pos "#ffff00" 0.2


renderBasicBlock :: Vector3 GLf -> String -> GLf -> IO ()
renderBasicBlock pos c h = preservingMatrix $ do
    translate pos
    color $ color4f_ c
    renderPrimitive Quads $ do 

      norm3 0.0    1.0  0.0
      vert3 0.0    h     0.0
      vert3 blockWidth      h    0.0
      vert3 blockWidth      h    (-blockHeight)
      vert3 0.0    h     (-blockHeight)

      color $ darken 0.2 $ color4f_ c

      norm3 0.0    0.0   (-1.0)
      vert3 0.0    h     0.0
      vert3 blockWidth      h     0.0
      vert3 blockWidth      d     0.0
      vert3 0.0    d     0.0

      norm3 (-1.0) 0.0   0.0
      vert3 0.0    d     0.0
      vert3 0.0    d     (-blockHeight)
      vert3 0.0    h     (-blockHeight)
      vert3 0.0    h     0.0

      norm3 1.0    0.0   0.0
      vert3 blockWidth      d     0.0
      vert3 blockWidth      h     0.0
      vert3 blockWidth      h     (-blockHeight)
      vert3 blockWidth      d     (-blockHeight)
  where
      d = 0.0
      darken f (Color4 r g b a) = Color4 (r-f) (g-f) (b-f) a
      
