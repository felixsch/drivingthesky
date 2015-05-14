module Block
    ( blockHeight
    , blockWidth
    , blockGetHeight
    ) where

import Control.Lens
import Control.Wire.Core
import FRP.Netwire
import Graphics.Rendering.OpenGL

import Types
import Util


blockWidth :: GLf
blockWidth = 0.8

blockHeight :: GLf
blockHeight = 2.0

blockGetHeight :: Block -> GLf
blockGetHeight (Start _ h _)  = h
blockGetHeight (Block _ h _)  = h
blockGetHeight (EmptyBlock) = 0.0
blockGetHeight (Goal _ h _)   = h


instance Entity Block where
   wire    = mkConst (Left mempty)
   collide = mkSF_ snd
   aabb    = aabbBlock

instance Renderable Block where
  render = renderBlock


aabbBlock :: Object Block -> AABB
aabbBlock (Object pos@(Vector3 x y z) _ block) = AABB (toVertex pos)
                                                      (Vertex3 maxX maxY maxZ)
  where
    h = blockGetHeight block
    maxX = x + blockWidth
    maxY = y + h
    maxZ = z - blockHeight


renderType :: Block -> BlockRenderType
renderTYpe 

renderBlock :: Object Block -> Runtime ()
renderBlock (Object _   _ (EmptyBlock) ) = return ()
renderBlock (Object pos _ (Block c h t)) = simpleRender pos c h t
renderBlock (Object pos _ _            ) = simpleRender pos "#ffff00" 0.2 BlockRenderNormal

simpleRender :: Vector3 GLf -> String -> GLf -> BlockRenderType -> Runtime ()
simpleRender p c h t = do
  





{-renderBlock :: Object Block -> Runtime ()
renderBlock (Object pos _ (Block color height)) = renderBasicBlock pos color height
renderBlock (Object pos _ _                   ) = renderBasicBlock pos "#ffff00" 0.2 -}


renderBasicBlock :: Vector3 GLf -> String -> GLf -> Runtime ()
renderBasicBlock pos c h = liftIO $ preservingMatrix $ do
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



