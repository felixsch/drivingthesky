module Block
    ( defaultBlockHeight
    , defaultBlockWidth
    , blockHeight
    , blockColor
    , blockType
    ) where

import Control.Applicative
import Data.Monoid
import Control.Lens
import Control.Wire.Core
--import FRP.Netwire
import Graphics.Rendering.OpenGL
import Graphics.GLUtil

import Linear

import Resource
import Types
import Util


defaultBlockWidth :: GLf
defaultBlockWidth = 0.8

defaultBlockHeight :: GLf
defaultBlockHeight = 2.0

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

instance Renderable Block where
  render = renderBlock


aabbBlock :: Object Block -> AABB
aabbBlock (Object pos@(Vector3 x y z) _ block) = AABB (toVertex pos)
                                                      (Vertex3 maxX maxY maxZ)
  where
    h = blockHeight block
    maxX = x + defaultBlockWidth
    maxY = y + h
    maxZ = z - defaultBlockHeight


getBlockShader :: BlockRenderType -> Runtime ShaderProgram
getBlockShader t = fromMaybeM (fatal $ msg ++ show t) =<< (getShader $ show t)
  where
    msg = "Could not compile block shader. type = " 
    fromMaybeM f Nothing  = f
    fromMaybeM _ (Just a) = return a

renderBlock :: M44 GLf -> Object Block -> Runtime ()
renderBlock vp (Object pos _ EmptyBlock) = return ()
renderBlock vp (Object pos _ block)      = do
   shader <- getBlockShader (blockType block)
   io $ do
     objVertices <- fromSource ArrayBuffer vertices
     objColors   <- fromSource ArrayBuffer colors
     objIndices  <- fromSource ElementArrayBuffer indices

     currentProgram $= (Just $ program $ shader)

     enableAttrib shader "coord"
     enableAttrib shader "color"

     bindBuffer ArrayBuffer $= Just objVertices
     setAttrib shader  "coord3d" 
       ToFloat $ VertexArrayDescriptor 3 Float 0 offset0

     bindBuffer ArrayBuffer $= Just objColors
     setAttrib shader  "color" 
       ToFloat $ VertexArrayDescriptor 3 Float 0 offset0

     -- normals

     -- mvp

     bindBuffer ElementArrayBuffer $= Just objIndices
     drawIndexedTris (fromIntegral $ length indices)

     vertexAttribArray (getAttrib shader "coord") $= Disabled
     vertexAttribArray (getAttrib shader "color") $= Disabled
  where
    b = defaultBlockWidth
    h = blockHeight block
    t = defaultBlockHeight
    colors   = replicate 8 (color4f_ $ blockColor block)
    vertices = V3 <$> [b, b] <*> [h, -h] <*> [t, -t]
    indices :: [ V3 GLuint ]
    indices  = [ V3 5 3 1 -- front
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
               
{-
p0 V3 b h z
p1 V3 b h (-z)
p2 V3 b (-h) z
p3 V3 b (-h) (-z)
p4 V3 (-b) h z
p5 V3 (-b) h (-z)
p6 V3 (-b) (-h) z
p7 V3 (-b) (-h) (-z)

         p4----p0
        /|    /|
       / |   / |
      /  p6-/--p2
     /  / */  /
    p5----p1 / 
    | /   | /
    |/    |/
    p7----p3
-}




     
     

    {- 
     

   
   

    



renderBlock :: M44 GLf -> Object Block -> Runtime ()
renderBlock (Object _   _ (EmptyBlock)                 ) = return ()
renderBlock (Object pos _ (Block c h BlockRenderNormal)) = simpleRender t pos c h
renderBlock (Object pos _ _                            ) = simpleRender BlockRenderNormal pos "#ffff00" 0.2

simpleRender :: BlockRenderType -> Vector3 GLf -> String -> GLf -> Runtime ()
simpleRender _ p c h = wit
  





{-renderBlock :: Object Block -> Runtime ()
renderBlock (Object pos _ (Block color height)) = renderBasicBlock pos color height
renderBlock (Object pos _ _                   ) = renderBasicBlock pos "#ffff00" 0.2 -}


renderBasicBlock :: Vector3 GLf -> String -> GLf -> Runtime ()
renderBasicBlock pos c h = liftIO $ preservingMatrix $ do
    translate pos
    color $ color4f_ c
    renderPrimitive Quads $ do 

      vert3 0    0     0
      vert3 0    h     0
      vert3 0    h     (-d)
      vert3 0    0     (-d)
      vert3 w    0     (-d)
      vert3 w    h     (-d)
      vert3 w    h     0
      vert3 w    0     0



V3 0 0 0
V3 0 h 0
V3 0 h (-w)
V3 0 0 (-w)
V3 w 0 (-w)
V3 w h (-w)
V3 w h 0
V3 w 0 0


V3 0 0 0
V3 0 2 0
V3 1 2 (-2)
V3 1 0 0
V3 1 0 (-2)
V3 1 2 0
V3 0 0 (-2)

V3 0 2 (-2)










  where
      d = 0.0
      darken f (Color4 r g b a) = Color4 (r-f) (g-f) (b-f) a -}



