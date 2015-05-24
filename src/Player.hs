module Player where

import FRP.Netwire
import Control.Wire.Core

import Graphics.Rendering.OpenGL
import Graphics.GLUtil

import Linear

import Resource
import Types
import Util


instance Entity Player where
   wire       = runPlayer
   aabb       = getAABB
   canCollide = mkConst (Right True)
   collide    = playerCollide

runPlayer :: (Monoid e) => Wire s e Runtime () (Object a)
runPlayer = undefined

getAABB :: Object Player -> AABB
getAABB = undefined

playerCollide :: Wire s e Runtime (Object b, Object a) (Object a)
playerCollide = undefined

renderPlayer :: M44 GLf -> Object Player -> Runtime ()
renderPlayer vp (Object pos velo _) = do
  shader <- getShader "ship"
  io $ do
    bufVertices <- fromSource ArrayBuffer playerVertices
    bufNormals  <- fromSource ArrayBuffer playerNormals
    bufIndices  <- fromSource ArrayBuffer playerIndicies

    currentProgram $= (Just $ program shader)

    enableAttrib shader "v_coord"
    enableAttrib shader "v_normal"

    bindBuffer ArrayBuffer $= Just bufVertices
    setAttrib shader "v_coord"
      ToFloat $ VertexArrayDescriptor 3 Float 0 offset0

    bindBuffer ArrayBuffer $= Just bufNormals
    setAttrib shader "v_normal"
      ToFloat $ VertexArrayDescriptor 3 Float 0 offset0

    asUniform mvp   $ getUniform shader "mvp"
    asUniform color $ getUniform shader "color"
    drawIndexedTris $ fromIntegral (length playerIndicies)
  where
    mvp = vp !*! model
    model = mkTransformationMat im33 pos
    im33 :: V3 (V3 GLf)
    im33 = identity
    color = color4f_ "#ff0000"

playerVertices :: [V3 GLf]
playerVertices = undefined

playerNormals :: [V3 GLf]
playerNormals = undefined

playerIndicies :: [V3 GLuint]
playerIndicies = undefined
