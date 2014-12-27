{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Resource
  ( Resource(..)
  , loadResource

  , Resources(..)
  , newResources
  , addResources
  , delResources
  , getResource

  , withResource
  , withTexture
  
  , getShaders
  , getTextures

  , initResources
  ) where

import Control.Monad
import Control.Applicative

import System.FilePath.Posix

import Data.IORef
import Data.Maybe 
import qualified Data.Map as M

import Graphics.Rendering.OpenGL hiding (Level)

import Graphics.GLUtil.Textures
import Graphics.GLUtil.Shaders

import Texture
import Road



data Resource = TextureR { resName :: String
                         , resPath :: FilePath
                         , resTexture     :: Maybe Tex }
              | RoadR { resName :: String
                       , resPath :: FilePath
                       , resRoad     :: Maybe Road }
              | ShaderR { resName :: String
                        , resShaderType :: ShaderType
                        , resPath :: FilePath
                        , resShader :: Maybe Shader }

--              | MusicR { resName :: String
--                       , resPath :: FilePath
--                       , resMusic     :: Maybe Music }
--              | FontR { resName :: String
--                      , resPath :: FilePath
--                      , resFont     :: Maybe Font } 

instance Show Resource where
  show (TextureR name path _) = name ++ " (texture:" ++ path ++ ")"
  show (RoadR name path _) = name ++ " (level:" ++ path ++ ")"
  show (ShaderR name typ path _) = name ++ " (" ++ show typ ++ " shader: " ++ path ++ ")"

 -- show (MusicR name path _) = name ++ " (music:" ++ path ++ ")"
 -- show (FontR name path _) = name ++ " (font:" ++ path ++ ")"


loadResource :: Resource -> IO Resource
loadResource tex@(TextureR name path mTex)
  | isJust mTex   = return tex
  | otherwise     = TextureR name path . Just <$> loadTexture' path
loadResource level@(RoadR name path mLevel)
  | isJust mLevel = return level
  | otherwise     = RoadR name path . Just <$> loadRoad path
loadResource shader@(ShaderR name typ path mShader)
  | isJust mShader = return shader
  | otherwise     = ShaderR name typ path . Just <$> loadShader typ path



--loadResource music@(MusicR name path mMusic)
--  | isJust mMusic = return music
--  | otherwise     = MusicR name path . Just <$> loadMUS path
--loadResource font@(FontR name path mFont)
--  | isJust mFont  = return font
--  | otherwise     = FontR name path . Just <$> openFont path 32


data Resources = Resources (IORef (M.Map String Resource))


loadAndAdd :: M.Map String Resource -> Resource -> IO (M.Map String Resource)
loadAndAdd res r = do
    lr <- loadResource r
    return $ M.insert (resName lr) lr res

newResources :: [Resource] -> IO Resources
newResources i = Resources <$> (newIORef =<< foldM loadAndAdd M.empty i)
 
getResource :: Resources -> String -> IO (Maybe Resource)
getResource (Resources ref) name = M.lookup name <$> readIORef ref


addResources :: Resources -> [Resource] -> IO ()
addResources (Resources ref) resources = do
    res    <- readIORef ref
    newRes <- foldM loadAndAdd res resources
    atomicWriteIORef ref newRes

delResources :: Resources -> [String] -> IO ()
delResources (Resources ref) names = do
        res <- readIORef ref
        atomicWriteIORef ref (foldl (flip M.delete) res names)

withResource :: Resources -> String -> (Resource -> IO ()) -> IO ()
withResource resources name f = maybe (return ()) f =<< getResource resources name

withTexture :: Resources -> String -> (Tex -> IO ()) -> IO ()
withTexture r n f = withResource r n toTexture
  where
      toTexture (TextureR _ _ (Just t)) = f t
      toTexture (TextureR n p _ )       = putStrLn $ "Texture " ++ n ++ "` is not loaded. (" ++ p ++ ")"
      toTexture _                       = return ()


getShaders :: Resources -> [String] -> IO [Shader]
getShaders res = mapM (\name -> maybe (notFound name) toShader =<< getResource res name)
  where
      notFound s  = error $ "Could not find Shader named `" ++ s ++ "`"
      toShader  (ShaderR _ _ _ (Just x)) = return x
      toShader  _                      = error "Could not fetch get Shader..."

getTextures :: Resources -> [String] -> IO [Tex]
getTextures res = mapM (\name -> maybe (notFound name) toTexture =<< getResource res name)
  where
     notFound t = error $ "Could not find Texture named `" ++ t ++ "`"
     toTexture (TextureR _ _ (Just x)) = return x
     toTexture _                       = error "Could not fetch Texture..."


initResources :: FilePath -> IO Resources
initResources path = newResources 
  [ TextureR "logo" (inst "data/logo.png") Nothing
  , TextureR "bg"   (inst "data/bg.png") Nothing
  , TextureR "play" (inst "data/play.png") Nothing
  , TextureR "quit" (inst "data/quit.png") Nothing
  , RoadR "level1" (inst "levels/basic.lvl") Nothing
  --, ShaderR "menuVert" VertexShader (inst "data/shader/menu.vert") Nothing
  --, ShaderR "menuFrag" FragmentShader (inst "data/shader/menu.frag") Nothing
  ]
  where
     inst = (</>) path
