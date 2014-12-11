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

  , initResources
  ) where

import Control.Monad
import Control.Applicative

import System.FilePath.Posix

import Data.IORef
import Data.Maybe 
import qualified Data.Map as M

import Graphics.UI.SDL.Mixer (Music)
import Graphics.UI.SDL.Mixer.Music (loadMUS)

import Graphics.UI.SDL.TTF

import Texture
import Level




data Resource = TextureR { resName :: String
                         , resPath :: FilePath
                         , resTexture     :: Maybe Tex }
              | LevelR { resName :: String
                       , resPath :: FilePath
                       , resLevel     :: Maybe Level }
              | MusicR { resName :: String
                       , resPath :: FilePath
                       , resMusic     :: Maybe Music }
              | FontR { resName :: String
                      , resPath :: FilePath
                      , resFont     :: Maybe Font } 

instance Show Resource where
  show (TextureR name path _) = name ++ " (texture:" ++ path ++ ")"
  show (LevelR name path _) = name ++ " (level:" ++ path ++ ")"
  show (MusicR name path _) = name ++ " (music:" ++ path ++ ")"
  show (FontR name path _) = name ++ " (font:" ++ path ++ ")"


loadResource :: Resource -> IO Resource
loadResource tex@(TextureR name path mTex)
  | isJust mTex   = return tex
  | otherwise     = TextureR name path . Just <$> loadTexture path
loadResource level@(LevelR name path mLevel)
  | isJust mLevel = return level
  | otherwise     = LevelR name path . Just <$> loadLevel path
loadResource music@(MusicR name path mMusic)
  | isJust mMusic = return music
  | otherwise     = MusicR name path . Just <$> loadMUS path
loadResource font@(FontR name path mFont)
  | isJust mFont  = return font
  | otherwise     = FontR name path . Just <$> openFont path 32

finalizeResource :: Resource -> IO ()
finalizeResource (TextureR _ _ mTex)
  | isJust mTex   = maybe (return ()) freeTexture mTex
  | otherwise     = return ()
finalizeResource _   = return ()

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
        newRes <- foldM del res names
        atomicWriteIORef ref newRes
  where
      del res n = case M.lookup n res of
                       Nothing -> return res
                       Just r  -> do
                           finalizeResource r
                           return $ M.delete (resName r) res

withResource :: Resources -> String -> (Resource -> IO ()) -> IO ()
withResource resources name f = maybe (return ()) f =<< getResource resources name

withTexture :: Resources -> String -> (Tex -> IO ()) -> IO ()
withTexture r n f = withResource r n toTexture
  where
      toTexture (TextureR _ _ (Just t)) = f t
      toTexture (TextureR n p _ )       = putStrLn $ "Texture " ++ n ++ "` is not loaded. (" ++ p ++ ")"
      toTexture _                       = return ()


initResources :: FilePath -> IO Resources
initResources path = newResources 
  [ TextureR "logo" (inst "data/logo.png") Nothing
  , TextureR "bg"   (inst "data/bg.png") Nothing
  , TextureR "play" (inst "data/play.png") Nothing
  , TextureR "quit" (inst "data/quit.png") Nothing
  , LevelR "level1" (inst "levels/basic.lvl") Nothing
  ]
  where
     inst = (</>) path
