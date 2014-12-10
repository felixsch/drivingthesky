{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Resource
  ( LoadError
  , Load(..)
  , Resource(..)
  , mkResource
  , isLoaded
  , loadResource
  , Resources(..)
  , mkResources
  , Manage(..)
  , initResources
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative

import System.FilePath.Posix
import System.IO.Error (catchIOError)

import Data.IORef
import Data.Maybe 
import qualified Data.Map as M

import Graphics.UI.SDL.Mixer (Music)
import Graphics.UI.SDL.Mixer.Music (loadMUS)

import Graphics.UI.SDL.TTF

import Paths_drivingthesky
import Texture
import Level


type LoadError = String

class Load a where
    loadR :: (MonadIO m) => String -> FilePath -> m (Either LoadError (Resource a))
    finalizeR :: (MonadIO m) => Resource a -> m ()
    finalizeR _ = return ()



data (Load a) => Resource a = Resource { resName :: String
                           , resFP   :: FilePath
                           , res     :: Maybe a }

mkResource :: (Load a) => String -> FilePath -> Resource a
mkResource name path = Resource name path Nothing

isLoaded :: (Load a) => Resource a -> Bool
isLoaded = isJust . res

loadResource :: (MonadIO m, Load a) => Resource a -> m (Either LoadError (Resource a))
loadResource r@(Resource name path _) = if isLoaded r
                                            then return $ Right r
                                            else loadR name path


instance Load Tex where
    loadR name path = liftIO $
      catchIOError (Right . Resource name path . Just <$> loadTexture path)
                   (\e -> return (Left $ show e))
    finalizeR = liftIO . maybe (return ()) freeTexture . res

instance Load Music where
    loadR name path = liftIO $
      catchIOError (Right . Resource name path . Just <$> loadMUS path)
                   (\e -> return (Left $ show e))

instance Load Font where
    loadR name path = liftIO $
      catchIOError (Right . Resource name path . Just <$> openFont path 32)
                   (\e -> return (Left $ show e))


instance Load Level where
    loadR name path = liftIO $ 
      catchIOError (Right . Resource name path . Just <$> loadLevel path)
                   (\e -> return (Left $ show e))



data Resources = Resources { levels :: M.Map String (Resource Level)
                                       , textures :: M.Map String (Resource Tex)
                                       , musics :: M.Map String (Resource Music)
                                       , fonts  :: M.Map String (Resource Font) }

mkResources :: Resources
mkResources = Resources M.empty M.empty M.empty M.empty



resManagerLoadAll :: Resources -> IO (Resources, [LoadError])
resManagerLoadAll mgr = loadAll textures =<< loadAll levels =<< loadAll musics =<< loadAll fonts (mgr, [])
  where
    loadAll f (mgr', err) = foldM (\(m, errors) r -> 
        if isLoaded r
            then return (m, errors)
            else do
                result <- loadResource r
                case result of
                    Left e -> return (m, e : errors)
                    Right lr -> return (addR m lr, errors)
        ) (mgr', err) (M.elems $ f mgr')



class (Load a) => Manage a where
    addR :: Resources -> Resource a -> Resources
    getR :: Resources -> String -> IO (Maybe a)
    

instance Manage Level where
    addR mgr r = mgr { levels = M.insert (resName r) r (levels mgr)}
    getR = basicGet levels

instance Manage Tex where
    addR mgr r = mgr { textures = M.insert (resName r) r (textures mgr)}
    getR = basicGet textures

instance Manage Music where
    addR mgr r = mgr { musics = M.insert (resName r) r (musics mgr)}
    getR = basicGet musics

instance Manage Font where
    addR mgr r = mgr { fonts = M.insert (resName r) r (fonts mgr)}
    getR = basicGet fonts
 

basicGet :: (Manage a) => (Resources -> M.Map String (Resource a)) -> Resources -> String -> IO (Maybe a)
basicGet f mgr name = case M.lookup name (f mgr) of
                           Nothing -> return Nothing
                           Just r  -> if isLoaded r
                                         then return $ res r
                                         else fromEither <$> loadResource r
    where
        fromEither (Left _)  = Nothing
        fromEither (Right r) = res r


initResources :: IO (IORef Resources)
initResources = do
    path <- getDataDir
    (res, errors) <- resManagerLoadAll $ basicResources path
    unless (null errors) $ error (unlines errors)
    newIORef res
  where
      basicResources path = Resources
        { textures = M.fromList 
          [ "logo" +> (path </> "data/logo.png")
          , "bg"   +> (path </> "data/bg.png")
          , "play" +> (path </> "data/play.png")
          , "quit" +> (path </> "data/quit.png")
          ]
        , levels = M.fromList 
          [ "1" +> (path </> "levels/basic.lvl")
          ]
        , musics = M.empty
        , fonts  = M.empty
        }

(+>) :: (Load a) => String -> FilePath -> (String, Resource a)
(+>) name path = (name, mkResource name path)
    
