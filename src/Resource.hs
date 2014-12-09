{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Resource
  ( LoadError
  , Image
  , Load(..)
  , Resource(..)
  , mkResource
  , isLoaded
  , loadResource
  , ResourceMgr(..)
  , mkResourceMgr
  , resMgrLoadAll
  , Manage(..)
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative


import System.IO.Error (catchIOError)

import Data.Maybe 
import qualified Data.Map as M

import Graphics.UI.SDL.Mixer (Music)
import Graphics.UI.SDL.Mixer.Music (loadMUS)

import Graphics.UI.SDL.TTF

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



data ResourceMgr = ResourceMgr { levels :: M.Map String (Resource Level)
                                       , textures :: M.Map String (Resource Tex)
                                       , musics :: M.Map String (Resource Music)
                                       , fonts  :: M.Map String (Resource Font) }

mkResourceMgr :: ResourceMgr
mkResourceMgr = ResourceMgr M.empty M.empty M.empty M.empty



resMgrLoadAll :: ResourceMgr -> IO (ResourceMgr, [LoadError])
resMgrLoadAll mgr = loadAll textures =<< loadAll levels =<< loadAll musics =<< loadAll fonts (mgr, [])
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

resMgrUnload :: [String] -> ResourceMgr -> IO ResourceMgr
resMgrUnload names mgr = foldM (\mgr' -> 
  case getResource name mgr' of
   


class (Load a) => Manage a where
    addR :: ResourceMgr -> Resource a -> ResourceMgr
    getResource :: String -> ResourceMgr -> Maybe (Resource a)

    getR :: String -> ResourceMgr -> IO (Maybe a)
    getR name mgr = 


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
 

basicGet :: (Manage a) => (ResourceMgr -> M.Map String (Resource a)) -> String -> ResourceMgr -> Maybe (Resource a)
basicGet f name mgr = M.lookup name (f mgr)



