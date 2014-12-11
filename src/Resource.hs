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




data Resource = TextureR { resName :: String
                         , resPath :: FilePath
                         , resTexture     :: (Maybe Tex) }
              | LevelR { resName :: String
                       , resPath :: FilePath
                       , resLevel     :: (Maybe Level) }
              | MusicR { resName :: String
                       , resPath :: FilePath
                       , resMusic     :: (Maybe Music) }
              | FontR { resName :: String
                      , resPath :: FilePath
                      , resFont     :: (Maybe Font) } 

instance Show Resource where
  show (TextureR name path _) = name ++ " (texture:" ++ path ++ ")"
  show (LevelR name path _) = name ++ " (level:" ++ path ++ ")"
  show (MusicR name path _) = name ++ " (music:" ++ path ++ ")"
  show (FontR name path _) = name ++ " (font:" ++ path ++ ")"


isTextureR :: Resource -> Bool
isTextureR (TextureR _ _ _) = True
isTextureR _                = False

isLevelR :: Resource -> Bool
isLevelR (LevelR _ _ _) = True
isLevelR _                = False

isMusicR :: Resource -> Bool
isMusicR (MusicR _ _ _) = True
isMusicR _                = False

isFontR :: Resource -> Bool
isFontR (FontR _ _ _) = True
isFontR _                = False

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

data Resources = Resources (IORef (M.Map String Resource))

addResources :: Resources -> [Resource] -> IO ()
addResources (Resources ref) resources = do
    res    <- readIORef ref
    newRes <- foldM (\res' r -> do
                        add <- loadResource r
                        return $ M.insert (resName add) add
                    ) res resources
    atomicWriteIORef ref newRes
     
      
  


--getTexture :: Resources -> String -> IO (Maybe Tex)



  





{-
type LoadError = String

class Load a where
    loadR :: (MonadIO m) => String -> FilePath -> m (Either LoadError (Resource a))
    finalizeR :: (MonadIO m) => Resource a -> m ()jn
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
-}    
