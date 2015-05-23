module Resource
  ( loadShader
  , getShader
  ) where

import Control.Lens
import Control.Applicative
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.FilePath.Posix ((</>))
import Control.Exception

import Graphics.GLUtil.ShaderProgram

import Types
import Util


fromMaybeM :: (MonadIO m) => m a -> Maybe a -> m a
fromMaybeM _ (Just a) = return a
fromMaybeM f _        = f

loadShader :: String -> Runtime ()
loadShader name = do
  path <- view runtimePath <$> get
  shader <- liftIO (try $ loadShaderFamily (path </> name) :: IO (Either IOError ShaderProgram))
  case shader of
    Left e -> fatal ("Could not load shader: " ++ show e)
    Right program -> shaders . at name ?= program


getShader :: String -> Runtime ShaderProgram
getShader name = fromMaybeM failed =<< shader
  where
    failed = fatal $ "Could not get shader " ++ name ++ ": No such file."
    shader = use $ shaders . at name



