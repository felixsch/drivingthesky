module Resource
  ( loadShader
  , getShader
  ) where

import Control.Lens
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import System.FilePath.Posix ((</>))
import Control.Exception

import Graphics.GLUtil.ShaderProgram

import Types
import Util

loadShader :: String -> Runtime ()
loadShader name = do
  path <- view runtimePath <$> get
  shader <- liftIO (try $ loadShaderFamily (path </> name) :: IO (Either IOError ShaderProgram))
  case shader of
    Left e -> fatal ("Could not load shader: " ++ show e)
    Right program -> shaders . at name ?= program


getShader :: String -> Runtime (Maybe ShaderProgram)
getShader name = use $ shaders . at name

