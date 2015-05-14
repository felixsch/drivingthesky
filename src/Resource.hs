module Resource
  ( loadRoad

  , loadShader
  , getShader
  ) where

import Control.Lens
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import System.FilePath.Posix ((</>))
import Control.Exception

import Graphics.GLUtil.ShaderProgram

import Types
import Road
import Util

loadRoad :: String -> Runtime ()
loadRoad name = do
  path <- view runtimePath <$> get
  def <- liftIO (try $ readFile (path </> "roads" </> name) :: IO (Either IOError String))
  case def of
    Left _ -> fatal ("Could not load road (road =" ++ path </> "roads" </> name ++ ")")
    Right d -> currentRoad .= (mkRoad $ read d)


loadShader :: String -> Runtime ()
loadShader name = do
  path <- view runtimePath <$> get
  shader <- liftIO (try $ loadShaderFamily (path </> name) :: IO (Either IOError ShaderProgram))
  case shader of
    Left e -> fatal ("Could not load shader: " ++ show e)
    Right program -> shaders . at name ?= program


getShader :: String -> Runtime (Maybe ShaderProgram)
getShader name = use $ shaders . at name

