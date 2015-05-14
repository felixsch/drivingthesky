{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}

module Types
  ( GLf
  , Alpha
  , AABB(..)
      
  -- Runtime   
  , Runtime(..)
  , RuntimeError(..)
  , Ref
  , run
  , get, put, modify

  -- RuntimeData
  , Data(..)
  , glfwWindow, runtimePath, shaders, currentRoad
  , mkRuntimeData

  -- Entity & Object
  , Entity(..)
  , Object(..)

  -- Block
  , Block(..)
  , BlockColor
  , BlockHeight
  , BlockAlpha

  -- Road
  , Road(..)
  , RoadDefinition(..)
  , blocks, definition

  -- Renderable
  , Renderable(..)

  -- OpenGL
  , GL.Vector3, GL.Vertex3

  , liftIO
 
  ) where



import Control.Lens
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Data.Monoid

-- Wires
import Control.Wire.Core

-- Error Handling
import Control.Monad.Error
import Control.Exception
import System.IO.Error

-- TVar
import Control.Monad.STM
import Control.Concurrent.STM.TVar

-- Graphics
import System.FilePath.Posix
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.GLUtil.ShaderProgram

-- Containers
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.List as L

-- Basic Types

type GLf = GL.GLfloat

data AABB = AABB { pMin :: GL.Vertex3 GLf
                 , pMax :: GL.Vertex3 GLf }
        deriving (Show)

type Alpha = GLf


-- Object & Entity


data Object a = Object { _position :: GL.Vector3 GLf
                                     , _velocity :: GL.Vector3 GLf
                                     , _object   :: a }

makeLenses ''Object
-- Block

type BlockColor  = String
type BlockHeight = GLf
type BlockAlpha  = GLf


data Block  = Start BlockColor BlockHeight
            | Block BlockColor BlockHeight
            | EmptyBlock
            | Goal BlockColor BlockHeight
            deriving (Show, Read, Eq)

-- Road

data RoadDefinition = RoadDefinition 
  { roadName   :: String
  , roadBlocks :: M.Map Int Block
  , roadDef    :: S.Seq [Int]
  } deriving (Show, Read, Eq)

data Road = Road { _blocks     :: S.Seq [Object Block]
                 , _definition :: RoadDefinition }

makeLenses ''Road
-- RuntimeData

data Data = Data { _glfwWindow  :: GLFW.Window
                 , _currentRoad :: Maybe Road
                 , _runtimePath :: FilePath
                 , _shaders     :: M.Map String ShaderProgram }
makeLenses ''Data

mkRuntimeData :: GLFW.Window -> FilePath -> IO Ref
mkRuntimeData win path = newTVarIO $ Data win Nothing path M.empty


-- Runtime 

data RuntimeError = Fatal String
              | Warning String
              | Info String

instance Error RuntimeError where
  strMsg = Info

type Ref = TVar Data

newtype Runtime a = Runtime { runRuntime :: ReaderT Ref (ErrorT RuntimeError IO) a}
  deriving (Functor, Monad, MonadIO, MonadReader Ref, MonadError RuntimeError)

instance Applicative Runtime where
    pure  = return
    (<*>) = ap

instance (Monoid a) => Monoid (Runtime a) where
  mempty = return mempty
  mappend = liftM2 mappend

instance MonadState Data Runtime where
  get   = liftIO . readTVarIO =<< ask
  put d = do
    ref <- ask
    liftIO $ atomically $ writeTVar ref d

run :: Ref -> Runtime a -> IO (Either RuntimeError a)
run r (Runtime f) = runErrorT $ runReaderT f r


class Entity a where
    wire :: (Monoid e) => Wire s e Runtime () (Object a)
    aabb :: Object a -> AABB
    canCollide :: Wire s e Runtime (Object a) Bool
    canCollide = mkConst (Right False)
    collide :: Wire s e Runtime (Object b, Object a) (Object a)

-- Renderable
class Renderable a where
    render :: Object a -> Runtime () 
