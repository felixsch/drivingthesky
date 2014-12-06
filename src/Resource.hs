
module Resource where
   
import Control.Applicative

import System.IO.Error (catchIOError)

import Data.Maybe 

import Graphics.UI.SDL (Surface)
import Graphics.UI.SDL.Image (load)

import Graphics.UI.SDL.Mixer (Music)
import Graphics.UI.SDL.Mixer.Music (loadMUS)



class Resource s where
    loadR :: String -> FilePath -> IO (Either String s)
    isLoaded :: s -> Bool
    saveR :: s -> IO Bool
    freeR :: s -> IO ()


data ImageR = ImageR { imageName    :: String
                     , image :: Maybe Surface }

data MusicR = MusicR { musicName   :: String
                     , music  :: Maybe Music }

data FontR = FontR { fontName :: String
                   , font :: Font }





instance Resource ImageR where
    loadR name path = catchIOError (Right <$> (ImageR name . Just <$> load path)) (\e -> return (Left $ show e))
    saveR _    = return False -- Do not allow image saving
    freeR _    = id

instance Resource MusicR where
    loadR name path = catchIOError (Right <$> (MusicR name . Just <$> loadMUS path)) (\e -> return (Left $ show e))
    isLoaded   = isJust . musicStream
    saveR _    = return False

instance Resource FontR where
    loadR name path = catchIOError (Right <$> (FontR name . Just <$> load





    

