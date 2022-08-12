module FRP.Rhine.SDL.Font where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (bracket)
import Data.Text
import qualified SDL
import qualified SDL.Font as SDLF


handle = (\e -> do
             let str = show (e :: SDL.SDLException)
             putStrLn str
             exitFailure)

loadFont :: MonadIO m
         => FilePath
         -> SDLF.PointSize
         -> SDLF.Color
         -> Text
         -> m SDL.Surface
loadFont path size color text =
  liftIO . bracket (SDLF.load path size) SDLF.free $
  \f -> SDLF.blended f color text

loadFontTexture :: MonadIO m
                => SDL.Renderer
                -> FilePath
                -> SDLF.PointSize
                -> SDLF.Color
                -> Text
                -> m SDL.Texture
loadFontTexture r path size color text =
  liftIO . bracket (loadFont path size color text) SDL.freeSurface $
  SDL.createTextureFromSurface r
  
