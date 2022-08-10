module FRP.Rhine.SDL.Font where

import Control.Monad.IO.Class (MonadIO)
import Data.Text
import qualified SDL
import qualified SDL.Font as SDLF


loadFont :: MonadIO m
         -> FilePath
         -> SDLF.PointSize
         -> SDLF.Color
         -> Text
         -> m SDL.Surface
loadFont path size color text =
  bracket (SDLF.load path size) SDLF.free $
  \f -> SDLF.blended f color text

loadFontTexture :: MonadIO m
                => SDL.Renderer
                -> FilePath
                -> SDLF.PointSize
                -> SDLF.Color
                -> Text
                -> m SDL.Texture
loadFontTexture r path size color text =
  bracket (loadFont path size color text) SDL.freeSurface $
  SDL.createTextureFromSurface r
