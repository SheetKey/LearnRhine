module FRP.Rhine.SDL.Background where

import FRP.Rhine
import FRP.Rhine.SDL.Renderer.Type
import FRP.Rhine.SDL.Renderer.Renderable

import qualified SDL

-- | The background is a texture. 
newtype Background = Background (IO SDL.Texture)

instance Renderable Background where
  getTexture (Background tex) = liftIO tex
