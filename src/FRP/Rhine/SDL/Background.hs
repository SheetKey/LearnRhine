module FRP.Rhine.SDL.Background where

import FRP.Rhine
import FRP.Rhine.SDL.Renderer.Type
import FRP.Rhine.SDL.Renderer.Renderable

import qualified SDL

-- | The background is a texture. 
type Background = SDL.Texture

instance Renderable Background where
  renderClSF bg ren = proc _ -> do
    constMCl (SDL.copy ren bg Nothing Nothing) -< ()
            
