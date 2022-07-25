module FRP.Rhine.SDL.Renderer.Renderable where

import FRP.Rhine

import qualified SDL

import Foreign.C.Types

import SDLRenderable.Type

class Renderable a where
  getTexture :: MonadIO m => a -> SDL.Renderer -> m SDL.Texture
  --getTexturClSF :: MonadIO m => a -> ClSF m cl () SDL.Texture

  --render :: MonadIO m => a -> SDL.Renderer -> m ()
  renderClSF :: MonadIO m => a -> SDL.Renderer -> ClSF m cl Point ()
