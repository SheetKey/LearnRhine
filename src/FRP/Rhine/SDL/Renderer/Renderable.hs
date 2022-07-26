module FRP.Rhine.SDL.Renderer.Renderable where

import FRP.Rhine
import FRP.Rhine.SDL.Renderer.Type

import qualified SDL

import Foreign.C.Types

-- | A type class for all types that can be rendered to the screen.
-- It is expected that 'a' has a texture or a means of getting a texture.
class Renderable a where
  renderClSF :: MonadIO m => a -> SDL.Renderer -> ClSF m cl Point ()
