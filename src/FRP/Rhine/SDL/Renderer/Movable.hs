module FRP.Rhine.SDL.Renderer.Movable where

import FRP.Rhine
import FRP.Rhine.SDL.Components
import FRP.Rhine.SDL.Renderer.Renderable

import qualified SDL

-- | A type class for all renderable types that can move on the screen.
class Renderable a => Movable a where
  moveClSF :: Monad m => a -> ClSF m cl Vel Position
