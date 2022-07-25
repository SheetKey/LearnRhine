module FRP.Rhine.SDL.Renderer.Movable where

import FRP.Rhine
import FRP.Rhine.SDL.Renderer.Type
import FRP.Rhine.SDL.Renderer.Renderable

import qualified SDL


class Renderable a => Movable a where
  move :: MonadIO m => a -> m ()
  moveClSF :: MonadIO m => a -> ClSF m cl Vel ()
