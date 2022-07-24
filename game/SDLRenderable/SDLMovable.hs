module SDLRenderable.SDLMovable where

import FRP.Rhine

import qualified SDL

import SDLRenderable.Type

import SDLRenderable.SDLRenderable

class Renderable a => Movable a where
  move :: MonadIO m => a -> m ()
  moveClSF :: MonadIO m => a -> SDL.Renderer m cl Vel ()
