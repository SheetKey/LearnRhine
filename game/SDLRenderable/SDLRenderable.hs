{-# LANGUAGE TypeFamilies #-}

module SDLRenderable.SDLRenderable where

import FRP.Rhine

import qualified SDL

import Data.Word

class Renderable a where
  -- | The type that should be rendered.
  type RenderWhat a

  -- | The value that should be rendered.
  --   Often a filepath for an image or a color.
  renderWhat :: RenderWhat a

  render :: MonadIO m => SDL.Renderer -> m ()

  renderClSF :: MonadIO m => SDL.Renderer -> ClSF m cl () ()
  renderClSF ren = constMCl $ render ren
