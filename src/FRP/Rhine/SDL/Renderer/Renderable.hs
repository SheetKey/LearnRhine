{-# LANGUAGE Arrows #-}

module FRP.Rhine.SDL.Renderer.Renderable where

import FRP.Rhine
import FRP.Rhine.SDL.Components

import qualified SDL

import Foreign.C.Types

-- | A type class for all types that can be rendered to the screen.
-- It is expected that 'a' has a texture or a means of getting a texture.
class Renderable a where
  getTexture :: MonadIO m => a -> m SDL.Texture
  renderClSF :: MonadIO m => a -> SDL.Renderer -> ClSF m cl (Maybe Position) ()
  renderClSF a ren = arrMCl  
                     (\mpos -> liftIO $ do
                         tex <- getTexture a
                         SDL.copy ren tex Nothing mpos)
  {-# MINIMAL getTexture #-}
