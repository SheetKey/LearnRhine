module FPR.Rhine.SDL.Entity where

import qualified SDL
import qualified SDL.Image as SDLI

data Entity = Player

getTexture :: MonadIO m => SDL.Renderer -> Entity -> m SDL.Texture
getTexture ren Player = loadTexture ren "sprites/Sprite-0001.png"
