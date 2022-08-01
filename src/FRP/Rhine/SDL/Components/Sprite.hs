{-# LANGUAGE RecordWildCards #-}

module FRP.Rhine.SDL.Components.Sprite
  (
  ) where

import qualified SDL
import qualified SDL.Image as SDLI

import FRP.Rhine.SDL.Components.Position

data Sprite = Sprite
  { index :: CInt
  , maxFrames :: CInt
  , frameIndex :: CInt
  , width :: Width
  , height :: Height
  }


spriteRect :: Sprite -> Rectangle
spriteRect (Sprite {..}) = mkRectangle $ Position (frameIndex * width)
                                                  (index * height)
                                                  size
                                                  size
  
