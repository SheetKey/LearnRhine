{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module FRP.Rhine.SDL.Components.Sprite where

import qualified SDL
import qualified SDL.Image as SDLI

import FRP.Rhine.SDL.Components.Position

import GHC.Generics (Generic)
import Data.Generics.Product.Fields

data Sprite = Sprite
  { index :: CInt
  , maxFrameIndex :: CInt
  , frameIndex :: CInt
  , width :: Width
  , height :: Height
  }
  deriving (Generic)


spriteRect :: Sprite -> Rectangle
spriteRect (Sprite {..}) = mkRectangle $ Position (frameIndex * width)
                                                  (index * height)
                                                  width
                                                  height
  
setFrameIndex :: Sprite -> CInt -> Sprite
setFrameIndex sp i = setField @"frameIndex" i sp

incFrameIndex :: Sprite -> Sprite
incFrameIndex sp = if ind < max
                   then setFrameIndex sp (ind + 1)
                   else setFrameIndex sp 0
  where max = maxFrameIndex sp
        ind = frameIndex sp
