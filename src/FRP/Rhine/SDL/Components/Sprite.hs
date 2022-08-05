{- |
The 'Sprite' component for an 'Entity' specifies
whether or not an 'Entity's texture is an animation.

This module also provides functions for working with sprites.
-}

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

-- | The sprite component of an entity.
data Sprite = Sprite
  { index :: CInt         -- ^ The vertical index.
  , maxFrameIndex :: CInt -- ^ The max number of frames in the animation. (Starts with 0 not 1.)
  , frameIndex :: CInt    -- ^ The current horizontal frame index.
  , width :: Width        -- ^ The pixel width of a frame.
  , height :: Height      -- ^ The pixel height of a frame.
  }
  deriving (Generic)

-- | A function to get the source rectangle from a sprite than should be used
--   with 'SDL.copy'.
spriteRect :: Sprite -> Rectangle
spriteRect (Sprite {..}) = mkRectangle $ Position (frameIndex * width)
                                                  (index * height)
                                                  width
                                                  height

-- | Sets the frame index.
setFrameIndex :: Sprite -> CInt -> Sprite
setFrameIndex sp i = setField @"frameIndex" i sp

-- | Increases the frame index by 1. If at max, resets to 0.
incFrameIndex :: Sprite -> Sprite
incFrameIndex sp = if ind < max
                   then setFrameIndex sp (ind + 1)
                   else setFrameIndex sp 0
  where max = maxFrameIndex sp
        ind = frameIndex sp
