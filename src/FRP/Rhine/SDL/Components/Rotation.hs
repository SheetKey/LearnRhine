{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module FRP.Rhine.SDL.Components.Rotation
  ( module FRP.Rhine.SDL.Components.Rotation
  , CDouble (..)
  ) where

import FRP.Rhine.SDL.Components.Position

import GHC.Generics (Generic)
import Data.Generics.Product.Fields

import qualified SDL

import Foreign.C.Types

data Rotation = Rotation
  { towardsMouse :: Bool
  , angle :: CDouble
  , rotPoint :: Maybe (SDL.Point (SDL.V2 CInt))
  }
  deriving (Generic)

setTowardsMouse :: Rotation -> Bool -> Rotation
setTowardsMouse rot val = setField @"towardsMouse" val rot
  
setAngle :: Rotation -> CDouble -> Rotation
setAngle rot val = setField @"angle" val rot

setRotPoint :: Rotation -> Maybe (XPos, YPos) -> Rotation
setRotPoint rot val = setField @"rotPoint" val rot
