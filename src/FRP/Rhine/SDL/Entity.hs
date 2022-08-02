{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module FRP.Rhine.SDL.Entity
  ( Entity
  , isActive
  , getTexture
  , getPosition
  , getSprite
  , defaultEntity
  , setIsActive
  , setTexture
  , setPosition
  , setSprite
  ) where

import FRP.Rhine

import FRP.Rhine.SDL.Components

import qualified SDL
import qualified SDL.Image as SDLI

import GHC.Generics (Generic)
import Data.Generics.Product.Fields


data Entity = Entity
  { isActive :: Bool
  , getTexture :: Maybe (IO SDL.Texture)
  , getPosition :: Maybe Position
  , getSprite :: Maybe Sprite
  }
  deriving (Generic)

defaultEntity :: Entity
defaultEntity = Entity
                True
                Nothing
                Nothing
                Nothing

setIsActive :: Entity -> Bool -> Entity
setIsActive ent val = setField @"isActive" val ent

setTexture :: Entity -> Maybe (IO SDL.Texture) -> Entity
setTexture ent val = setField @"getTexture" val ent

setPosition :: Entity -> Maybe Position -> Entity
setPosition ent val = setField @"getPosition" val ent

setSprite :: Entity -> Maybe Sprite -> Entity
setSprite ent val = setField @"getSprite" val ent
