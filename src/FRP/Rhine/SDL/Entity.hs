{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module FRP.Rhine.SDL.Entity
  ( Entity
  , getTexture
  , getPosition
  , defaultEntity
  , mapTexture
  , mapPosition
  ) where

import FRP.Rhine

import FRP.Rhine.SDL.Components

import qualified SDL
import qualified SDL.Image as SDLI


data Entity = Entity
  { isActive :: Bool
  , getTexture :: Maybe (IO SDL.Texture)
  , getPosition :: Maybe Position
  }

defaultEntity :: Entity
defaultEntity = Entity
                True
                Nothing
                Nothing

mapIsActive :: Entity -> (Bool -> Bool) -> Entity
mapIsActive (Entity {..}) f = Entity (f isActive) getTexture getPosition

setIsActive :: Entity -> Bool -> Entity
setIsActive (Entity {..}) b = Entity b getTexture getPosition
                
mapTexture :: Entity -> (Maybe (IO SDL.Texture) -> Maybe (IO SDL.Texture)) -> Entity
mapTexture (Entity {..}) f = Entity isActive (f getTexture) getPosition

setTexture :: Entity -> Maybe (IO SDL.Texture) -> Entity
setTexture (Entity {..}) tex = Entity isActive tex getPosition

mapPosition :: Entity -> (Maybe Position -> Maybe Position) -> Entity
mapPosition (Entity {..}) f = Entity isActive getTexture (f getPosition)

setPosition :: Entity -> Maybe Position -> Entity
setPosition (Entity {..}) pos = Entity isActive getTexture pos
