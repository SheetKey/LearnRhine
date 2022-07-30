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
  { getTexture :: Maybe (IO SDL.Texture)
  , getPosition :: Maybe Position
  }

defaultEntity :: Entity
defaultEntity = Entity
                Nothing
                Nothing
                
mapTexture :: Entity -> (Maybe (IO SDL.Texture) -> Maybe (IO SDL.Texture)) -> Entity
mapTexture (Entity {..}) f = Entity (f getTexture) getPosition

mapPosition :: Entity -> (Maybe Position -> Maybe Position) -> Entity
mapPosition (Entity {..}) f = Entity getTexture (f getPosition)
