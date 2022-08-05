{- |
The entire game state can be broken down into entities with optional properties.
This allws for a more extensible game as everthing from the background to the player
to UI elements is implemented as an entity. If a new property (component) is added,
it is automatically accesible to all entities, not just a specific game object such
as player or enemy. 
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module FRP.Rhine.SDL.Entity
  ( Entity
  , isActive
  , isPlayer
  , getTexture
  , getPosition
  , getSprite
  , defaultEntity
  , setIsActive
  , setIsPlayer
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

-- | An entity is a collection of necessary and optional properties.
data Entity = Entity
  { isActive :: Bool                     -- ^ An entity either is or is not active.
  , isPlayer :: Bool                     -- ^ An entity either is or is not the player.
  , getTexture :: Maybe (IO SDL.Texture) -- ^ An entity might have a texture.
  , getPosition :: Maybe Position        -- ^ An entity might have a position.
  , getSprite :: Maybe Sprite            -- ^ An entity might have a sprite (animation).
  }
  deriving (Generic)

-- | The entity data constructor is not exported. Instead it is recommented
--   to modify the defaultEntity when creating a new entity.
defaultEntity :: Entity
defaultEntity = Entity
                True
                False
                Nothing
                Nothing
                Nothing

-- | Set whether or not an entity is active.
setIsActive :: Entity -> Bool -> Entity
setIsActive ent val = setField @"isActive" val ent

-- | Set whether or not an entity is the player.
setIsPlayer :: Entity -> Bool -> Entity
setIsPlayer ent val = setField @"isPlayer" val ent

-- | Set the texture.
setTexture :: Entity -> Maybe (IO SDL.Texture) -> Entity
setTexture ent val = setField @"getTexture" val ent

-- | Set the position.
setPosition :: Entity -> Maybe Position -> Entity
setPosition ent val = setField @"getPosition" val ent

-- | Set the sprite.
setSprite :: Entity -> Maybe Sprite -> Entity
setSprite ent val = setField @"getSprite" val ent
