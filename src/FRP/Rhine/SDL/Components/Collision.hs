{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module FRP.Rhine.SDL.Components.Collision where

import FRP.Rhine

import FRP.Rhine.SDL.Components.Position

import GHC.Generics (Generic)
import Data.Generics.Product.Fields


data Collision a = Collision
  { hitBox :: (Width, Height)     -- ^ The width and height of the hitbox. 
  , canHit :: Bool                -- ^ Whether or not an entity can colide. 
  , hitOther :: a -> a            -- ^ What this entity does to the entity it collides with.
  , deactivate :: Bool       -- ^ Whether or not to set self inactive on collision. (Used for entities such as bullets.)
  , collisionCounter :: Maybe Int -- ^ An entity with collisions might need to track the number of collisions. (Used for bullets that have some amount of peircing.)
  }
  deriving (Generic)

setHitBox :: Collision a -> (Width, Height) -> Collision a
setHitBox c val = setField @"hitBox" val c

setCanHit :: Collision a -> Bool -> Collision a
setCanHit c val = setField @"canHit" val c

setHitOther :: Collision a -> (a -> a) -> Collision a
setHitOther c f = setField @"hitOther" f c

setDeactivate :: Collision a -> Bool -> Collision a
setDeactivate c f = setField @"deactivate" f c

setCollisionCounter :: Collision a -> Maybe Int -> Collision a
setCollisionCounter c f = setField @"collisionCounter" f c

incCollisionCounter :: Collision a -> Collision a
incCollisionCounter c = setCollisionCounter c $ fmap (+ 1) $ collicionCounter c
