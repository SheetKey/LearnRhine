module FRP.Rhine.SDL.Components.Collicion where

import FRP.Rhine

import FRP.Rhine.SDL.Components.Position

import GHC.Generics (Generic)
import Data.Generics.Product.Fields


data Collision = Collision
  { hitBox :: (Width, Height)    -- ^ The width and height of the hitbox. 
  , canHit :: Bool               -- ^ Whether or not an entity can colide. 
  , hitOther :: Entity -> Entity -- ^ What this entity does to the entity it collides with.
  }
  deriving (Generic)

setHitBox :: Collision -> (Width, Height) -> Collision
setHitBox c val = setField @"hitBox" val c

setCanHit :: Collision -> Bool -> Collision
setCanHit c val = setField @"canHit" val c

setHitOther :: Collision -> (Entity -> Entity) -> Collision
setHitOther c f = setField @"hitOther" f c
