{- |
Each component specifies a property than could belong to any given entity.
This is based on the Entity Component System (ECS) design principle.
-}

module FRP.Rhine.SDL.Components
  ( module X
  , CInt (..)
  ) where

import Foreign.C.Types

import FRP.Rhine.SDL.Components.Position as X
import FRP.Rhine.SDL.Components.Sprite as X
import FRP.Rhine.SDL.Components.Velocity as X
import FRP.Rhine.SDL.Components.Collision as X
import FRP.Rhine.SDL.Components.Rotation as X
