{- |
The 'Position' component of an 'Entity' specifies the
location and size of the destination rectangle when calling
'SDL.copy'. The source rectangle is specified in the 'Sprite'
component.

This module also provides useful functions for working with
'SDL.Rectangle'.
-}

module FRP.Rhine.SDL.Components.Position
  ( Position (..)
  , Rectangle
  , Point
  , XPos
  , YPos
  , Width
  , Height
  , mkRectangle
  , modifyPosition
  , setFromPoint
  , mkPoint
  , CInt (..)
  ) where

import qualified SDL
import Foreign.C.Types


-- | Represents the points at which an object should be rendered and its size.
type Rectangle = SDL.Rectangle CInt

-- | The x position is a 'CInt'.
type XPos = CInt
-- | The y position is a 'CInt'.
type YPos = CInt
-- | The width is a 'CInt'.
type Width = CInt
-- | The height is a 'CInt'.
type Height = CInt

-- | The position component is an x and y position with a width and height.
data Position = Position XPos YPos Width Height

-- | A point is a tuple of 'Double's. This has an instance in the 'VectorSpace' typeclass.
type Point = (Double, Double)

-- | A utility for converting. 
mkRectangle :: Position -> Rectangle
mkRectangle (Position x y w l) = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w l)

-- | modifies the current position with a new point.
modifyPosition :: Point -> Position -> Position
modifyPosition (nx, ny) (Position x y w h) = Position (round nx + x) (round ny + y) w h

-- | Sets the new position with a new point.
setFromPoint :: Point -> Position -> Position
setFromPoint (nx, ny) (Position _ _ w h) = Position (round (nx)) (round (ny)) w h

-- | Converts the position into a point. 
mkPoint :: Position -> Point
mkPoint (Position x y _ _) = (fromIntegral x, fromIntegral y)
