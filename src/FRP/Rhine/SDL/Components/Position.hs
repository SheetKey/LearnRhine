module FRP.Rhine.SDL.Components.Position
  ( Position (..)
  , Rectangle
  , Point
  , Velocity
  , XPos
  , YPos
  , Width
  , Height
  , mkRectangle
  , updatePosition
  , setFromPoint
  , CInt (..)
  ) where

import qualified SDL
import Foreign.C.Types


-- | Represents the points at which an object should be rendered and its size.
type Rectangle = SDL.Rectangle CInt

type XPos = CInt
type YPos = CInt
type Width = CInt
type Height = CInt

data Position = Position XPos YPos Width Height

type Point = (Double, Double)
type Velocity = (Double, Double)

mkRectangle :: Position -> Rectangle
mkRectangle (Position x y w l) = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w l)

updatePosition :: Point -> Position -> Position
updatePosition (nx, ny) (Position x y w h) = Position (round nx + x) (round ny + y) w h

setFromPoint :: Point -> Position -> Position
setFromPoint (nx, ny) (Position _ _ w h) = Position (round (100 * nx)) (round (100 * ny)) w h
