module FRP.Rhine.SDL.Components.Position
  ( Position (..)
  , Rectangle
  , XPos
  , YPos
  , Width
  , Height
  , mkRectangle
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

mkRectangle :: Position -> Rectangle
mkRectangle (Position x y w l) = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w l)


