module FRP.Rhine.SDL.Components.Position
  ( Position
  , Rectangle
  , XPos
  , YPose
  , Width
  , Length
  , mkPosition
  ) where

import qualified SDL
import Foreign.C.Types


-- | Represents the points at which an object should be rendered and its size.
type Rectangle = SDL.Rectangle CInt

type XPos = CInt
type YPos = CInt
type Width = CInt
type Length = CInt

newtype Position = Position XPose YPos Width Length

mkRectangle :: Position -> Rectangle
mkRectangle (Position x y w l) = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w l)


