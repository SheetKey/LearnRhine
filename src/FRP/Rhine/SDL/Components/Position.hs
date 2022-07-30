module FRP.Rhine.SDL.Components.Position
  ( Position
  , mkPosition
  ) where

import qualified SDL
import Foreign.C.Types


-- | Represents the points at which an object should be rendered and its size.
type Position = SDL.Rectangle CInt


mkPosition :: CInt ->  CInt -> CInt -> CInt -> Position
mkPosition x y w l = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w l)

-- | The x and y velocity of a object fow calculating its new position.
type Vel = (Double, Double)
