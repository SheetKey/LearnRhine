module FRP.Rhine.SDL.Renderer.Type where

import qualified SDL
import Foreign.C.Types

-- | Represents the point at which an object should be rendered.
type Point = SDL.Point SDL.V2 CInt

-- | Represents the points at which an object should be rendered and its size.
type Position = SDL.Rectangle CInt

-- | The x and y velocity of a object fow calculating its new position.
type Vel = (Double, Double)

