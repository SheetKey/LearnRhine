module SDLRenderable.Type where

import qualified SDL
import Foreign.C.Types

type Point = SDL.Point SDL.V2 CInt

type Position = SDL.Rectangle CInt

type Vel = (Double, Double)
