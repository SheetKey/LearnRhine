module FRP.Rhine.SDL.Clock
  ( module X
  , FRP.Rhine.SDL.Clock
  ) where 

import FRP.Rhine.SDL.Clock.SDLClock as X
import FRP.Rhine.SDL.Clock.SDLQuitClock as X

import FRP.Rhine

type Second = Millisecond 1000
