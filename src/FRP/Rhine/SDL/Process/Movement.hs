module FRP.Rhine.SDL.Process.Movement where

import FRP.Rhine
import FRP.Rhine.SDL.Util

import qualified SDL

import FRP.Rhine.SDL.Components.Position

processInput :: ClSF m cl SDL.Event Velocity
processInput = feedback (0,0) $ proc (event, vel@(x,y)) ->
                                  case SDL.eventPayload event of
                                    SDL.KeyboardEvent kEvent ->
                                      case SDL.keysymKeycode (SDL.keyboardEventKeysym kEvent ) of
                                        SDL.KeycodeW -> if isPressed kEvent
                                                        then returnA -< ((x, (-5)), (x, (-5)))
                                                        else returnA -< ((x, 0), (x, 0))
                                        SDL.KeycodeA -> if isPressed kEvent
                                                        then returnA -< ((5, y), (5, y))
                                                        else returnA -< ((0, y), (0, y))
                                        SDL.KeycodeS -> if isPressed kEvent
                                                        then returnA -< ((x, 5), (x, 5)) 
                                                        else returnA -< ((x, 0), (x, 0))
                                        SDL.KeycodeD -> if isPressed kEvent
                                                        then returnA -< (((-5), y), ((-5), y))
                                                        else returnA -< ((0, y), (0, y))
                                        _ -> returnA -< (vel, vel)
                                    _ -> returnA -< (vel, vel)

getPoint :: MonadIO m => ClSF m SDLClock () Point
getPoint = tagS >>> processInput >>> arr normalize >>> integral
