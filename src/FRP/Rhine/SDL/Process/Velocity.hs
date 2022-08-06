module FRP.Rhine.SDL.Process.Velocity where

import FRP.Rhine

import FRP.Rhine.SDL.Entity
import FRP.Rhine.SDL.Components

processInput :: Monad m => ClSF m cl (Maybe SDL.Event) Velocity
processInput = feedback (0,0) $ proc (mevent, vel@(x,y)) ->
                                  case mevent of
                                    Nothing -> returnA -< (vel, vel)
                                    Just event -> 
                                      case SDL.eventPayload event of
                                        SDL.KeyboardEvent kEvent ->
                                          case SDL.keysymKeycode (SDL.keyboardEventKeysym kEvent ) of
                                            SDL.KeycodeW -> if isPressed kEvent
                                                            then returnA -< ((x, (-1)), (x, (-1)))
                                                            else if y == (-1)
                                                                 then returnA -< ((x, 0), (x, 0))
                                                                 else returnA -< (vel, vel)
                                            SDL.KeycodeA -> if isPressed kEvent
                                                            then returnA -< (((-1), y), ((-1), y))
                                                            else if x == (-1)
                                                                 then returnA -< ((0, y), (0, y))
                                                                 else returnA -< (vel, vel)
                                            SDL.KeycodeS -> if isPressed kEvent
                                                            then returnA -< ((x, 1), (x, 1)) 
                                                            else if y == 1
                                                                 then returnA -< ((x, 0), (x, 0))
                                                                 else returnA -< (vel, vel)
                                            SDL.KeycodeD -> if isPressed kEvent
                                                            then returnA -< ((1, y), (1, y))
                                                            else if x == 1
                                                                 then returnA -< ((0, y), (0, y))
                                                                 else returnA -< (vel, vel)
                                            _ -> returnA -< (vel, vel)
                                        _ -> returnA -< (vel, vel)

getPlayerVelocity :: Rhine IO (SequentialClock IO SDLClock Busy) () Velocity
getPlayerVelocity = pollEvent @@ SDLClock
           >-- fifoUnbounded -@- concurrently -->
           (processInput >>> arr normalizeSafe >>> arr (200 *^)) @@ Busy


newPlayerVel :: Velocity -> Entity -> Entity
newPlayerVel vel e = if isPlayer e 
                    then setVelocity e $ Just vel
                    else e

setPlayerVelocity :: Monad m => ClSF m cl (Velocity, [Entity]) [Entity]
setPlayerVelocity = proc (vel, ents) -> do
  let nents = newPlayerVel vel <$> ents
  returnA -< nents