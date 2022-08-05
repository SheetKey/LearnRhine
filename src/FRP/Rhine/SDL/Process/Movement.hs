{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.SDL.Process.Movement where

import FRP.Rhine
import FRP.Rhine.SDL.Util
import FRP.Rhine.SDL.Clock

import qualified SDL

import FRP.Rhine.SDL.Entity
import FRP.Rhine.SDL.Components.Position

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

normalizeSafe :: (VectorSpace v a) => v -> v
normalizeSafe v = if nv == 0 then v else v ^/ nv
  where nv = norm v

getPoint :: Rhine IO (SequentialClock IO SDLClock Busy) () Velocity
getPoint = pollEvent @@ SDLClock
           >-- fifoBounded 25 -@- concurrently -->
           (processInput >>> arr normalizeSafe >>> traceWith (liftIO . putStrLn) "norm:" ) @@ Busy

playerPos :: Point -> Entity -> Entity
playerPos pt e = if isPlayer e 
                 then setPosition e $ fmap (updatePosition pt) (getPosition e)
                 else e

updatePlayerPos :: (Point, [Entity]) -> [Entity]
updatePlayerPos (pt, es) = fmap (playerPos pt) es

newPlayerPos :: Point -> Entity -> Entity
newPlayerPos pt e = if isPlayer e 
                    then setPosition e $ fmap (setFromPoint pt) (getPosition e)
                    else e

setPlayerPos :: (Point, [Entity]) -> [Entity]
setPlayerPos (pt, es) = fmap (newPlayerPos pt) es

movePlayer :: (Monad m, Diff (Time cl) ~ Double)
           => ClSF m cl (Velocity, [Entity]) [Entity]
--movePlayer = arr updatePlayerPos
--movePlayer initPos = first integral >>> first (arr (^+^ initPos)) >>> arr setPlayerPos

movePlayer = proc (vel, ents) -> do
  pos <- integral -< vel
  initPos <- keepFirst -< foldr (\e b -> if isPlayer e
                                         then case getPosition e of
                                                Nothing -> b
                                                Just p  -> mkPoint p
                                         else b
                                ) (0,0) ents
  arr setPlayerPos -< (pos ^+^ initPos, ents)
