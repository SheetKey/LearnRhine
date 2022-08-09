module FRP.Rhine.SDL.Process.Rotate where

import FRP.Rhine

import FRP.Rhine.SDL.Components
import FRP.Rhine.SDL.Entity

import qualified SDL


rotateTowards :: (XPos, YPos) -> [Entity] -> [Entity]
rotateTowards pt@(x, y) [] = []
rotateTowards pt@(x, y) (e:es) = case getMRotation e of
                                   Nothing  -> e : rotateTowards pt es
                                   Just rot -> if towardsMouse rot
                                               then case getMPosition e of
                                                      Nothing -> e : rotateTowards pt es
                                                      Just (Position xP yP w h) ->
                                                        let dx = fromIntegral (xP - x) + ((fromIntegral w) / 2)
                                                            dy = fromIntegral (yP - y) + ((fromIntegral h) / 2)
                                                            angle = ((atan2 dy dx) * 180 / 3.14) - 90
                                                            newE = setRotation e $
                                                                   Just (setAngle rot angle) 
                                                        in newE : rotateTowards pt es
                                               else e : rotateTowards pt es

rotateTowardsMouse :: MonadIO m => [Entity] -> m [Entity]
rotateTowardsMouse ents = do
  SDL.P (SDL.V2 x y) <- SDL.getAbsoluteMouseLocation
  return $ rotateTowards (x, y) ents

rotate :: MonadIO m => ClSF m cl [Entity] [Entity]
rotate = arrMCl rotateTowardsMouse
