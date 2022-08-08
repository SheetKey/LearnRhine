module FRP.Rhine.SDL.Process.Rotate where

import FRP.Rhine.SDL.Components
import FRP.Rhine.SDL.Entity

import qualified SDL


rotateTowards :: (XPos, YPos) -> [Entity] -> [Entity]
rotateTowards pt@(x, y) [] = []
rotateTowards pt@(x, y) (e:es) = case getMRotation e of
                                   Nothing  -> e : rotateTowards pt es
                                   Just rot -> if towardsMouse e
                                               then case getMPosition e of
                                                      Nothing -> e : rotateTowards pt es
                                                      Just (Position xP yP _ _) ->
                                                        let dx = xP - x
                                                            dy = yP - y
                                                            angle = (atan2 dy dx) * 180 / 3.14
                                                            newE = setRotation e $
                                                                   Just (setAngle rot angle) 
                                                        in newE : rotateTowards pt es
                                               else e : rotateTowards pt es

rotateTowardsMouse :: [Entity] -> IO [Entity]
rotateTowardsMouse ents = do
  SDL.P (SDL.V x y) <- SDL.getAbsoluteMouseLocation
  rotateTowards (x, y) ents

