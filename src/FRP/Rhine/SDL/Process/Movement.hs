{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.SDL.Process.Movement where

import FRP.Rhine
import FRP.Rhine.SDL.Util
import FRP.Rhine.SDL.Clock

import qualified SDL

import FRP.Rhine.SDL.Entity
import FRP.Rhine.SDL.Components
import FRP.Rhine.SDL.Process.Collision


--playerPos :: Point -> Entity -> Entity
--playerPos pt e = if isPlayer e 
--                 then setPosition e $ fmap (modifyPosition pt) (getPosition e)
--                 else e
--
--updatePlayerPos :: (Point, [Entity]) -> [Entity]
--updatePlayerPos (pt, es) = fmap (playerPos pt) es
--
--newPlayerPos :: Point -> Entity -> Entity
--newPlayerPos pt e = if isPlayer e 
--                    then setPosition e $ fmap (setFromPoint pt) (getPosition e)
--                    else e
--
--setPlayerPos :: (Point, [Entity]) -> [Entity]
--setPlayerPos (pt, es) = fmap (newPlayerPos pt) es
--
--movePlayer :: (Monad m, Diff (Time cl) ~ Double) => ClSF m cl (Velocity, [Entity]) [Entity]
--movePlayer = proc (vel, ents) -> do
--  pos <- integral -< vel
--  initPos <- keepFirst -< foldr (\e b -> if isPlayer e
--                                         then case getPosition e of
--                                                Nothing -> b
--                                                Just p  -> mkPoint p
--                                         else b
--                                ) (0,0) ents
--  arr setPlayerPos -< (pos ^+^ initPos, ents)

updateRelPos :: Double -> Entity -> Entity
updateRelPos t e = case getMVelocity e of
                     Nothing  -> e
                     Just vel -> setPosition e $ fmap (modifyRelPos (t *^ vel)) (getMPosition e)
                     --setPosition e $ fmap (modifyPosition (t *^ vel)) (getMPosition e)

type Camera = Position

mkCamera :: ([Entity], Camera) -> Camera
mkCamera ([], c) = c
mkCamera (e:es, c@(Position cx cy cw ch)) =
  if isPlayer e
  then case getMPosition e of
         Nothing -> c
         Just (RenderPosition (Position px py _ _) _)
           -> Position (px - (cw `div` 2)) (py - (ch `div` 2)) cw ch
  else mkCamera (es, c)
                      
cameraBounds :: Camera -> Camera
cameraBounds (Position x y w h) = Position nx ny w h
  where nx = if x < 0 then 0 else if x > w then w else x
        ny = if y < 0 then 0 else if y > h then h else y

updateDest :: Camera -> RenderPosition -> RenderPosition
updateDest cPos@(Position cx cy _ _) rPos@(RenderPosition pos@(Position x y w h) _)
  = if doCollision cPos pos
    then setRenderDest rPos $ Just $ Position (x - cx) (y - cy) w h
    else setRenderDest rPos Nothing

updateDestPos :: Camera -> Entity -> Entity
updateDestPos c e = setPosition e $ fmap (updateDest c) (getMPosition e)

updateAllDest :: ([Entity], Camera) -> [Entity]
updateAllDest (ents, c) = fmap (updateDestPos c) ents

move :: (Monad m, Diff (Time cl) ~ Double) => Camera -> ClSF m cl [Entity] [Entity]
move c = feedback c $ proc (ents, c) -> do
  _sinceLast <- sinceLastS -< ()

  nents <- arr id -< fmap (updateRelPos _sinceLast) ents

  nc <- arr mkCamera -< (nents, c)
  nnc <- arr cameraBounds -< nc

  nnents <- arr updateAllDest -< (nents, nnc)

  returnA -< (nnents, nnc)
