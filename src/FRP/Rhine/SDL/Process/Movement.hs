{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.SDL.Process.Movement where

import FRP.Rhine
import FRP.Rhine.SDL.Util
import FRP.Rhine.SDL.Clock

import qualified SDL

import FRP.Rhine.SDL.Entity
import FRP.Rhine.SDL.Components


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

updatePos :: Double -> Entity -> Entity
updatePos t e = case getMVelocity e of
                  Nothing  -> e
                  Just vel -> setPosition e $ fmap (modifyPosition (t *^ vel)) (getMPosition e)

move :: (Monad m, Diff (Time cl) ~ Double) => ClSF m cl [Entity] [Entity]
move = proc ents -> do
  _sinceLast <- sinceLastS -< ()
  returnA -< fmap (updatePos _sinceLast) ents
