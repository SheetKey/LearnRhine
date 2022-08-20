{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.SDL.Process.Movement where

import FRP.Rhine
import FRP.Rhine.SDL.Util
import FRP.Rhine.SDL.Clock

import qualified SDL

import FRP.Rhine.SDL.Entity
import FRP.Rhine.SDL.Components

import Control.Monad.STM
import Control.Concurrent.STM.TVar


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

type CameraPosition = Position
type Camera = TVar CameraPosition

updateCamera :: ([Entity], Camera) -> STM ()
updateCamera ([], c) = return ()
updateCamera (e:es, c) =
  if isPlayer e
  then case getMPosition e of
         Nothing -> return ()
         Just (RenderPosition (Position px py _ _) _)
           -> do (Position cx cy cw ch) <- readTVar c
                 writeTVar c $ Position (px - (cw `div` 2)) (py - (ch `div` 2)) cw ch
  else updateCamera (es, c)
                      
getCameraBounds :: CameraPosition -> CameraPosition
getCameraBounds (Position x y w h) = Position nx ny w h
  where nx = if x < 0 then 0 else if x > w then w else x
        ny = if y < 0 then 0 else if y > h then h else y

cameraBounds :: Camera -> STM ()
cameraBounds c = modifyTVar c getCameraBounds

updateDest :: CameraPosition -> RenderPosition -> RenderPosition
updateDest (Position cx cy _ _) rpos@(RenderPosition (Position x y w h) _)
  = setRenderDest rpos $ Position (x - cx) (y - cy) w h

updateDestPos :: CameraPosition -> Entity -> Entity
updateDestPos c e = setPosition e $ fmap (updateDest c) (getMPosition e)

updateAllDest :: ([Entity], CameraPosition) -> [Entity]
updateAllDest (ents, c) = fmap (updateDestPos c) ents

move :: (MonadIO m, Diff (Time cl) ~ Double) => Camera -> ClSF m cl [Entity] [Entity]
move c = proc ents -> do
  _sinceLast <- sinceLastS -< ()

  nents <- arr id -< fmap (updateRelPos _sinceLast) ents

  --nc <- arr mkCamera -< (nents, c)
  --nnc <- arr cameraBounds -< nc
  cPos <- arrMCl (liftIO . atomically) -< (do updateCamera (nents, c)
                                              cameraBounds c
                                              readTVar c
                                          )

  nnents <- arr updateAllDest -< (nents, cPos)

  returnA -< nnents
