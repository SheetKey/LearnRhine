{-# LANGUAGE Arrows #-}

module FRP.Rhine.SDL.Process.Draw
  ( draw
  ) where

import FRP.Rhine

import FRP.Rhine.SDL.Entity
import FRP.Rhine.SDL.Components

import qualified SDL


type Camera = Position


mkCamera :: ([Entity], Camera) -> Camera
mkCamera ([], c) = c
mkCamera (e:es, c@(Position cx cy cw ch)) =
  if isPlayer e
  then case getMPosition e of
         Nothing -> c
         Just (Position px py _ _) -> Position (px - (cw `div` 2)) (py - (ch `div` 2)) cw ch
  else mkCamera (es, c)
                      
cameraBounds :: Camera -> Camera
cameraBounds (Position x y w h) = Position nx ny w h
  where nx = if x < 0 then 0 else if x > w then w else x
        ny = if y < 0 then 0 else if y > h then h else y

updateDest :: Camera -> Position -> Position
updateDest (Position cx cy _ _) (Position x y w h) = Position (x - cx) (y - cy) w h
               

drawRect :: MonadIO m => SDL.Renderer -> Camera -> Entity -> Maybe Rectangle -> m Entity
drawRect ren c ent rect =
  case (getMTexture ent, getMRotation ent) of
    (Nothing, _) -> liftIO $ return ent
    (Just iotex, Nothing) -> liftIO $ do
      tex <- iotex
      SDL.copy
        ren
        tex
        rect
        (fmap mkRectangle $ fmap (updateDest c) $ getMPosition ent)
      return ent
    (Just iotex, Just rot) -> liftIO $ do
      tex <- iotex
      SDL.copyEx
        ren
        tex
        rect
        (fmap mkRectangle $ fmap (updateDest c) $ getMPosition ent)
        (angle rot)
        (rotPoint rot)
        (SDL.V2 False False)
      return ent
  

drawSprite :: MonadIO m => SDL.Renderer -> Camera -> Entity -> m Entity
drawSprite ren c ent = drawRect ren c ent (fmap spriteRect $ getMSprite ent)

drawAll :: MonadIO m => SDL.Renderer -> Camera -> [Entity] -> m [Entity]
drawAll ren c lst = sequence $ fmap (drawSprite ren c) lst

drawHelper :: MonadIO m => SDL.Renderer -> ([Entity], Camera) -> m ([Entity], Camera)
drawHelper ren (lst, c) = do
  nlst <- drawAll ren c lst
  return (nlst, c)


draw :: MonadIO m => SDL.Renderer -> Camera -> ClSF m cl [Entity] [Entity]
draw ren c = feedback c $ proc (lst, c) -> do
  nc <- arr mkCamera -< (lst, c)
  nnc <- arr cameraBounds -< nc
  arrMCl (drawHelper ren) -< (lst, nnc)
