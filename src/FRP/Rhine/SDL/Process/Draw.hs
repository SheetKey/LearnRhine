{-# LANGUAGE Arrows #-}

module FRP.Rhine.SDL.Process.Draw
  ( draw
  ) where

import FRP.Rhine

import FRP.Rhine.SDL.Entity
import FRP.Rhine.SDL.Components

import qualified SDL



drawRect :: MonadIO m => SDL.Renderer -> Entity -> Maybe Rectangle -> m Entity
drawRect ren ent rect =
  case (getMTexture ent, getMRotation ent, getDestination <$> getMPosition ent) of
    (Nothing, _, _) -> liftIO $ return ent
    (_, _, Just Nothing) -> liftIO $ return ent
    (Just iotex, Nothing, _) -> liftIO $ do
      tex <- iotex
      SDL.copy
        ren
        tex
        rect
        (fmap mkRectangle $ getDestination =<< getMPosition ent)
      return ent
    (Just iotex, Just rot, _) -> liftIO $ do
      tex <- iotex
      SDL.copyEx
        ren
        tex
        rect
        (fmap mkRectangle $ getDestination =<< getMPosition ent)
        (angle rot)
        (rotPoint rot)
        (SDL.V2 False False)
      return ent
  

drawSprite :: MonadIO m => SDL.Renderer -> Entity -> m Entity
drawSprite ren ent = drawRect ren ent (fmap spriteRect $ getMSprite ent)

drawHelper :: MonadIO m => SDL.Renderer -> [Entity] -> m [Entity]
drawHelper ren lst = sequence $ fmap (drawSprite ren) lst


draw :: MonadIO m => SDL.Renderer -> ClSF m cl [Entity] [Entity]
draw ren = arrMCl (drawHelper ren)

