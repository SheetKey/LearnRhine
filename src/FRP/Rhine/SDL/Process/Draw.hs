module FRP.Rhine.SDL.Process.Draw
  ( draw
  ) where

import FRP.Rhine

import FRP.Rhine.SDL.Entity
import FRP.Rhine.SDL.Components

import qualified SDL


drawHelper :: MonadIO m => SDL.Renderer -> [Entity] -> m [Entity]
drawHelper ren lst = sequence $ fmap (drawSprite ren) lst
--drawHelper ren lst = case lst of
--                       []   -> liftIO $ return ()
--                       x:xs -> liftIO $ do
--                         drawSprite ren x
--                         drawHelper ren xs


drawRect :: MonadIO m => SDL.Renderer -> Entity -> Maybe Rectangle -> m Entity
drawRect ren ent rect = case getMTexture ent of
                              Nothing    -> liftIO $ return ent
                              Just iotex -> liftIO $ do
                                tex <- iotex
                                SDL.copy ren tex rect (fmap mkRectangle $ getMPosition ent)
                                return ent
  

drawSprite :: MonadIO m => SDL.Renderer -> Entity -> m Entity
drawSprite ren ent = drawRect ren ent (fmap spriteRect $ getMSprite ent)


draw :: MonadIO m => SDL.Renderer -> ClSF m cl [Entity] [Entity]
draw ren = arrMCl $ drawHelper ren
