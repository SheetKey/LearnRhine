module FRP.Rhine.SDL.Process.Draw
  ( draw
  ) where

import FRP.Rhine

import FRP.Rhine.SDL.Entity
import FRP.Rhine.SDL.Components

import qualified SDL


drawHelper :: MonadIO m => SDL.Renderer -> [Entity] -> m ()
drawHelper ren lst = case lst of
                       []   -> liftIO $ return ()
                       x:xs -> drawSprite ren x


drawRect :: MonadIO m => SDL.Renderer -> Entity -> Maybe Rectangle -> m ()
drawRect ren ent rect = case getTexture ent of
                              Nothing   -> liftIO $ return ()
                              Just iotex -> liftIO $ do
                                tex <- iotex
                                SDL.copy ren tex rect (fmap mkRectangle $ getPosition ent)
  

drawSprite :: MonadIO m => SDL.Renderer -> Entity -> m ()
drawSprite ren ent = drawRect ren ent (fmap spriteRect $ getSprite ent)


draw :: MonadIO m => SDL.Renderer -> ClSF m cl [Entity] ()
draw ren = arrMCl $ drawHelper ren
