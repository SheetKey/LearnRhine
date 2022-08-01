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
                       x:xs -> case getTexture x of
                                 Just mtex -> liftIO $ do
                                   tex <- mtex
                                   SDL.copy ren tex Nothing (fmap mkRectangle $ getPosition x)
                                   drawHelper ren xs
                                 Nothing -> liftIO $ return ()

draw :: MonadIO m => SDL.Renderer -> ClSF m cl [Entity] ()
draw ren = arrMCl $ drawHelper ren
