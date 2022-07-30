{-# LANGUAGE DataKinds #-}

module FRP.Rhine.SDL.Renderer.Render
  ( render
  , draw
  , FPS60
  ) where

import FRP.Rhine

import FRP.Rhine.SDL.Components
import FRP.Rhine.SDL.Entity


import qualified SDL


-- | A clock that will tick roughly 60 times per second.
-- Only used for presenting the renderer.
type FPS60 = Millisecond 17

-- | A 60 fps signal function that presents and clears the renderer's backbuffer.
-- This is the only function that should be used to present the renderer and should
-- only be called once in the entire program.
render :: MonadIO m => SDL.Renderer -> ClSF m FPS60 () ()
render ren = arrMCl $
  \_ -> do
    SDL.present ren
    SDL.rendererDrawColor ren SDL.$= SDL.V4 0 0 0 0
    SDL.clear ren

--drawHelper :: MonadIO m => SDL.Renderer -> [(Entity, Maybe Position)] -> m ()
--drawHelper ren lst = case lst of
--                       []   -> liftIO $ return ()
--                       x:xs -> liftIO $ do
--                         let ent = fst x
--                             mpos = snd x
--                         tex <- getTexture ren ent
--                         SDL.copy ren tex Nothing mpos
--                         drawHelper ren xs
--
--
--draw :: MonadIO m => SDL.Renderer -> ClSF m cl [(Entity, Maybe Position)] ()
--draw ren = arrMCl $ drawHelper ren

drawHelper :: MonadIO m => SDL.Renderer -> [Entity] -> m ()
drawHelper ren lst = case lst of
                       []   -> liftIO $ return ()
                       x:xs -> case getTexture x of
                                 Just mtex -> liftIO $ do
                                   tex <- mtex
                                   SDL.copy ren tex Nothing (getPosition x)
                                   drawHelper ren xs
                                 Nothing -> liftIO $ return ()

draw :: MonadIO m => SDL.Renderer -> ClSF m cl [Entity] ()
draw ren = arrMCl $ drawHelper ren
