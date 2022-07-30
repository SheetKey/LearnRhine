{-# LANGUAGE DataKinds #-}

module FRP.Rhine.SDL.Renderer.Render
  ( render
  , FPS60
  ) where

import FRP.Rhine

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


