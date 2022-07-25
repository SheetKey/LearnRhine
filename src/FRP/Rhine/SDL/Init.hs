{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module FRP.Rhine.SDL.Init where

import FRP.Rhine
import qualified SDL

sdlInitAndFlow :: (Clock IO cl, GetClockProxy cl, Time cl ~ Time (In cl), Time cl ~ Time (Out cl))
               => (SDL.Window -> SDL.Renderer -> Rhine IO cl () ()) -> IO ()
sdlInitAndFlow rhine = do
  SDL.initializeAll
  window <- SDL.createWindow "Test" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  flow $ rhine window renderer
