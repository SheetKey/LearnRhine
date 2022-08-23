{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}


module FRP.Rhine.SDL.Init where

import FRP.Rhine
import qualified SDL
import qualified SDL.Image as SDLI
import qualified SDL.Font as SDLF

import FRP.Rhine.SDL.Entity
import Data.Void

myDefaultWindow :: SDL.WindowConfig
myDefaultWindow = SDL.WindowConfig
  { SDL.windowBorder          = True
  , SDL.windowHighDPI         = False
  , SDL.windowInputGrabbed    = False
  , SDL.windowMode            = SDL.FullscreenDesktop
  , SDL.windowGraphicsContext = SDL.NoGraphicsContext
  , SDL.windowPosition        = SDL.Wherever
  , SDL.windowResizable       = False
  , SDL.windowInitialSize     = SDL.V2 800 600
  , SDL.windowVisible         = True
  }

sdlInitAndFlow :: (Clock IO cl, GetClockProxy cl, Time cl ~ Time (In cl), Time cl ~ Time (Out cl))
               => (SDL.Window -> SDL.Renderer -> Rhine IO cl () ()) -> IO ()
sdlInitAndFlow rhine = do
  SDL.initializeAll
  SDLI.initialize [ SDLI.InitPNG ]
  SDLF.initialize
  window <- SDL.createWindow "Test" myDefaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  flow $ rhine window renderer


startFeedback :: Monad m => ClSF m cl ((), a) a
startFeedback = proc (_, a) -> returnA -< a

-- | Depreciate
startFeedbackWith :: Monad m => ClSF m cl (b, a) (b, a)
startFeedbackWith = proc (b, a) -> returnA -< (b, a)

endFeedback :: Monad m => ClSF m cl a ((), a)
endFeedback = proc a -> returnA -< ((), a)


sdlInitAndFlowLoop :: (Clock IO cl, Clock IO (In cl), Clock IO (Out cl), GetClockProxy cl, Time cl ~ Time (In cl), Time cl ~ Time (Out cl))
                   => gameState
                   -> (SDL.Window -> SDL.Renderer -> Rhine IO cl gameState gameState)
                   -> IO ()
sdlInitAndFlowLoop initial rhine = do
  SDL.initializeAll
  SDLI.initialize [ SDLI.InitPNG ]
  SDLF.initialize
  window <- SDL.createWindow "Test" myDefaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  flow $ feedbackRhine (keepLast initial) $ startFeedback ^->@ (rhine window renderer) @>-^ endFeedback
