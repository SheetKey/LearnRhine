{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows #-}

module Main where

import FRP.Rhine
import FRP.Rhine.ClSF.Except 

import SDLClock

import Control.Monad.Schedule
import Control.Concurrent
import Data.Void

import qualified Data.Vector.Sized as V

import qualified SDL

main :: IO ()
main = main1

{--------------------------------------------
Basic SDL
-}-------------------------------------------
main1 :: IO ()
main1 = do
  SDL.initializeAll
  window <- SDL.createWindow "Test" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.delay 5000
  SDL.destroyWindow window

{--------------------------------------------
Testing our clock
-}-------------------------------------------
getEvent :: MonadIO m => ClSF m SDLClock () SDL.Event
getEvent = tagS

eventIsKey :: SDL.Keycode -> Maybe SDL.Event -> Bool
eventIsKey code (Just event)
  = case SDL.eventPayload event of
      SDL.KeyboardEvent kEvent -> SDL.keyboardEventKeyMotion kEvent == SDL.Pressed
                               && SDL.keysymKeycode (SDL.keyboardEventKeysym kEvent) == code
      _ -> False
eventIsKey _ Nothing = False

eventIsQ :: Maybe SDL.Event -> Bool
eventIsQ = eventIsKey SDL.KeycodeQ

quitProgram :: MonadIO m => SDL.Window -> ClSF m Busy (Maybe SDL.Event) ()
quitProgram win = arr eventIsQ >>> proc b -> if b
                                                then arrMCl SDL.destroyWindow -< win
                                                else returnA -< ()

appLoop2 :: SDL.Window -> Rhine IO (SequentialClock IO SDLClock Busy) () ()
appLoop2 win = getEvent @@ SDLClock
               >-- fifoBounded 5 -@- concurrently
               --> quitProgram win @@ Busy

main2 :: IO ()
main2 = do
  SDL.initializeAll
  window <- SDL.createWindow "Test" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  flow $ appLoop2 window
