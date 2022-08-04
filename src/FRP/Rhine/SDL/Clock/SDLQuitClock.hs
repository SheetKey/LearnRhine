{-# LANGUAGE Arrows #-}

module FRP.Rhine.SDL.Clock.SDLQuitClock
  ( sdlQuitAllRh
  ) where

import FRP.Rhine
import FRP.Rhine.SDL.Clock.SDLClock
import FRP.Rhine.SDL.Util

import qualified SDL
import System.Exit (exitSuccess)


type SDLQuitClock = SelectClock SDLClock ()

quitAll :: MonadIO m => SDL.Window -> m ()
quitAll win = do
  SDL.destroyWindow win
  SDL.quit
  liftIO exitSuccess

sdlQuitAll :: MonadIO m => SDL.Window -> ClSF m SDLQuitClock () ()
sdlQuitAll win = proc _ -> arrMCl quitAll -< win

sdlQuitAllRh :: MonadIO m => SDL.Keycode -> SDL.Window -> Rhine m SDLQuitClock () ()
sdlQuitAllRh code win = sdlQuitAll win @@ SelectClock SDLClock
  (\event ->
     case SDL.eventPayload event of
       SDL.KeyboardEvent kEvent ->
         if isPressed kEvent 
            && SDL.keysymKeycode (SDL.keyboardEventKeysym kEvent) == code
         then Just ()
         else Nothing
       SDL.QuitEvent -> Just ()
       _ -> Nothing)
                       
