{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows #-}

module Main where

import FRP.Rhine
import FRP.Rhine.ClSF.Except 

import FRP.Rhine.SDL.Init
import FRP.Rhine.SDL.Clock.SDLClock
import FRP.Rhine.SDL.Clock.SDLQuitClock
import FRP.Rhine.SDL.Renderer.Renderable
import FRP.Rhine.SDL.Renderer.Movable
import FRP.Rhine.SDL.Renderer.Type
import FRP.Rhine.SDL.Renderer.Render
import FRP.Rhine.SDL.Background

import System.Exit (exitSuccess, exitFailure)
import Control.Monad.Schedule
import Control.Concurrent
import Data.Void
import Data.Word

import qualified Data.Vector.Sized as V

import qualified SDL

import System.Random

main :: IO ()
main = main5

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
  SDL.quit

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

quitAll :: MonadIO m => SDL.Window -> m ()
quitAll win = do
  SDL.destroyWindow win
  SDL.quit
  liftIO exitSuccess

quitProgram :: MonadIO m => SDL.Window -> ClSF m Busy (Maybe SDL.Event) ()
quitProgram win = arr eventIsQ >>> proc b -> if b
                                             then arrMCl quitAll -< win
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

{--------------------------------------------
A simpler quit
-}-------------------------------------------
main3 :: IO ()
main3 = do
  SDL.initializeAll
  window <- SDL.createWindow "Test" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  flow $ sdlQuitAllRh SDL.KeycodeQ window

{--------------------------------------------
Background
-}-------------------------------------------
setBackground :: SDL.Renderer -> IO ()
setBackground ren = do
  SDL.rendererDrawColor ren SDL.$= SDL.V4 25 165 255 1
  SDL.clear ren
  SDL.fillRect ren Nothing
  SDL.present ren

setBackgroundOnce :: SDL.Renderer -> ClSF (ExceptT () IO) cl () ()
setBackgroundOnce ren = proc _ -> do
  (runClSFExcept $ safe $ arrMCl setBackground) -< ren
  throwS -< ()

setBackgroundOnceSafe :: SDL.Renderer -> ClSF IO cl () ()
setBackgroundOnceSafe ren = safely $ do
  try $ setBackgroundOnce ren
  try $ runClSFExcept $ safe $ constMCl $ return ()

setBRh :: SDL.Renderer -> Rhine IO Busy () ()
setBRh ren = setBackgroundOnceSafe ren @@ Busy

loop4 win ren = setBRh ren ||@ concurrently @|| sdlQuitAllRh SDL.KeycodeQ win

main4 = sdlInitAndFlow loop4

{--------------------------------------------
A better background
-}-------------------------------------------
testBackground :: SDL.Renderer -> IO SDL.Texture
testBackground ren = do
  tex <- SDL.createTexture ren SDL.RGBA8888 SDL.TextureAccessTarget (SDL.V2 800 600)
  SDL.rendererRenderTarget ren SDL.$= Just tex
  r <- randomRIO (0 :: Word8,255)
  g <- randomRIO (0 :: Word8,255)
  b <- randomRIO (0 :: Word8,255)
  SDL.rendererDrawColor ren SDL.$= SDL.V4 r g b 1
  SDL.clear ren
  SDL.rendererRenderTarget ren SDL.$= Nothing
  return tex

updateStuff :: SDL.Renderer -> ClSF IO FPS60 () ()
updateStuff ren = constMCl (return Nothing)
                  >>> renderClSF (Background (testBackground ren)) ren
                  >>> render ren

loop5 win ren = updateStuff ren @@ waitClock ||@ concurrently @|| sdlQuitAllRh SDL.KeycodeQ win

main5 = sdlInitAndFlow loop5


