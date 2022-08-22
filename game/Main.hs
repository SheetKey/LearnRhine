{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import FRP.Rhine
import FRP.Rhine.ClSF.Except 

import FRP.Rhine.SDL
 
import Control.Exception (bracket, catch)
import System.Exit (exitSuccess, exitFailure)
import Control.Monad.Schedule
import Control.Concurrent
import Data.Void
import Data.Word

import qualified Data.Vector.Sized as V

import qualified SDL
import qualified SDL.Internal.Exception as SDL
import qualified SDL.Image as SDLI
import qualified SDL.Font as SDLF

import Foreign.C.Types

import System.Random

import Control.Monad.STM
import Control.Concurrent.STM.TVar

main :: IO ()
main = main8

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
--testBackground :: SDL.Renderer -> IO SDL.Texture
--testBackground ren = do
--  tex <- SDL.createTexture ren SDL.RGBA8888 SDL.TextureAccessTarget (SDL.V2 800 600)
--  SDL.rendererRenderTarget ren SDL.$= Just tex
--  r <- randomRIO (0 :: Word8,255)
--  g <- randomRIO (0 :: Word8,255)
--  b <- randomRIO (0 :: Word8,255)
--  SDL.rendererDrawColor ren SDL.$= SDL.V4 r g b 1
--  SDL.clear ren
--  SDL.rendererRenderTarget ren SDL.$= Nothing
--  return tex
--
--updateStuff :: SDL.Renderer -> ClSF IO FPS60 () ()
--updateStuff ren = constMCl (return Nothing)
--                  >>> renderClSF (Background (testBackground ren)) ren
--                  >>> render ren
--
--loop5 win ren = updateStuff ren @@ waitClock ||@ concurrently @|| sdlQuitAllRh SDL.KeycodeQ win
--
--main5 = sdlInitAndFlow loop5

{--------------------------------------------
A player
NOTE: this example is broken by code rewrites
-}-------------------------------------------
--mkplayer :: MonadIO m => SDL.Renderer -> ClSF m cl () [Entity] 
--mkplayer ren = constMCl $ liftIO $ do
--  xpos <- randomRIO (0 :: CInt, 500)
--  ypos <- randomRIO (0 :: CInt, 500)
--  let e1 = setTexture defaultEntity $ Just $ SDLI.loadTexture ren "sprites/Sprite-0001.png"
--      e2 = setPosition e1 $ Just $ Position xpos ypos 64 64
--  return [ e2 ]
--
--drawStuff ren = mkplayer ren >>> draw ren >>> render ren
--
--loop6 win ren = drawStuff ren @@ waitClock ||@ concurrently @|| sdlQuitAllRh SDL.KeycodeQ win
--
--main6 = sdlInitAndFlow loop6

{--------------------------------------------
An animated player
-}-------------------------------------------
--animPlayer :: SDL.Renderer -> [Entity]
--animPlayer ren = constMCl $ liftIO $ do
--  let e1 = setTexture defaultEntity $ Just $ SDLI.loadTexture ren "sprites/Sprite-0001.png"
--      e2 = setPosition e1 $ Just $ Position 100 100 64 64
--      e3 = setSprite e2 $ Just $ Sprite 0 4 0 32 32
--  return [ e3 ]

--animDraw :: MonadIO m => SDL.Renderer -> ClSF m FPS60 () ()
--animDraw ren = feedback [e3] (animate >>> draw ren)
--  where e1 = setTexture defaultEntity $ Just $ SDLI.loadTexture ren "sprites/Sprite-0001.png"
--        e2 = setPosition e1 $ Just $ Position 100 100 64 64
--        e3 = setSprite e2 $ Just $ Sprite 0 4 0 32 32
-- NOTE: THE FOLLOWING IS BROKEN BY CHANGES TO 'startFeedback'
-- animDraw :: MonadIO m => SDL.Renderer -> ClSF m FPS60 [Entity] [Entity]
-- animDraw ren = animate >>> draw ren
-- 
-- loopAnimDraw :: MonadIO m => SDL.Renderer -> ClSF m FPS60 ((),[Entity]) ((),[Entity])
-- loopAnimDraw ren = startFeedback >>> animDraw ren >>> endFeedback
-- 
-- loopAnimDrawRen :: MonadIO m => SDL.Renderer -> Entity -> ClSF m FPS60 () ()
-- loopAnimDrawRen ren ent = feedback [ent] (loopAnimDraw ren) >>> (render ren)
-- 
-- loop7 win ren = loopAnimDrawRen ren ent @@ waitClock
--                 ||@ concurrently @|| sdlQuitAllRh SDL.KeycodeQ win
--   where e1 = setTexture defaultEntity $ Just $ SDLI.loadTexture ren "sprites/Sprite-0001.png"
--         e2 = setPosition e1 $ Just $ Position 100 100 64 64
--         ent = setSprite e2 $ Just $ Sprite 0 4 0 32 32
-- 
-- main7 = sdlInitAndFlow loop7

{--------------------------------------------
An moving player
-}-------------------------------------------
bullet :: SDL.Renderer -> Entity
bullet ren = e5
  where e1 = setTexture defaultEntity $ Just $ SDLI.loadTexture ren "sprites/Sprite-0003.png"
        e2 = setPosition e1 $ Just $ RenderPosition (Position 0 20 32 32) (Just $ Position 0 20 32 32)
        e3 = setIsPlayer e2 False
        e4 = setVelocity e3 $ Just (90,0)
        e5 = setCollision e4 $ Just $ Collision (30, 30) True id True Nothing

myText :: SDL.Renderer -> Entity
myText ren = e2
  where e1 = setTexture defaultEntity $ Just $
             loadFontTexture ren
                             "fonts/Berry_Rotunda.ttf"
                             70
                             (SDL.V4 128 128 128 100)
                             "Test"
        e2 = setPosition e1 $ Just $ RenderPosition (Position 0 0 100 100) (Just $ Position 0 0 100 100)

spawnBullet :: Monad m => SDL.Renderer -> ClSF m FPS60 [Entity] [Entity]
spawnBullet ren = feedback Nothing $ proc (ents, mlast) ->
  case mlast of
    Nothing -> do
      current <- absoluteS -< ()
      returnA -< (ents, Just current)
    Just last -> do
      current <- absoluteS -< ()
      if diffTime current last >= 1
        then returnA -< (ents ++ [bullet ren], Just current)
        else returnA -< (ents, Just last)
  

moveAnimDraw :: MonadIO m => SDL.Renderer -> Camera -> ClSF m FPS60 (Velocity, [Entity]) [Entity]
moveAnimDraw ren c = setPlayerVelocity
                   >>> collide
                   >>> move c
                   >>> animate
                   >>> removeInactive
                   >>> rotate
                   >>> draw ren
                   -- >>> spawnBullet ren

loopMove :: MonadIO m => SDL.Renderer -> [Entity] -> Camera -> ClSF m FPS60 Velocity ()
loopMove ren ents c = feedback ents
                    (startFeedbackWith
                     >>> moveAnimDraw ren c
                     >>> endFeedback)
                    >>> (render ren)

loopMoveExcept :: MonadIO m => SDL.Renderer -> IO (SDL.V2 CInt) -> [Entity]
               -> ClSFExcept m FPS60 Velocity () Void
loopMoveExcept ren wandhIO ents = do
  (SDL.V2 w h) <- once_ $ liftIO wandhIO
  safe $ loopMove ren ents $ Position 0 0 w h

loop8Help ren wandhIO ents = getPlayerVelocity >-- keepLast (0,0) -@- concurrently
                     --> safely (loopMoveExcept ren wandhIO ents) @@ waitClock

loop8 win ren = loop8Help ren wandhIO ents
                --  ||@ concurrently @|| sdlQuitAllRh SDL.KeycodeQ win
  where e1 = setTexture defaultEntity $ Just $ SDLI.loadTexture ren "sprites/Sprite-0001.png"
        e2 = setPosition e1 $ Just $ RenderPosition (Position 100 100 64 64) (Just $ Position 100 100 64 64)
        e3 = setIsPlayer e2 True
        e4 = setVelocity e3 $ Just (200,0)
        e5 = setCollision e4 $ Just $ Collision (50, 50) True 
             (\e -> setVelocity e $ ((0) *^) <$> (getMVelocity e)) False Nothing
        e6 = setRotation e5 $ Just $ Rotation True 0 Nothing
        ent = setSprite e6 $ Just $ Sprite 0 4 0 32 32

        en1 = setTexture defaultEntity $ Just $ SDLI.loadTexture ren "sprites/Sprite-0002.png"
        en2 = setPosition en1 $ Just $ RenderPosition (Position 100 200 64 64) (Just $ Position 100 200 64 64)
        en3 = setSprite en2 $ Just $ Sprite 0 4 0 32 32
        en4 = setCollision en3 $ Just $ Collision (50, 50) True
              (\e -> setVelocity e $ ((0) *^) <$> (getMVelocity e)) False Nothing
        textEnt = myText ren
        ents = [ent, en4, textEnt]
        wandhIO :: IO (SDL.V2 CInt)
        wandhIO = SDL.get $ SDL.windowSize win

main8 = sdlInitAndFlow loop8


test :: SN IO (SequentialClock IO FPS60 Busy) () ()
test = Feedback (keepLast 0) (Sequential (Synchronous startFeedback) (keepLast 0) (Synchronous endFeedback))
