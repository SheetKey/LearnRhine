{-# LANGUAGE Arrows #-}

module FRP.Rhine.SDL.Util where

import FRP.Rhine

import qualified SDL

isPressed :: SDL.KeyboardEventData -> Bool
isPressed kEvent = SDL.keyboardEventKeyMotion kEvent == SDL.Pressed

printInOut :: (MonadIO m, Show a) => ClSF m cl a a
printInOut = proc a -> do
  arrMCl (\x -> liftIO $ print x) -< a
  returnA -< a
