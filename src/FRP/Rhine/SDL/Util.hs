module FRP.Rhine.SDL.Util where

import FRP.Rhine

import qualified SDL

isPressed :: SDL.KeyboardEventData -> Bool
isPressed kEvent = SDL.keyboardEventKeyMotion kEvent == SDL.Pressed
