module FRP.Rhine.SDL.Process.Animate
  ( animate
  ) where

import FRP.Rhine

import qualified SDL

import FRP.Rhine.SDL.Components


updateFrame :: Entity -> Entity
updateFrame ent = setSprite ent (fmap incFrameIndex $ getSprite ent)

animateHelper :: [Entity] -> [Entity]
animateHelper = fmap updateFrame
  
animate :: Monad => BehaviorF m time [Entity] [Entity]
animate = undefined
