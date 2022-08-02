{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}

module FRP.Rhine.SDL.Process.Animate
  ( animate
  ) where

import FRP.Rhine
import FRP.Rhine.ClSF.Upsample

import qualified SDL

import FRP.Rhine.SDL.Entity
import FRP.Rhine.SDL.Clock
import FRP.Rhine.SDL.Components




updateFrame :: Entity -> Entity
updateFrame ent = setSprite ent (fmap incFrameIndex $ getSprite ent)

animateHelper :: [Entity] -> [Entity]
animateHelper = fmap updateFrame
  
animateFeedback :: Monad m => ClSF m cl ([Entity],CInt) ([Entity],CInt)
animateFeedback = proc (es, i) -> if mod i 60 == 0
                                  then returnA -< (animateHelper es, i + 1)
                                  else returnA -< (es, i + 1)

animate :: Monad m => ClSF m cl [Entity] [Entity]
animate = feedback 0 animateFeedback
