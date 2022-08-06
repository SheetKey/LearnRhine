{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

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
updateFrame ent = setSprite ent (fmap incFrameIndex $ getMSprite ent)

animateHelper :: [Entity] -> [Entity]
animateHelper = fmap updateFrame
  
animateFeedback :: (Monad m, TimeDomain (Time cl), Diff (Time cl) ~ Double)
                => ClSF m cl ([Entity], Maybe (Time cl)) ([Entity], Maybe (Time cl))
animateFeedback = proc (es, mlast) ->
  case mlast of
    Nothing -> do
      current <- absoluteS -< ()
      returnA -< (es, Just current)
    Just last -> do
      current <- absoluteS -< ()
      if diffTime current last >= 1
        then returnA -< (animateHelper es, Just current)
        else returnA -< (es, Just last)

animate :: (Monad m, TimeDomain (Time cl), Diff (Time cl) ~ Double) => ClSF m cl [Entity] [Entity]
animate = feedback Nothing animateFeedback
