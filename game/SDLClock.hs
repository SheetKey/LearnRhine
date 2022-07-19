{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module SDLClock where

import Data.Time.Clock 
import Data.Semigroup

import FRP.Rhine

import qualified SDL


data SDLClock = SDLClock

instance MonadIO m => Clock m SDLClock where
  type Time SDLClock = UTCTime
  type Tag  SDLClock = SDL.Event

  initClock _ = do
    initialTime <- liftIO getCurrentTime
    return
      ( constM $ liftIO $ do
          event <- SDL.waitEvent
          time  <- getCurrentTime
          return (time, event)
      , initialTime
      )

instance GetClockProxy SDLClock

instance Semigroup SDLClock where
  _ <> _ = SDLClock
