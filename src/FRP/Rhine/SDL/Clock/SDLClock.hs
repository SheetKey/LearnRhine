{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.SDL.Clock.SDLClock where

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
      ( filterS $ constM $ liftIO $ do
          mevent <- SDL.pollEvent
          case mevent of
            Just event -> do
              time  <- getCurrentTime
              return $ Just (time, event)
            Nothing -> return Nothing
      , initialTime
      )

instance GetClockProxy SDLClock

instance Semigroup SDLClock where
  _ <> _ = SDLClock
