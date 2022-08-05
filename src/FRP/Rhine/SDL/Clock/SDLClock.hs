{- |
The source of all SDL events. The 'SDLClock' triggers an event
an provides the 'SDL.Event' as the tag. Uses 'UTCTime' for the
time.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.SDL.Clock.SDLClock where

import Data.Time.Clock 
import Data.Semigroup

import FRP.Rhine

import qualified SDL

-- | The clock providing SDL events.
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

-- | A 'ClSF' that provides the tag of 'SDLClock'.
--   It is advised that this be the only 'ClSF' using
--   the 'SDLClock'. 'pollEvent' should feed into a 'ResBuf'
--   before being passed into another 'ClSF' to be processed.
--   Having any computation happen in a tick of 'SDLClock' will
--   almost ceratinly cause the clock to lag. This is avoided if
--   'pollEvent' is the only 'ClSF' using 'SDLClock'.
pollEvent :: MonadIO m => ClSF m SDLClock () SDL.Event
pollEvent = tagS
