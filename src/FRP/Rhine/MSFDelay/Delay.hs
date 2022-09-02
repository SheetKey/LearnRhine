module FRP.Rhine.MSF.Delay
  ( module FRP.Rhine.MSF.Delay
  , module X
  ) where

import FRP.Rhine.MSF.Delay.Proxy as X

class Delay m delay where
  type DTag delay

  initDelay :: delay -> RunningDelay m delay

type RunningDelay m delay = MSF m () (DelayInfo delay)

type DelayInfo delay = DelayInfo
  { doTick :: Bool
  , dTag :: DTag delay
  }
