module FRP.Rhine.MSFDelay.Core where

import FRP.Rhine
import Control.Monad.Trans.Reader

import FRP.Rhine.MSF.Delay

type MSFDelay m delay a b = MSF (ReaderT (DelayInfo delay) m) a b

type ClSFDelay m delay cl a b = MSFDelay (ReaderT

data DSN m delay a b where
  DSynchronous :: MSFDelay m delay a b -> DSN m delay a b

instance GetDelayProxy delay => ToDelayProxy (DSN m delay a b) where
  type Dl (DSN m delay a b) = delay

data DRhine m delay a b = DRhine
  { dsn :: DSN m delay a b
  , delay :: delay
  }

instance GetDelayProxy delay => ToDelayProxy (DRhine m delay a b) where
  type Dl (DRhine m delay a b) = delay