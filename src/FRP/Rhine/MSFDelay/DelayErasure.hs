module FRP.Rhine.MSFDelay.DelayErasure where

import FRP.Rhine
import FRP.Rhine.MSFDelay.Core

eraseDelay :: (Monad m, Delay m delay, GetDelayProxy delay) => DRhine m delay a a -> MSF m a a
eraseDelay DRhine {..} = proc a -> do
  delayInfo <- liftTransS (initDelay delay) -< ()
  eraseDelayDSN dsn -< (delayInfo, a)

eraseDelayDSN :: (Monad m, Delay m delay, GetDelayProxy delay) => DSN m delay a a -> MSF m (DelayInfo delay, a) a
eraseDelayDSN dsn@(DSynchronous msfDelay) = eraseDelayMSF (toDelayProxy dsn) msfDelay
    
eraseDelayMSF :: (Monad m, Delay m delay) => DelayProxy delay -> MSFDelay m delay a a -> MSF m (DelayInfo delay, a) a
eraseDelayMSF _ msfDelay = proc (delayInfo, a) -> do
  case delayInfo of
    (False, _) -> returnA -< a
    (True, _) -> (runReaderS msfDelay) -< (a, delayInfo)