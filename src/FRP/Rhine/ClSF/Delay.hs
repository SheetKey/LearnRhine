{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Arrows #-}

module FRP.Rhine.ClSF.Delay where

import FRP.Rhine
import Data.MonadicStreamFunction.InternalCore (MSF (..))

import GHC.TypeLits

type ClSFDelay m cl delay a = ClSF (ReaderT delay m) cl a a


-- TODO: Move to new file
class Delay m delay where
  type DTag delay

  initDelay :: delay -> RunningDelay m (DTag delay)

type RunningDelay m tag = MSF m () (DelayInfo tag)

type DelayInfo tag = (Bool, tag)

newtype FixedStepDelay (n :: Nat) where
  FixedStepDelay :: KnownNat n => FixedStepDelay n

stepsizedelay :: FixedStepDelay n -> Integer
stepsizedelay fsd@FixedStepDelay = natVal fsd

instance Monad m => Delay m (FixedStepDelay n) where
  type DTag (FixedStepDelay n) = ()

  initDelay delay = feedback 0 $ arr $ \(_, delayAcc) -> if delayAcc == stepsizedelay delay
                                                         then return ((True, ()), 0)
                                                         else return ((False, ()), delayAcc + 1)

data DSN m cl delay a where
  DSynchronous :: ClSFDelay m cl delay a -> DSN m cl delay a

data DRhine m cl delay a = DRhine
  { dn :: DN m cl delay a
  , delay :: delay
  }

eraseDelay :: DRhine m cl delay a -> ClSF m cl a a
eraseDelay DRhine {..} = proc a -> do
  delayInfo <- initDelay delay -< ()
  eraseDelayDSN dn -< (delayInfo a)

eraseDelayDSN :: DSN m cl delay a -> ClSF m cl (DelayInfo delay, a) a
eraseDelayDSN (DSynchronous clsfDelay) = eraseDelayClSF clsfDelay
    
eraseDelayClSF :: ClSFDelay m cl delay a -> ClSF m cl (DelayInfo delay, a) a
eraseDelayClSF clsfDelay = proc (delayInfo, a) -> do
  case delayInfo of
    (False, _) -> returnA -< a
    (True, tag) -> runReaderS clsfDelay -< (delayInfo, a)
