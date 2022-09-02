module FRP.Rhine.MSFDelay.Delay.FixedStepDelay where

import FRP.Rhine.MSF.Delay

data FixedStepDelay (n :: Nat) where
  FixedStepDelay :: KnownNat n => FixedStepDelay n

stepsizedelay :: FixedStepDelay n -> Integer
stepsizedelay fsd@FixedStepDelay = natVal fsd

instance Monad m => Delay m (FixedStepDelay n) where
  type DTag (FixedStepDelay n) = ()

  initDelay delay = feedback 0 $ arrM $ \(_, delayAcc) -> if delayAcc == stepsizedelay delay
                                                          then return ((True, ()), 0)
                                                          else return ((False, ()), delayAcc + 1)

instance GetDelayProxy (FixedStepDelay n)
