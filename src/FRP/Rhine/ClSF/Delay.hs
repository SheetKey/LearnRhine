{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}

{-# LANGUAGE TypeFamilyDependencies #-}

module FRP.Rhine.ClSF.Delay where

import FRP.Rhine
import Control.Monad.Trans.Reader

import GHC.TypeLits

import Data.Functor.Identity

import Data.Kind (Type)

--import Data.Proxy

-- TRY: Change this type to 'a -> b' but insist type is 'a -> a' in 'eraseDelay'
type ClSFDelay m cl delay a = ClSF (ReaderT (DelayInfo delay) m) cl a a

type ClSFDelayTest3 m cl delay a b = ClSF (ReaderT (DelayInfo delay) m) cl a (Either a b)

type family ClSFDelayTest (m :: * -> *) cl delay (f :: * -> *) a = r | r -> f a where
  ClSFDelayTest m cl delay (Either b) a = ClSF (ReaderT (DelayInfo delay) m) cl a (Either b a)
  ClSFDelayTest m cl delay Identity a = ClSF (ReaderT (DelayInfo delay) m) cl a (Identity a)

type family ClSFDelayTest2 (m :: * -> *) cl delay a = r | r -> a where
  ClSFDelayTest2 m cl delay (Either b a) = ClSF (ReaderT (DelayInfo delay) m) cl a (Either b a)
  ClSFDelayTest2 m cl delay a = ClSF (ReaderT (DelayInfo delay) m) cl (Identity a) (Identity a)


-- TODO: Move to new file
class Delay m delay where
  type DTag delay

  initDelay :: delay -> RunningDelay m delay

type RunningDelay m delay = MSF m () (DelayInfo delay)

type DelayInfo delay = (Bool, DTag delay)

--genDelayInfo :: (Monad m, Delay m delay) => Proxy delay -> MSF m (Bool, DTag delay) (DelayInfo delay)
--genDelayInfo _ = proc input -> returnA -< input

data FixedStepDelay (n :: Nat) where
  FixedStepDelay :: KnownNat n => FixedStepDelay n

stepsizedelay :: FixedStepDelay n -> Integer
stepsizedelay fsd@FixedStepDelay = natVal fsd

instance Monad m => Delay m (FixedStepDelay n) where
  type DTag (FixedStepDelay n) = ()

  initDelay delay = feedback 0 $ arrM $ \(_, delayAcc) -> if delayAcc == stepsizedelay delay
                                                          then return ((True, ()), 0)
                                                          else return ((False, ()), delayAcc + 1)

data DSN m cl delay a where
  DSynchronous :: ClSFDelay m cl delay a -> DSN m cl delay a

data DRhine m cl delay a = DRhine
  { dsn :: DSN m cl delay a
  , delay :: delay
  }

eraseDelay :: (Monad m, Delay m delay, GetDelayProxy delay) => DRhine m cl delay a -> ClSF m cl a a
eraseDelay DRhine {..} = proc a -> do
  delayInfo <- liftTransS (initDelay delay) -< ()
  eraseDelayDSN dsn -< (delayInfo, a)

eraseDelayDSN :: (Monad m, Delay m delay, GetDelayProxy delay) => DSN m cl delay a -> ClSF m cl (DelayInfo delay, a) a
eraseDelayDSN dsn@(DSynchronous clsfDelay) = eraseDelayClSF (toDelayProxy dsn) clsfDelay
    
eraseDelayClSF :: (Monad m, Delay m delay) => DelayProxy delay -> ClSFDelay m cl delay a -> ClSF m cl (DelayInfo delay, a) a
eraseDelayClSF _ clsfDelay = proc (delayInfo, a) -> do
  case delayInfo of
    (False, _) -> returnA -< a
    (True, _) -> (runReaderS clsfDelay) -< (a, delayInfo)

delayInfoOf :: Monad m => (DelayInfo delay -> b) -> ClSF (ReaderT (DelayInfo delay) m) cl a b
delayInfoOf f = constM $ asks f

dTagS :: ClSF (ReaderT (DelayInfo delay) m) cl a (DTag delay)
dTagS = delayInfoOf snd

-- PROXY

data DelayProxy delay where
  LeafDelay :: DelayProxy delay

class GetDelayProxy delay where
  getDelayProxy :: DelayProxy delay

  default getDelayProxy
    :: DelayProxy delay
  getDelayProxy = LeafDelay
  
class ToDelayProxy a where
  type Dl a :: Type

  toDelayProxy :: a -> DelayProxy (Dl a)

  default toDelayProxy
    :: GetDelayProxy (Dl a)
    => a -> DelayProxy (Dl a)
  toDelayProxy _ = getDelayProxy

-- Proxy instances
instance GetDelayProxy delay => ToDelayProxy (DSN m cl delay a) where
  type Dl (DSN m cl delay a) = delay

instance GetDelayProxy delay => ToDelayProxy (DRhine m cl delay a) where
  type Dl (DRhine m cl delay a) = delay

instance GetDelayProxy (FixedStepDelay n)
