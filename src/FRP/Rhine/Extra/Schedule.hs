{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module FRP.Rhine.Extra.Schedule where

import FRP.Rhine
import Data.MonadicStreamFunction.Async (concatS)

-- base
import GHC.TypeLits

-- UNNEEDED
--duplicateDualTick :: Monad m => MSF m () (time, Either (Either a b) c)
--                  -> MSF m () (time, Either (Either a b) (Either a c))
--duplicateDualTick runningClock = concatS $ runningClock >>> arr dupA
--  where
--    dupA (time, Left (Left  a)) = [ (time, Left (Left a)), (time, Right (Left a)) ]
--    dupA (time, Left (Right b)) = [ (time, Left (Right b)) ]
--    dupA (time, Right c)        = [ (time, Right (Right c)) ]
--
--  
--
--schedDualPar :: Monad m
--             => Schedule m (ParClock m cla clb) clc
--             -> Schedule m (ParClock m cla clb) (ParClock m cla clc)
--schedDualPar sched = Schedule $ \parab parac -> do
--  let clc = parallelCl2 parac
--  (runningClock, initTime) <- (initSchedule sched) parab clc
--  return (duplicateDualTick runningClock, initTime)

--------------------------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------------------------

clsf :: Monad m => ClSF m cl Int Int
clsf = arr id

mysn :: ( Monad m
        , cl ~ In cl
        , cl ~ Out cl
        )
     => SN m cl Int Int
mysn = Synchronous clsf

type S1 = Millisecond 1000
type S2 = Millisecond 2000
type S3 = Millisecond 3000

mysn1 :: Monad m => SN m S1 Int Int
mysn1 = mysn

mysn2 :: Monad m => SN m S2 Int Int
mysn2 = mysn

mysn3 :: Monad m => SN m S3 Int Int
mysn3 = mysn

test1 :: ( Monad m
         , Clock m S1
         , Clock m S2
         , Clock m S3
         )
      => MySN m (SeqClock m S1 (SeqClock m S1 (ParClock m S2 S3))) Int Int
test1 = SeqInjection
  (Parallel mysn1 mysn2)
  (keepLast 0)
  (Parallel mysn1 mysn3)

--------------------------------------------------------------------------------------------------


  
--------------------------------------------------------------------------------------------------
  -- Creating a SeqInjection
--------------------------------------------------------------------------------------------------
-- Rhine.Reactimation.Combinators

--mkSeqInjRhine :: ( Monad m
--                 , IsCombo cl1
--                 , FarLeft clDom cl1 ~ clDom
--                 )
--              => Rhine m cl1 a b
--              -> ResBuf m (Out clDom) (In clDom) b c
--              -> Schedule m (ParROrFirstRPar cl1) clnew
--              -> Rhine m (ParClock m clDom clNew) c d
--              -> Rhine m (SeqClock m clDom
--                           (SeqClock m clDom
--                             (CombinePar m (ParROrFirstRPar cl1) clNew))) a d
--mkSeqInjRhine (Rhine (Injection snL1 snR1) (ParallelClock clL1 clR2))
--              bf newSched
--              (Rhine (Injection snL2 snR2) (ParallelClock clL2 clR2))
--  = 
--
--
--(>=-) :: Rhine m cl1 a b
--      -> ResamplingPoint m cl1 cl2 b c
--      -> RhineAndResamplingPoint m cl1 cl2 a c
--
--(-=>) :: RhineAndResamplingPoint m cl1 cl2 a b
--  -> Rhine m cl2 b c
--  -> Rhine m (

  
--------------------------------------------------------------------------------------------------
-- Rhine.SN


data MySN m cl a b where
  SeqInjection :: ( Monad m
                  , IsCombo cl1
                  , FarLeft clDom cl1 ~ clDom
                  )
               => SN m cl1 a b
               -- If can't remove the resbuf then equivalent to Sequential
               -> ResBuf m (Out clDom) (In clDom) b c
               -> SN m (ParClock m clDom clNew) c d
               -> MySN m (SeqClock m clDom
                           (SeqClock m clDom
                             (CombinePar m (ParROrFirstRPar cl1) clNew))) a d

  Injection :: (Monad m
               )
            => SN m cl1 b c
            -> ResBuf m (Out cl1) (In cl2) c d
            -> SN m cl2 (a, d) e
            -> MySN m (ParClock m cl2 cl1) a e

--Rhine.Schedule

  
class IsCombo cl

instance IsCombo (ParallelClock m cla clb)
instance IsCombo (SequentialClock m cla clb)

-- Does not compile. No info found online as to why.
--type family IsInj (con :: a -> b -> c -> st) :: Bool where
--  IsInj 'SeqInjection = 'True
--  IsInj 'Injection = 'True
--  IsInj _ = 'False


type family CombinePar m cl clNew where
  CombinePar m (ParClock m a b) clNew = ParClock m (ParClock m a b) clNew 
  CombinePar m (SeqClock m a b) clNew = SeqClock m a (CombinePar m b clNew)
  CombinePar m cl               clNew = ParClock m cl clNew
                                    

type family FarLeft clDom cl where
  FarLeft clDom (SeqClock m clDom cl2) = clDom
  FarLeft clDom (SeqClock m cl1   cl2) = FarLeft clDom cl1
  FarLeft clDom (ParClock m clDom clR) = clDom
  FarLeft clDom (ParClock m clL   clR) = TypeError
    ( Text "'FarLeft' didn't match"
      :$$: Text "when applied to the types '"
      :<>: ShowType clDom :<>: Text "' and '"
      :<>: ShowType clL :<>: Text "'" )
  FarLeft clDom cl = TypeError
    ( Text "'FarLeft' didn't match"
      :$$: Text "when applied to the types '"
      :<>: ShowType clDom :<>: Text "' and '"
      :<>: ShowType cl :<>: Text "'" )

  

type family ParROrFirstRPar cl where
  ParROrFirstRPar (SeqClock m cla (ParClock m clL clR)) = ParClock m clL clR
  ParROrFirstRPar (SeqClock m cla clb) = ParROrFirstRPar clb
  ParROrFirstRPar (ParClock m cla clb) = clb
  ParROrFirstRPar cl = TypeError ( Text "'ParROrFirstRPar' didn't match"
                                   :$$: Text "when applied to the type '"
                                   :<>: ShowType cl :<>: Text "'" )
