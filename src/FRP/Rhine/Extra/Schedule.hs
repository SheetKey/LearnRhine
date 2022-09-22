module FRP.Rhine.Extra.Schedule where

import FRP.Rhine


duplicateDualTick :: MSF m () (time, Either (Either a b) c) -> MSF m () (time, Either (Either a b) (Either a c))
duplicateDualTick runningClock = concatS $ runningClock >>> arr dupA
  where
    dupA (time, Left (Left  a)) = [ (time, Left (Left a)), (time, Right (Left a)) ]
    dupA (time, Left (Right b)) = [ (time, Left (Right b)) ]
    dupA (time, Right c         = [ (time, Right (Right c)) ]

schedDualPar :: Monad m => Schedule m (ParClock m cla clb) clc -> Schedule m (ParClock m cla clb) (ParClock m cla clc)
schedDualPar sched = Schedule $ \ parab parac ->
  let clc = parallelCl2 parac
      --newClock = ParallelClock parab clc sched
  in do
    (runningClock, initTime) <- (initSchedule sched) parab clc
    return (duplicateDualTick runningClock, initTime
