{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows #-}

module Stdin
  ( test
  ) where

import FRP.Rhine
import Control.Monad.Schedule
import System.Exit (exitSuccess, exitFailure)


test :: IO ()
test = flow mainRhine

-- Slight changes to example provide in rhine examples folder

type MyPeriodic = Periodic '[500, 1000]
type UnPeriodic = HoistClock (ScheduleT Integer IO) IO MyPeriodic

rhEveryNowAndThen :: Monad m => ClSF m MyPeriodic arbitrary String
rhEveryNowAndThen = sinceInitS >>> proc time ->
  returnA -< unwords ["It's now", show time, "o'clock."]

rhPrintEveryNowAndThen :: MonadIO m => ClSF (ScheduleT Integer m) MyPeriodic () ()
rhPrintEveryNowAndThen = rhEveryNowAndThen >-> arrMCl (liftIO . putStrLn) 

rhPrintEveryNowAndThenRh :: Rhine IO UnPeriodic () ()
rhPrintEveryNowAndThenRh = hoistClSFAndClock runScheduleIO rhPrintEveryNowAndThen
                         @@ HoistClock Periodic runScheduleIO
