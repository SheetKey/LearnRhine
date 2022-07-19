{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows #-}

module Main where

import qualified Example1 as E1

import FRP.Rhine
import FRP.Rhine.ClSF.Except 
import Control.Monad.Schedule
import Data.Void

import qualified Data.Vector.Sized as V

import Control.Monad ( guard )
import System.Exit (exitSuccess, exitFailure)
import System.IO (hFlush, stdin, stdout)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

main :: IO ()
main = flow checkOrPrintRh

--------------------------------------------------
-- 1 second clock
--------------------------------------------------
type Second = Millisecond 1000

--------------------------------------------------
-- Printing
--------------------------------------------------
rhPrint :: Show a => a -> ClSF IO Second () ()
rhPrint = constMCl . print

rhPrintRh :: Rhine IO Second () ()
rhPrintRh = rhPrint "Hello haskell" @@ waitClock

--------------------------------------------------
-- User input 
--------------------------------------------------
rhGetLine :: Monad m => ClSF m StdinClock () String
rhGetLine = tagS

rhPrintMyLine :: ClSF IO StdinClock () ()
rhPrintMyLine = rhGetLine >>> arrMCl putStrLn

rhPrintMyLineRh :: Rhine IO StdinClock () ()
rhPrintMyLineRh = rhPrintMyLine @@ StdinClock

--------------------------------------------------
-- Quitting
--------------------------------------------------
rhValidate :: Monad m => ClSF (ExceptT () m) cl String String
rhValidate = proc str -> do
  throwOn' -< (str == "q", ())
  returnA -< str

rhValidatePrint :: ClSF (ExceptT () IO) StdinClock () ()
rhValidatePrint = rhGetLine >>> rhValidate >>> runClSFExcept (safe (arrMCl putStrLn))

rhUseInput :: ClSFExcept IO StdinClock () () Void
rhUseInput = do
  try rhValidatePrint
  once_ exitSuccess

rhUseInputSafe :: ClSF IO StdinClock () ()
rhUseInputSafe = safely rhUseInput

rhUseInputSafeRh :: Rhine IO StdinClock () ()
rhUseInputSafeRh = rhUseInputSafe @@ StdinClock

--------------------------------------------------
-- Using exceptions (This section doesn't work
-- but is left here as an example)
--------------------------------------------------
rhCheckInput :: Monad m => ClSF (ExceptT String m) cl String String
rhCheckInput = proc str -> do
  throwOn' -< (str == "q" || str == "Hello", str)
  returnA  -< str

rhCheckPrint :: ClSF (ExceptT String IO) StdinClock () ()
rhCheckPrint = rhGetLine >>> rhCheckInput >>> runClSFExcept (safe (arrMCl putStrLn))

rhCheckUseInput :: ClSFExcept IO StdinClock () () Void
rhCheckUseInput = do
  str <- try rhCheckPrint
  case str of
    "q"     -> once_ exitSuccess
    "Hello" -> do
      once_ $ putStrLn "Hi!"
      rhCheckUseInput
    _       -> once_ exitFailure

rhCheckUseInputSafe :: ClSF IO StdinClock () ()
rhCheckUseInputSafe = safely rhCheckUseInput

rhCheckUseInputSafeRh :: Rhine IO StdinClock () ()
rhCheckUseInputSafeRh = rhCheckUseInputSafe @@ StdinClock

--------------------------------------------------
-- Combining clocks
--------------------------------------------------
type Second5 = Millisecond 5000

rhPrint1S :: ClSF IO Second () ()
rhPrint1S = constMCl $ print "Every 1s."

rhPrint5S :: ClSF IO Second5 () ()
rhPrint5S = constMCl $ print "Every 5s."

rhPrint1SSN :: SN IO Second () ()
rhPrint1SSN = Synchronous rhPrint1S

rhPrint5SSN :: SN IO Second5 () ()
rhPrint5SSN = Synchronous rhPrint5S

rhPrintComboSN :: SN IO (ParClock IO Second Second5) () ()
rhPrintComboSN = rhPrint1SSN |||| rhPrint5SSN

rhPrintComboClock :: ParClock IO Second Second5
rhPrintComboClock = ParallelClock waitClock waitClock scheduleMillisecond

rhPrintComboRh :: Rhine IO (ParClock IO Second Second5) () ()
rhPrintComboRh = Rhine rhPrintComboSN rhPrintComboClock

--------------------------------------------------
-- Time parallel Rhine composition
--------------------------------------------------
rhPrint1SRh :: Rhine IO Second () ()
rhPrint1SRh = rhPrint1S @@ waitClock

rhPrint5SRh :: Rhine IO Second5 () ()
rhPrint5SRh = rhPrint5S @@ waitClock

rhPrintComboRhV2 :: Rhine IO (ParClock IO Second Second5) () ()
rhPrintComboRhV2 = rhPrint1SRh ||@ scheduleMillisecond @|| rhPrint5SRh

--------------------------------------------------
-- Scheduling non-deterministic clocks
--------------------------------------------------
rhPrintComboAndInputRh :: Rhine IO (ParallelClock IO (ParClock IO Second Second5) StdinClock) () ()
rhPrintComboAndInputRh = rhPrintComboRhV2 ||@ concurrently @|| rhUseInputSafeRh

--------------------------------------------------
-- Sequential Millisecond
--------------------------------------------------
type Second2 = Millisecond 2000
type Second3 = Millisecond 3000

rhGiveEvery1Rh :: Monad m => Rhine m Second () Int
rhGiveEvery1Rh = arr (const 5) @@ waitClock

rhGiveEvery3Rh :: Monad m => Rhine m Second3 () Int
rhGiveEvery3Rh = arr (const 5) @@ waitClock

rhTakeEvery2Rh :: Rhine IO Second2 Int ()
rhTakeEvery2Rh = arrMCl print @@ waitClock

rhGiveAndTake1 :: Rhine
  IO
  (SequentialClock IO Second Second2)
  ()
  ()
rhGiveAndTake1 = rhGiveEvery1Rh >-- keepLast 1 -@- scheduleMillisecond --> rhTakeEvery2Rh

rhGiveAndTake2 :: Rhine
  IO
  (SequentialClock IO Second3 Second2)
  ()
  ()
rhGiveAndTake2 = rhGiveEvery3Rh >-- keepLast 1 -@- scheduleMillisecond --> rhTakeEvery2Rh

--------------------------------------------------
-- downsampleMillisecond
--------------------------------------------------
rhGiveAndTake3 :: Rhine IO (SequentialClock IO Second Second2) () ()
rhGiveAndTake3 =
  rhGiveEvery1Rh
  >-- (downsampleMillisecond >>-^ arr V.head)
  -@- scheduleMillisecond
  --> rhTakeEvery2Rh

--------------------------------------------------
-- Different clocks
--------------------------------------------------
rhValidateString :: ClSF (ExceptT () IO) StdinClock () String
rhValidateString = rhGetLine >>> rhValidate

rhGetInput :: ClSFExcept IO StdinClock () String Void
rhGetInput = do
  try rhValidateString
  once_ exitSuccess

rhGetInputSafeRh :: Rhine IO StdinClock () String
rhGetInputSafeRh = safely rhGetInput @@ StdinClock

rhPutStringLnRh :: Rhine IO Second2 String ()
rhPutStringLnRh = arrMCl putStrLn @@ waitClock

rhThing1Rh :: Rhine IO (SequentialClock IO StdinClock Second2) () ()
rhThing1Rh =
  rhGetInputSafeRh
  >-- keepLast "Nothing yes."
  -@- concurrently
  --> rhPutStringLnRh

rhPutStringLnMaybe :: ClSF IO Second2 (Maybe String) ()
rhPutStringLnMaybe = proc mStr ->
  case mStr of
    Just str -> (arrMCl putStrLn) -< str
    Nothing -> (arrMCl putStrLn) -< "Waiting..."

rhPutStringLnMaybeRh :: Rhine IO Second2 (Maybe String) ()
rhPutStringLnMaybeRh = rhPutStringLnMaybe @@ waitClock

rhThing2Rh :: Rhine IO (SequentialClock IO StdinClock Second2) () ()
rhThing2Rh =
  rhGetInputSafeRh
  >-- fifoBounded 5
  -@- concurrently
  --> rhPutStringLnMaybeRh

--------------------------------------------------
-- Interpolation buffers
--------------------------------------------------
rhGetDoubleMaybe :: ClSF IO StdinClock () (Maybe Double)
rhGetDoubleMaybe = safely rhGetInput >>> arr readMaybe

rhMaybeDoubleStateful :: ClSF IO cl (Maybe Double, Double) (Double, Double)
rhMaybeDoubleStateful = proc (mayBud, last) -> do
  case mayBud of
    Just dub -> returnA -< (dub, dub)
    Nothing  -> returnA -< (last,last)

rhMaybeToDouble :: ClSF IO cl (Maybe Double) Double
rhMaybeToDouble = feedback 0 rhMaybeDoubleStateful

rhGetDouble :: Rhine IO StdinClock () Double
rhGetDouble = (@@ StdinClock) $ rhGetDoubleMaybe >>> rhMaybeToDouble

rhTestGetDouble :: Rhine IO StdinClock () ()
rhTestGetDouble = (@@ StdinClock) $ rhGetDoubleMaybe >>> rhMaybeToDouble >>> arrMCl print

rhPrintDubRh :: Rhine IO Second Double ()
rhPrintDubRh = arrMCl print @@ waitClock

rhSincDubRh :: Rhine IO (SequentialClock IO StdinClock Second) () ()
rhSincDubRh = rhGetDouble >-- sinc 100 -@- concurrently --> rhPrintDubRh

rhCubicDubRh :: Rhine IO (SequentialClock IO StdinClock Second) () ()
rhCubicDubRh = rhGetDouble >-- cubic -@- concurrently --> rhPrintDubRh

rhLinearDubRh :: Rhine IO (SequentialClock IO StdinClock Second) () ()
rhLinearDubRh = rhGetDouble >-- linear 0 0 -@- concurrently --> rhPrintDubRh

--------------------------------------------------
-- Behavior functions
--------------------------------------------------
rhPrintIntegral :: ClSF IO Second () ()
rhPrintIntegral = arr (const (10 :: Double)) >>> integral >>> arrMCl print

--------------------------------------------------
-- Periodic and hoist clocks
--------------------------------------------------
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

--------------------------------------------------
-- Select Clock
--------------------------------------------------
type SelectQ = SelectClock StdinClock Bool

checkQ :: ClSF IO SelectQ () Bool
checkQ = tagS

quitProgram :: ClSF IO SelectQ Bool ()
quitProgram = proc b -> if b
                        then constMCl exitSuccess -< ()
                        else returnA -< ()

quitRh :: Rhine IO SelectQ () ()
quitRh = checkQ >-> quitProgram @@ SelectClock StdinClock
         (\str -> if str == "q"
                  then Just True
                  else Nothing)

checkOrPrintRh :: Rhine IO (ParallelClock IO StdinClock SelectQ) () ()
checkOrPrintRh = rhPrintMyLineRh ||@ schedSelectClockAndMain @|| quitRh 
