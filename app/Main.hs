{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows #-}

module Main where

import FRP.Rhine
    ( returnA,
      (>>>),
      safe,
      safely,
      arrMCl,
      constMCl,
      once_,
      runClSFExcept,
      throwOn',
      try,
      tagS,
      scheduleMillisecond,
      waitClock,
      flow,
      concurrently,
      schedPar1,
      schedPar1',
      schedPar2,
      schedPar2',
      (@@),
      (@||),
      (||@),
      (||||),
      (>--),
      (-@-),
      (-->),
      Empty,
      ClSF,
      ClSFExcept,
      Millisecond,
      StdinClock(..),
      SN(Synchronous),
      ParClock,
      ParallelClock(ParallelClock),
      Rhine(Rhine),
      ExceptT,
      Arrow (arr), keepLast, SequentialClock, downsampleMillisecond, (>>-^), arrM, fifoBounded)
import FRP.Rhine.ClSF.Except ()

import qualified Data.Vector.Sized as V

import Control.Monad ( guard )
import System.Exit (exitSuccess, exitFailure)
import System.IO (hFlush, stdin, stdout)

main :: IO ()
main = flow rhThing2Rh

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

rhUseInput :: ClSFExcept IO StdinClock () () Empty
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

rhCheckUseInput :: ClSFExcept IO StdinClock () () Empty
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

rhGetInput :: ClSFExcept IO StdinClock () String Empty
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
