{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows #-}

module Main where

import FRP.Rhine
import FRP.Rhine.ClSF.Except


import Control.Monad ( guard )
import System.Exit (exitSuccess, exitFailure)
import System.IO (hFlush, stdin, stdout)

main :: IO ()
main = flow rhPrintMyLineRh

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



