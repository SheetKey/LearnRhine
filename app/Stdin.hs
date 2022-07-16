{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows #-}

module Stdin
  ( test
  ) where

import FRP.Rhine
import System.Exit (exitSuccess, exitFailure)


test :: IO ()
test = putStrLn "Hello"
