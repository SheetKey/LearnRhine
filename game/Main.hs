{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows #-}

module Main where

import FRP.Rhine
import FRP.Rhine.ClSF.Except 

import Control.Monad.Schedule
import Control.Concurrent
import Data.Void

import qualified Data.Vector.Sized as V

import qualified SDL

main :: IO ()
main = print 2

{--------------------------------------------
Basic SDL
-}-------------------------------------------
main1 :: IO ()
main1 = do
  SDL.initializeAll
  window <- SDL.createWindow "Test" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  threadDelay 5000
  SDL.destroyWindow window
