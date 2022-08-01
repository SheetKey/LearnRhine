{-# LANGUAGE Arrows #-}

module FRP.Rhine.SDL.Process.Active
  ( removeInactive
  ) where

import FRP.Rhine.SDL.Entity

checkActive :: [Entity] -> [Entity]
checkActive []     = []
checkActive (e:es) = if isActive e
                     then e : checkActive es
                     else checkActive es

removeInactive :: Monad m => ClSF m cl [Entity] [Entity]
removeInactive = proc lst -> returnA -< checkActive lst
