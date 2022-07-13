{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows #-}

module Example1
  ( test
  )where

import FRP.Rhine
import System.Exit (exitSuccess, exitFailure)

rhValidate :: Monad m => ClSF (ExceptT () m) cl String String
rhValidate = proc str -> do
  throwOn' -< (str == "q", ())
  returnA -< str

rhValidateString :: ClSF (ExceptT () IO) StdinClock () String
rhValidateString = tagS >>> rhValidate

rhGetLine :: ClSF IO StdinClock () String
rhGetLine = safely $ do
  try rhValidateString
  once_ exitSuccess

rhGetLineRh :: Rhine IO StdinClock () String
rhGetLineRh = rhGetInput @@ StdinClock

type XVel = Double
type YVel = Double
type Vel = (XVel, YVel)
data Direction =
    North
  | East
  | South
  | West
  | NorthEast
  | NorthWest
  | SouthEast
  | SouthWest
  | Still
  deriving (Eq, Show)

instance Semigroup Direction where
  a <> a = a
  a <> Still = a
  Still <> a = a
  North <> East = NorthEast
  North <> West = NorthWest
  North <> NorthEast = North
  North <> NorthWest = North
  North <> _ = Still
  East  <> North = NorthEast
  East  <> South = SouthEast
  East  <> NorthEast = East
  East  <> SouthEast = East
  East  <> _ = Still
  South <> East = SouthEast
  South <> West = SouthWest
  South <> SouthEast = South
  South <> SouthWest = South
  South <> _ = Still
  West  <> North = NorthWest
  West  <> South = SouthWest
  West  <> NorthWest = West
  West  <> SouthWest = West
  West  <> _ = Still
  NorthEast <> North = North
  NorthEast <> East = East
  NorthEast <> SouthEast = South
  NorthEast <> NorthWest = North
  NorthEast <> _ = Still
  NorthWest <> North = North
  NorthWest <> West = West
  NorthWest <> NorthEast = North
  NorthWest <> SouthWest = West
  NorthWest <> _ = Still
  SouthEast <> South = South
  SouthEast <> East = East
  SouthEast <> SouthWest = South
  SouthEast <> NorthEast = East
  SouthEast <> _ = Still
  SouthWest <> South = South
  SouthWest <> West = West
  SouthWest <> SouthEast = South
  SouthWest <> NorthWest = West
  SouthWest <> _ = Still
  
  

checkDir :: String -> Direction
checkDir []     = Still
checkDir (c:cs) | c == "w" = North <> checkDir cs
                | c == "a" = West  <> checkDir cs
                | c == "s" = South <> checkDir cs
                | c == "d" = East  <> checkDir cs

test = print $ checkDir "wa"
