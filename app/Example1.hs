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
rhGetLineRh = rhGetLine @@ StdinClock

type XVel = Double
type YVel = Double
type Vel = (XVel, YVel)
  
xVel :: String -> XVel
xVel [] = 0
xVel (c:cs) | c == 'a' = (-5) + xVel cs
            | c == 'd' = 5    + xVel cs
            | otherwise = xVel cs

yVel :: String -> YVel
yVel [] = 0
yVel (c:cs) | c == 'w' = 5    + yVel cs
            | c == 's' = (-5) + yVel cs
            | otherwise = yVel cs

vel :: String -> Vel
vel str = (xVel str, yVel str)
-- I NEED TO NORMALIZE THE VELOCITY
test :: IO ()
test = print $ vel "wa"
