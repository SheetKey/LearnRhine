module FRP.Rhine.SDL.Process.Collision where

import FRP.Rhine

import FRP.Rhine.SDL.Components

doCollision :: Position -> Position -> Bool
doCollision (Position x1 y1 w1 h1) (Position x2 y2 w2 h2) =
  if x1 + w1 >= x2 &&
     x2 + w2 >= x1 &&
     y1 + h1 >= y2 &&
     y2 + h2 >= y1
  then True
  else False

checkCollision Entity -> Entity -> Bool
checkCollision e1 e2 =
  case (getMPosiont e1, getMPosition e2) of
    (Nothing, _) -> False
    (_, Nothing) -> False
    (Just e1P, e2P) ->

      case (getMCollision e1, getMCollision e2) of
        (Nothing, _) -> False
        (_, Nothing) -> False
        (Just e1C, Just e2C) ->

          case (canHit e1C, canHit e2C) of
            (False, _) -> False
            (_, False) -> False
            (True, True) -> doCollision (setWH e1P $ hitBox e1C) (setWH e2P $ hitBox e2C)

collideOne :: Entity -> [Entity] -> (Entity, [Entity])
collideOne e1 ents = (newE1, acc)
  where collideAcc :: (Entity, [Entity], [Entity]) -> (Entity, [Entity], [Entity])
        collideAcc (e1, [], acc) = (e1, [], acc)
        collideAcc (e1, (e:es), acc) = if checkCollision e1 e
                                       then case (getMCollision e1, getMCollision e) of
                                              (Nothing, _) -> error "Collision doesn't exist."
                                              (_, Nothing) -> error "Collision doesn't exist."
                                              (Just c1, Just c2) ->
                                                collideAcc (hitOther c2 e1
                                                           , es
                                                           , acc ++ hitOther c1 e)
        (newE1, _, acc) = collideAcc (e1, ents, [])

collideAll :: [Entity] -> [Entity]
collideAll []     = []
collideAll (e:es) =
  case getMCollision e of
    Nothing -> e : collideAll es
    Just _  -> let (newE, newES) = collideOne e es
               in newE : collideAll newES



collide :: Monad m => ClSF m cl [Entity] [Entity]
collide = arr collideAll
  
