module ExampleObject where

import FRP.Rhine
import qualified SDL
import qualified SDL.Image as SDLI
import SDLRenderable

data Object = Object
  { filePath :: String
  }

instance Renderable Object where
  getTexture a ren = SDLI.loadTexture ren $ filePath a

  renderClSF a ren = proc pnt -> do
    let texture = getTexture a ren
    arrMCl (\tex -> do
               SDL.clear ren
