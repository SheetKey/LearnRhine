module SDLInit.SDLInit where

import Rhine
import qualified SDL

sdlInitAndFlow :: (SDL.Window -> SDL.Renderer -> Rhine IO cl () ()) -> IO ()
sdlInitAndFlow rhine = do
  SDL.initializeAll
  window <- SDL.createWindow "Test" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  flow $ rhine window renderer
  
