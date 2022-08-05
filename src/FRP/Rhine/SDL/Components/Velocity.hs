module FRP.Rhine.SDL.Components.Velocity where

import FRP.Rhine

import Foreign.C.Types

-- | A velocity is a tuple of 'Double's. This has an instance in the 'VectorSpace' typeclass.
type Velocity = (Double, Double)

-- | Normalize a vector ('Velocity') without throwing and error
--   on the zero vector.
normalizeSafe :: (VectorSpace v a) => v -> v
normalizeSafe v = if nv == 0 then v else v ^/ nv
  where nv = norm v
