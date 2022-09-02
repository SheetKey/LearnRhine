module FRP.Rhine.MSF.Delay.Proxy where

data DelayProxy delay where
  LeafDelay :: DelayProxy delay

class GetDelayProxy delay where
  getDelayProxy :: DelayProxy delay

  default getDelayProxy
    :: DelayProxy delay
  getDelayProxy = LeafDelay
  
class ToDelayProxy a where
  type Dl a :: Type

  toDelayProxy :: a -> DelayProxy (Dl a)

  default toDelayProxy
    :: GetDelayProxy (Dl a)
    => a -> DelayProxy (Dl a)
  toDelayProxy _ = getDelayProxy
