# Learn Rhine

This is my personal attempt at both learning and documents [rhine](https://github.com/turion/rhine). Rhine is an intriguing FRP library built on [dunai](https://github.com/ivanperez-keera/dunai), another intruiging FRP library. Dunai [claims](http://www.cs.nott.ac.uk/~psxip1/papers/2016-HaskellSymposium-Perez-Barenz-Nilsson-FRPRefactored-short.pdf) to address many issues with other FRP libraries by creating a foundational library for FRP. Rhine adds [type level clocks](https://www.manuelbaerenz.de/files/Rhine.pdf) to dunai, allowing for input event handling and the management of many subsystems running on different time scalse. Documentation for rhine is sparse ([this](https://www.manuelbaerenz.de/files/Rhine.pdf) is pretty much it asside from a few examples), so I hope to create some. 

## Hello haskell with rhine

```haskell
main :: IO ()
main = putStrLn "Hello world."
```
