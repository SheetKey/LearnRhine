# Learn Rhine

This is my personal attempt at both learning and documents [rhine](https://github.com/turion/rhine). Rhine is an intriguing FRP library built on [dunai](https://github.com/ivanperez-keera/dunai), another intruiging FRP library. Dunai [claims](http://www.cs.nott.ac.uk/~psxip1/papers/2016-HaskellSymposium-Perez-Barenz-Nilsson-FRPRefactored-short.pdf) to address many issues with other FRP libraries by creating a foundational library for FRP. Rhine adds [type level clocks](https://www.manuelbaerenz.de/files/Rhine.pdf) to dunai, allowing for input event handling and the management of many subsystems running on different time scalse. Documentation for rhine is sparse ([this](https://www.manuelbaerenz.de/files/Rhine.pdf) is pretty much it asside from a few examples), so I hope to create some. 

## Hello haskell with rhine

Rather than use do notation and monad stacks with `IO` to do things, 
we want to let rhine handle everything. 
Rhine provides several key types that we will use to create programs.
We will start with our goal type. Rhine provides the `Rhine` type, with accompanying
`flow` function. `Rhine` is analagous to `StateT a IO ()` and `flow` to `evalState`, were
we using `StateT` to manage our program.

```haskell
flow :: (Monad m, Clock m cl, Time cl ~ Time (In cl), Time cl ~ Time (Out cl)) 
     => Rhine m cl () () -> m ()
```
```haskell
data Rhine m cl a b
```

The type constraints for `flow` is can largely be ignored at present. The constrains for `cl`
help ensure clock safety, which will be explained shortly. In all type signature, `cl` refers
to a clock.

Then it can be seen that `Rhine` has some monad `m`, a clock `cl`, an input `a`, and and
output `b`. While `Rhine` is useful, we won't be looking to carefully at its data constructors.
We typically create a `Rhine` directly, but will use the `@@` combinator.

```haskell
(@@) :: (cl ~ In cl, cl ~ Out cl) => ClSF m cl a b -> cl -> Rhine m cl a b infix 5
```

This function takes a `ClSF` and a clock and gives us a `Rhine`. When we need a `Rhine`, this
is what we will do (at least to start with).

Lets dive into `ClSF` first. The name means clock signal function, and is a signal function 
(just like in other FRP libraries I think) with a clock (you're not meant to know what a
signal function is). So how do we make `ClSF`s?

We have a few methods to create a `ClSF`:

```haskell
arr :: (a -> b) -> ClSF m cl a b
```
```haskell
arrMCl :: (a -> m b) -> ClSF m cl a b
```
```haskell
constMCl :: Monad m => m b -> ClSF m cl a b
```

`arr` turns a pure function into a `ClSF` that will be performed at every tick of the
clock `cl`. `arrMCL` does the same but for unpure functions.
`constMCl` produces a `ClSF` that ignores input.
At this point the only mention
of a clock has been in the type signature. It is when we turn a `ClSF` into a `Rhine` that
an actual clock is provided.

So how do we get user input with rhine?
We need a clock! For our first program we'll use the `Millisecond` clock.
The `Millisecond` clock is a type level signifier for clock type.
It takes an argument `n :: Nat` and ticks every `n` milliseconds.
(Make sure to have `{-# LANGUAGE DataKinds #-}` enabled.) 
For more readable type signatures, we will create type synonyms for all the millisecond clocks
we use.

```haskell
type Second = Millisecond 1000
```

`Second` is a clock type that will tick every second. 








Well clocks in rhine have associated types of `Time` and `Tag`. 
