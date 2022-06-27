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
Now that we have our clock, lets make use of it. We'll create a function that
takes some showable input and returns a `ClSF`.

NOTE: From here on out functions involving the rhine library will begin with `rh`.
Functions of the `Rhine` type will end with `Rh`. It seems to be common (at least in the 
examples provide by rhine) to put name `Rhine`s the same thing as their `ClSF` counterparts
but with `Rh` at the end. I'm adding `rh` to the front of everything for my own clarity.

```haskell
rhPrint :: Show a => a -> ClSF IO Second () ()
rhPrint = constMCl . print
```

`rhPrint` lifts `print` into a `ClSF` context. From the type signature we see that 
we are using the IO monad and the `Second` clock. Our signal function takes no input and
produces not output. Only side effects. To have something we can use as a main function,
we need to create a `Rhine`.

```haskell
rhPrintRh :: Rhine IO Second () ()
rhPrintRh = rhPrint "Hello haskell" @@ waitClock
```

Sine `@@` takes a `ClSF`, we have to apply an argument to `rhPrint`, in this case 
"Hello haskell". `@@` also takes a clock. `waitClock` creates a millisecond clock, and
we will use this often. It knows from the type signature that it is a 1000 millisecond clock,
so `waitClock` takes no arguments. The benefit of declaring at the type level the speed of the 
clock is that, when we get to composing signal function, we will be given nice type errors
if we try to compose `ClSF`'s of different speeds.

Finally we can have a main function using `flow`.

```haskell
main = flow rhPrintRh
```

This program will print "Hello haskell" every second, forever. 


## User input

In the previous example we used the `Millisecond` clock, which allowed us to do something
once every second. While this is often very useful, rhine extends the idea of clocks 
considerably. Rhine uses the `EventClock` type to represent clocks that tick whenever
an event happens. For now, we will stick to `StdinClock`, an event clock for std input.

Clocks in rhine have associated types of `Time` and `Tag`. We'll come back to `Time` later.
For `StdinClock`, the `Tag` is a `String`. Whenever a clock ticks it emits a time and a tag.
In the case of the `StdinClock`, when the user hits `Enter`, the clokc emits the sting that 
the user typed. There are several functions for getting information from the clock, but for now
all we care about is getting the string from the user. `tagS` will give us this string.

```haskell
tagS :: Monad m => ClSF m cl a (Tag cl)
```

So `tagS` is a signal function that seems to ignore input and give us the tag of the clock,
whenever the clock ticks. We'll use this as a replacement for `getLine`.

```haskell
rhGetLine :: ClSF IO StdinClock () String
rhGetLine = tagS
```

Now we have a clocked signal function that ticks when the user hits enter, and gives us 
a string. To do something with this string, we will compose our first two `ClSF`'s. 
Technically there's some complicated stuff with arrows going on, but knowing very little
about arrows hasn't stopped me yet. We will use `(>>>)`, which for our purposes has the type
signature

```haskell
(>>>) :: ClSF m cl a b -> ClSF m cl b c -> ClSF m cl a c
```

This function essentially pipes the output from the first `ClSF` into the next `ClSF`.
`rhGetLine` has input `()`, and in order to have something we can use as a main function,
we need to compose it with some function that takes a `String` ans returns `()`. Why don't
we just print the string back to the user.

```haskell
rhPrintMyLine :: ClSF IO StdinClock () ()
rhPrintMyLine = rhGetLine >>> arrMCl putStrLn
```

Her we use `arrMCl`, which turns `putStrLn` into a signal function. Since `rhGetLin` has
clock type `StdinClock`, `rhPrintMyLin` MUST have the same clock type. When composeing
with `(>>>)`, both `ClSF`'s must have the same clock type and the resulting `ClSF` will
also have the same clock type. This is one of the key benefits of using rhine. We will
learn about composing functions with different clocks lates.

We're not quite done yet. We still need a `Rhine` to be used with `flow`. Whereas we
used `waitClock` to create a millisecond clock, we will use the `StdinClock` data constructor
for `StdinClock` type.

```haskell
rhPrintMyLineRh :: Rhine IO StdinClock () ()
rhPrintMyLineRh = rhPrintMyLine @@ StdinClock
```

Now we have a main,

```haskell
main :: IO ()
main = flow rhPrintMyLineRh
```

that, upon getting user input, prints the user input, and waits for more user input.
