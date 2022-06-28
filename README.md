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


## Control flow

In rhine, control flow is dealt with throw `ExceptT` and `ClSFExcept`. This section will
be quite a bit more complcated than previous sections, but will allow us to write much more
complicated programs. Lets set the goal of adding a quitting mechanism to our `rhPrintMyLineRh`
program. When the user enters "q", we want to quit, rather than print the input string. We still
want a to get the line the user types, so we won't change `rhGetLine`.

When working with `ExceptT` in rhine, we have two different types. The first is a
`ClSF` with the `ExceptT e m` monad. The second is the `ClSFExcept` type. 
We will define function that create a split with `ClSF (ExceptT e m) cl a b` and
will manage the flow of splits with `ClSFExcept m cl a b e` (where `e` is the exception type).

`ClSFExcept` is a monad, so we will use do notation to manage flow. The `ClSFExcept` monad is
quite different than any monad I'm familiar with, so I'll try to go through it a bit slowly.
First, our goal when using `ClSFExcept` is to handle every exception. We do this so that
we can then use the `safely` function.

```haskell
safely :: Monad m => MSFExcept m a b Empty -> MSF m a b
```

We can see from the type signiture that we cannot allow for any exceptions, and the type
signature will help us ensure we have caught every exception.

In the `ClSFExcept` monad, (forgive me for probably incorrect terminology) we move to the next
line in the do block only when the current line has thrown an exception. `once_` is very
useful.

```haskell
once_ :: Monad m => m e -> ClSFExcept m cl a b e
```

Once takes some monad, often some IO action, and returns a `ClSFExcept`. In a do block,
when we call `once_ $ putStrLn "hello"`, the argument to `once_` is evaluated, and the resulting
value is immediately emmited as an exception, moving us to the next line in the do block. In this
case the value is `()`, but if there was some value, we could get it through monad bind the normal
way. This brings us to the key point: `ClSFException`'s monad bind uses the exception value. 

The other important function to use in the `ClSFException` monad is `try`.

```haskell
try :: Monad m => ClSF (ExceptT e m) cl a b -> ClSFExcept m cl a b e
```

Try takes a `ClSF` and runs it repeatedly until an exception is thrown. We can use monad bind
to get the exception value, or discard the value and move on to the next line in the do block.

For our `ClSF`, we want to accept a string (from `rhGetLin`) and return a string, throwing an
exception when the string is "q". Our exception type will be `()` since we don't care about the
value "q". Thus our type signiture is as follows
(Its good practice to not specify the clock or monad types whenever possible.)

```haskell
rhValidate :: Monad m => ClSF (ExceptT () m) cl String String
```

To write this function we are going to use arrow notation (Ensure you have 
`{-# LANGUAGE Arrows #-}` enabled). While starting to learn rhine, I know nothing about arrows,
and I have managed so far. If you would like to learn about arrows I'm sure there are many 
good resources. I care more about using them as opposed to the theory (for now). My explanation
will be by analogy as I understand them, which is not very well. If I get to a point where I need
to know more, or have the time to learn, I will make some edits.

Arrow notation gives us a new kind of lambda called `proc`, that has its own do notation.
In rhine, many things that aren't necessarily monads are arrows. `ClSF` is an arrow, for example.
We start defining `rhValidate`,

```haskell
rhValidate :: Monad m => ClSF (ExceptT () m) cl String String
rhValidate = proc str -> do
```

`proc str ->` is analagous to `\str ->`. Instead of using a lambda to define an anonymous function
with argument `str`, we use `proc` to define an anonmous arrow with argument `str`. 
From our type signature for `rhValidate`, we know `str` has type `String`.
Next we want to throw an exception when `str == "q"`. To do this we will use `throwOn'`.

```haskell
throwOn' :: Monad m => ClSF (ExceptT e m) cl (Bool, e) ()
```

We can see that `throwOn'` take a tuple of bool and `e`. If an exception is not thrown, it
returns `()`. If an exception is thrown it throws `e`. 

```haskell
rhValidate :: Monad m => ClSF (ExceptT () m) cl String String
rhValidate = proc str -> do
  throwOn' -< (str == "q", ())
``` 

The `-<` syntac is arrow notation an feeds the argument into the arrow. We now have a function
that throws an exception `()` when we input "q", but this function will not type check as is,
because in the case that `throwOn'` does not throw an exception, we are currently returing 
`()`, since `throwOn'` returns `()`. Thus we need to specify a return value in the 
case that we don't throw an exception. `returnA` is like the monad `return` but for arrows.
Just like `throwOn'`, we have to feed it a value using `<-`. Our final function is

```haskell
rhValidate :: Monad m => ClSF (ExceptT () m) cl String String
rhValidate = proc str -> do
  throwOn' -< (str == "q", ())
  returnA  -< str
``` 

Now to practice composing `ClSF`'s, lets make a function that inputs `()`, and returns `()`, but
has the side effects of getting user input, validating the input potentially throwing an
exception, and then printing the input assuming no exceptions were thrown.
This function will have type

```haskell
rhValidatePrint :: ClSF (ExceptT () IO) StdinClock () ()
```

Since we want to start by getting user input and then validating user input, we start with

```haskell
rhValidatePrint :: ClSF (ExceptT () IO) StdinClock () ()
rhValidatePrint = rhGetLine >>> rhValidate
```

This won't typecheck for a couple reasons. The import one is that `rhGetLine` has the IO monad,
and we need it to have the `ExceptT () IO` monad. This is fixed by adjusting `rhGetLine`'s 
type signiture alone:

```haskell
rhGetLine :: Monad m => ClSF m StdinClock () String
```

Now we are getting input and throwing an exception if the input is "q". If it isn't "q",
we want to print the input. This involves `putStrLn`. Obviously we can't just use
`putStrLn`, its not a `ClSF`. So we start with 

```haskell
(arrMCl putStrLn) :: ClSF IO StdinClock String ()
```

This is close to what we want, by its the wrong monad. 
I mentioned the `safely` function earlier which turns a `ClSFExcpet` into a `ClSF` with the
condition that all exceptions are handled. Well when we have a `ClSF`, there are no exceptions,
so it is safe to turn it into a `ClSFExcept` with the `safe` function.

```haskell
safe :: Monad m => MSF m a b -> MSFExcept m a b e
```

So now we have

```haskell
(safe (arrMCl putStrLn)) :: ClSFExcept IO StdinClock String () ()
```

(The extra `()` at the end is the exception type (recall the type sigiture for `ClSFExcept`. 
But this will never throw an exception so no worries.) 
We're much closer to the type we want, and we have one final step.
`runClSFExcept`, similar to `runReaderT` or other `run` functions, does just what we want.

```haskell
runClSFExcept (safe (arrMCl putStrLn)) :: ClSF (ExceptT () IO) StdinClock String ()
```

This function will take out validate string and print it! Putting this all together we have

```haskell
rhValidatePrint :: ClSF (ExceptT () IO) StdinClock () ()
rhValidatePrint = rhGetLine >>> rhValidate >>> runClSFExcept (safe (arrMCl putStrLn))
```

Okay that was a lot but we're nearly there. Now we can return to talking about `ClSFExcept`.
We'll create a function

```haskell
rhUseInput :: ClSFExcept IO StdinClock () () Empty
```

This function takes nothing and should return nothing and should never throw an exception.
Instead, we will hadnle the exception thrown by `rhValidatePrint` in order to control the flow
of our program and manage desired splits.
So now we get to use `try` and `once_` that we talked about earlier.

```haskell
rhUseInput :: ClSFExcept IO StdinClock () () Empty
rhUseInput = do
  try rhValidatePrint
  once_ exitSuccess
```

That doesn't look so bad. We `try` `rhValidatePrint` over and over and over ... forever. Each time
the user hits enter, the string is printed back. But when the user types "q", an exception is
thrown. We move onto the next line, which exits the program. 

Now, we need to turn this into a `Rhine`. As mentioned above, we use the `safely function
now that we have handled all excpetions.

```haskell
rhUseInputSafe :: ClSF IO StdinClock () () 
rhUseInputSafe = safely rhUseInput
```

(It seems like this is a naming convention: put `Safe` at the end of the name when
calling `safely`.)
And finally we get our `Rhine`:

```haskell
rhUseInputSafeRh :: Rhine IO StdinClock () ()
rhUseInputSafeRh = rhUseInputSafe @@ StdinClock
```

We can put this in main:

```haskell
main :: IO ()
main = flow rhUseInputSafeRh
```

