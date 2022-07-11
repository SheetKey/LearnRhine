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

Now, we need to turn this into a `Rhine`. As mentioned above, we use the `safely` function
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

### Control flow warnings

I ran into a few strange things trying to expand this example. I started by adding another
exception

```haskell
rhCheckInput :: Monad m => ClSF (ExceptT String m) cl String String
rhCheckInput = proc str -> do
  throwOn' -< (str == "q" || str == "Hello", str)
  returnA  -< str
```

This is the same as our previous `rhValidate` but now we throw an exception if the input is
"Hello". Now we can change our exception handling function to provide a nice response
when someone types "Hello". Attempt 1 (`rhCheckPrint` is the same as `rhValidatePrint`):

```haskell
rhCheckUseInput :: ClSFExcept IO StdinClock () () Empty
rhCheckUseInput = do
  str <- try rhCheckPrint
  case str of
    "q"     -> once_ exitSuccess
    "Hello" -> do
      once_ $ putStrLn "Hi!"
      rhCheckUseInput
    _       -> once_ exitFailure
```

This looks good, right? When this is run with `flow`,
(after having used `safely` and turned it into a `Rhine`)
when "Hello" is input, "Hi!" is printed repeatedly forever. This seemed very stange to
me at first. What is happening is that the `flow` function ticks the `StdinClock`.
Our `tagS` in `rhCheckPrint` gets the string and then `rhCheckPrint` does its thing. 
When the string is "Hello", an exception is thrown and "Hi!" is printed.
This all has happened on the same clock tick. The clock has not advanced, and still holds
the same time value and tag value. So when `rhCheckUseInput` is called recursively, `tagS`
gets the same tag, which is "Hello", so we find ourselves in an infinite loop. 
(Note that in some cases we will be able to use recuresive `ClSFExcept`s very nicely,
but we have to be careful.)

Thats a problem but we can probably fix it. What if we don't make `rhCheckUseInput` 
recursive, and let `flow` deal with the looping. Attempt 2:

```haskell
rhCheckUseInput :: ClSFExcept IO StdinClock () () Empty
rhCheckUseInput = do
  str <- try rhCheckPrint
  case str of
    "q"     -> once_ exitSuccess
    "Hello" -> safe $ constMCl $ putStrLn "Hi!"
    _       -> once_ exitFailure
```

The function is no longer recursive, but that means I can't use `once_` to print "Hi!", since
once throws an exception (note that the reason `once_ exitSuccess` doesn't cause a type error
is that `exitSuccess` has the type `IO Empty` in this case, so `once_` throws the
`Empty` exception, which is nothing). 
Instead I lift `putStrLn "Hi!"` into a `ClSF` and then use `safe`
to turn the signal function in to a `ClSFExcept`. 

When this is run with `flow`, it seems to work. Input of "Hello" prints "Hi!" only once, and
then it waits for more input. But now anything I input just prints "Hi!". Other strings
aren't printed back as expceted, and "q" doesn't quit the program. This is because once
a switch has happened, its done. It won't switch back. We've matched to the "Hello" case,
and we can never get out of the "Hello" case. 

Finally a solution with help from the [internet](https://www.reddit.com/r/haskell/comments/vmvf7t/strange_behavior_with_rhine/).

```haskell
rhCheckUseInput :: ClSFExcept IO StdinClock () () ()
rhCheckUseInput = do
  str <- try rhCheckPrint
  case str of
    "q"     -> once_ exitSuccess
    "Hello" -> once_ $ putStrLn "Hi!"
    _       -> once_ exitFailure
	
main :: IO ()
main = flow $ (fmap (const ()) . exceptS . runMSFExcept $ checkUseInput) @@ StdinClock
```

We've changed our exception type from `Empty` to `()`, as well as our main. We no longe have
an all exceptions handled `ClSFExcept` function so we can't use `safely`. Instead we 
use `runMSFExcept` to get a type `ClSF (ExceptT () IO) StdinClock () ()`. 
Then `exceptS` to get a type `ClSF IO StdinClock () (Either () ())`. (We haven't talked
about `exceptS` but all it does is turn the result type into `Either error result`.) 
Then our we really have a result of `IO (Either () ())` so we `fmap` to get `IO ()` no matter
what. This works but is not satisfying. 

In the end the best approach would be to not throw an exception for "Hello", and instead
write a pure function used by `rhValidatePrint` that handles that case.

```haskell
rhValidatePrint :: ClSF (ExceptT () IO) StdinClock () ()
rhValidatePrint = rhGetLine >>> rhValidate 
                  >>> runClSFExcept (safe (arrMCl (\str -> if str == "Hello"
                                                             then putStrLn "Hi!"
                                                             else putStrLn str)))
```
	
## Combining Clocks

While what we did works just fine to demonstrate control flow in rhine, its not really the best 
way to do things. It doesn't make sense to have a single clock, the point of rhine is to have
many, and using a single `StdinClock` throughout the program can create some strange behavior.

Lets start with the basics of combining things. We have already done a little sequential
combination using `(>>>)`. This allowed us to sequence two `ClSF`s with the same clock type,
so that the output of the first is the input of the second, and all this happens within one tick
of the same clock. 

Now suppose we have two signal functions that each take no input and produce no output. 

```haskell
type Second  = Millisecond 1000
type Second5 = Millisecond 5000

rhPrint1S :: ClSF IO Second () ()
rhPrint1S = constMCl $ print "Every 1s."

rhPrint5S :: ClSF IO Second5 () ()
rhPrint5S = constMCl $ print "Every 5s."
```

(We've seen this before.) 
We want to combine these `ClSF`s, but if we try to use `(>>>)` we get a type error. 
So what can we do?

### Signal Networks

First we need to make a short segue to introduce signal networks.
In rhine `SN`s are defined

```haskell
data SN m cl a b where
  Synchronous 
    :: (cl ~ In cl, cl ~ Out cl) 
    => ClSF m cl a b -> SN m cl a b
  Sequential 
    :: (Clock m clab, Clock m clcd, Time clab ~ Time clcd, Time clab ~ Time (Out clab), Time clcd ~ Time (In clcd)) 
    => SN m clab a b 
    -> ResamplingBuffer m (Out clab) (In clcd) b c 
    -> SN m clcd c d -> SN m (SequentialClock m clab clcd) a d
  Parallel 
    :: (Clock m cl1, Clock m cl2, Time cl1 ~ Time (Out cl1), Time cl2 ~ Time (Out cl2), Time cl1 ~ Time cl2, Time cl1 ~ Time (In cl1), Time cl2 ~ Time (In cl2)) 
    => SN m cl1 a b 
    -> SN m cl2 a b 
    -> SN m (ParallelClock m cl1 cl2) a b
```

Its best to ingnore the type constraints, they just ensure clock safety.
(Also if you're unfamiliar with the `data ... where` syntax its called GATDs, you can look it
up. Its really just alternate syntax to define a product type in this case.)

So a signal network can be synchronous, in which case its just a single `ClSF` which is also
synchronous, it can be squential which we won't worry about for now, or it can be parallel.

Additionally, we have already used `Rhine`s, but heres the data type:

```haskell
data Rhine m cl a b = 
  Rhine { sn :: SN m cl a b
        , clock :: cl }
```

The `@@` operator we used to make a `ClSF` into a `Rhine` is just for convience.

```haskell
@@ = Rhine . Synchronous
```

We've already been using `SN`s!

### Parallel Signal Networks

Lets return to our example and figure out how to combine our two `ClSF`s.
First we need to make them into `SN`s.

```haskell
rhPrint1SSN :: SN IO Second () ()
rhPrint1SSN = Synchronous rhPrint1S

rhPrint5SSN :: SN IO Second5 () ()
rhPrint5SSN = Synchronous rhPrint5S
```

(Here I've added "SN" to the ends of the names. This can be done much more concisely
but I've opted to be verbose for clarity.)
Now that we have two `SN`s, we can use the `Parallel` data constructor to create a new
`SN`. We will instead use the combinator `||||` which is defined simply: `|||| = Parallel`.

```haskell
rhPrintComboSN :: SN IO (ParClock IO Second Second5) () ()
rhPrintComboSN = rhPrint1SSN |||| rhPrint5SSN
```

Our type signature has a new clock type, a `ParClock`. Interestingly, it has an associated 
monad, in this case `IO`. This is because the `Millisecond` clock type uses the system clock,
and thus has side effects in `IO`. When we combine `Millisecond` clocks, the combo also
has side effects in `IO`. We won't worry about clocks to much, and will just let the 
combinators take care of the clock type. 

Now that we have our `SN`, we want to make it a `Rhine`. But the `Rhine` data constructor
requires us to provide a clock. Previously we only had to provide an "atomic" clock, meaning
a singular clock, such as with `waitClock` for `Millisecond n` or `StdinClock` for `StdinClock`.
Now we have to create a clock with type `ParClock IO Second Second5`. 

### ParClock and Schedules

If we look at the definition of `ParClock` (Which is a type synonym for `ParallelClock`):

```haskell
data ParallelClock m cl1 cl2
  = Time cl1 ~ Time cl2
  => ParallelClock
    { parallelCl1      :: cl1
    , parallelCl2      :: cl2
    , parallelSchedule :: Schedule m cl1 cl2
    }
```

we see that we need to provide two clocks, which we know about (`Second` and `Second5`),
but we also have to provide a `Schedule`. A schedule determines when clocks will tick.
Rhine provides lots of different and useful schedules, so we don't need to look into the
type. Clocks can be scheduled deterministically or not. Non-determinist clocks can
be squeduled concurrently. Since we have two `Millisecond n` clocks, they can be 
squeduled deterministically, and rhine provides `scheduleMillisecond :: Schedule () (Millisecond n1) (Millisecond n2)` to help us do so. When creating a `Schedule`, no arguments are given.
This seems strange but it is because what would be arguments are just handled at the
type level. So lets see if we can create our clock.

```haskell
rhPrintComboClock :: ParClock IO Second Second5
rhPrintComboClock = ParallelClock waitClock waitClock scheduleMillisecond
```

And now we have our clock! (Theres no `ParClock` data constructor so we must use
the `ParallelClock` data constructor.) Now we can turn our `SN` into a `Rhine`.

```haskell
rhPrintComboRh :: Rhine IO (ParClock IO Second Second5) () ()
rhPrintComboRh = Rhine rhPrintComboSN rhPrintComboClock
```

And it works! You may notice that on the fifth tick, "Every 1s." prints immidiately before
"Every 5s." This is because the schedule evaluates the first `SN` before the second if they
happen at the same time. There are other schedules and ways to create schedules from schediles 
that can allow the user to pick the order. In this case, simply changing the order would
make "Every 5s." print before "Every 1s." on the fifth tick.

### Skipping signal networks

While there may be times when we would like to work with signal networks, we can often go straight
from `ClSF`s to `Rhine`s. I think it is useful to know whats going on so we can be more
confident when skipping steps. Just as we used `@@` to skip the `SN` step before,
we have some convient operators for time parallel composition.

Lets make our first `Rhine`s using the same functions as before

```haskell
rhPrint1SRh :: Rhine IO Second () ()
rhPrint1SRh = rhPrint1S @@ waitClock

rhPrint5SRh :: Rhine IO Second5 () ()
rhPrint5SRh = rhPrint5S @@ waitClock
```

Remembering that `Rhine`s have clocks, all we need to combine these `Rhine`s in time parallel is
a schedule. We will use the same `scheduleMilliseond` schedule as before. Rhine
gives us to operators, `||@` and `@||`. Often when combining things with different clocks, 
we will have two operators like these, folling the pattern

```haskell
thing1 op1 stuff op2 thing2
```

For now `thing1` and `thing2` are our `Rhine`s, but they could also be `SN`s. The `stuff` is
will be a schedule, but we will need additional components when we want to sequence data.

So now we can write

```haskell
rhPrintComboRhV2 :: Rhine IO (ParClock IO Second Second5) () ()
rhPrintComboRhV2 = rhPrint1SRh ||@ scheduleMillisecond @|| rhPrint5SRh
```

We can read this as "We combing `rhPrint1SRh` in parallel at the `scheduleMillisecond` rate,
then at this rate combine in parallel with `rhPrint5SRh`." This is a bit wordy, but in rhine
one or more `|` generally translates to "in parallel", and `@` to "at this rate."
We now have an equivalent program to `rhPrintComboRh`, but we haven't used any `SN`s 
directly.

### More time parallel composition

In the previous sections we say to to compose things with parallel time, such that
they have the same input and output types. We can also compose `SN`s or `Rhine`s that have 
different clocks, the same input type, but different output types. Replacing `||||` we have

```haskell
(++++) :: (Monad m, Clock m clL, Clock m clR, Time clL ~ Time clR, Time clL ~ Time (Out clL), Time clL ~ Time (In clL), Time clR ~ Time (Out clR), Time clR ~ Time (In clR)) 
       => SN m clL a b 
	   -> SN m clR a c 
	   -> SN m (ParClock m clL clR) a (Either b c)
```

Again its best to ingore the type constraints for now. We can see that the only thing thats
changed is we now return an `Either`. 

Additionally, replacing `||@` and `@||` we have `++@` and `@++` which again allow us to skip
explicitly defining `SN`s.

### Composition with non-deterministic locks

We previously looked at scheduling two different `Millisecond n` clocks, which can always be
scheduled deterministically. Suppose we want to create a singal program out of 
`rhPrintComboRhV2` and `rhUseInputSafeRh`. The clocks of each of these `Rhine`s cannot be
scheduled deterministically because of `StdinClock`. We never know when `StdinClock` will tick,
so it can't be scheduled deterministically with any clock. 

We're dealing with two `Rhine`s that each have the same input and output type, `()`. This is a
sign that we want to combine them clock parallel, which we know how to do using `||@` and `@||`.
All we need is a schedule. Since we can't use a deterministic schedule, we're left with
`concurrently`. (I first wrote the function and let HLS give me the type signature. This is 
useful because in more complicated situations we can have quite complex clock trees.)

```haskell
rhPrintComboAndInputRh :: Rhine IO (ParallelClock IO 
                                   (ParClock IO Second Second5) StdinClock) () ()
rhPrintComboAndInputRh = rhPrintComboRhV2 ||@ concurrently @|| rhUseInputSafeRh
```

### Sequential composition

Now that we thoroughly understand clock parallel composition, we return to the more challenging
topic of sequential composition. We have seen sequential composition for `ClSF`s
with `(>>>)`, but this requires the same clock type. Now we want to have sequential composition
with different clock types. Recall that when we first looked at `SN`s, we ignored the 
`Sequential` `SN` data constructor.

```haskell
Sequential 
  :: (Clock m clab, Clock m clcd, Time clab ~ Time clcd, Time clab ~ Time (Out clab), Time clcd ~ Time (In clcd)) 
  => SN m clab a b 
  -> ResamplingBuffer m (Out clab) (In clcd) b c 
  -> SN m clcd c d 
  -> SN m (SequentialClock m clab clcd) a d
```

We can see that our final `SN` will have a `SequentialClock` clocktype. Additionally,
the data constructor takes a `ResamplingBuffer` (`ResBuf` is a type synonym) which we haven't
seen before. You may know what a `ResBuf` is, but when I began with rhine I did not, so
I'll try to explain.

When we want to sequentially combine signal functions with different clocks, there are two
scenarios: either the first clock is faster or the second clock is faster (this isn't quite 
true because one of the clocks could be `StdinClock` which we know nothing about the speed,
but that isn't an issue). If the first clock is faster, then it will produce too much output 
for the second signal function to handle. In this case we need to decide what to do with all
that output. We use a `ResBuf` to store the output, and then when the second clock finally ticks,
the type of `ResBuf` decides what value to give it. `ResBuf`s include things like 
FIFO (first in first out) and LIFO (last in first out). There are both bounded and unbounded
versions which will delete values to prevent running out of memory. If the second clock is faster,
then we can still use FIFO or LIFO buffers, which actually pass a `Maybe` value. If the buffer
is empty it passes `Nothing`. We also have other buffers that we can give a default value to.
There are interpolation buffers which interpolate the input to always be able to provide a value.
This should become clearer if its not already.

Rhine gives us the ability to extend `ResBuf`s, by pre or post composing them with functions.
We can also compose `ResBuf`s in data parallel for when we have signal functions
that have been composed in data parallel. 

While we could use the `SN`s `Sequential` data constructor, we're better off just working with
`Rhine`s. Rhine gives us some nice sugar that makes thigs very easy. It resemples the pattern
we used for composing `Rhine`s in parallel, but now we have a `ResBuff`.

```haskell
rhine1 >-- resBuf -@- schedule --> rhine2
```

`-@-` creates a `ResamplingPoint`, `>--` creates a `RhineAndResamplingPoint`, and finally
`-->` creates a new `Rhine`. We don't need to worry about the intermediate types. We have 
enough power working soley with `Rhine`s, `ResBuf`s, and `Schedule`s.

So far all of our signal function took no input or output, so lets write some new ones.
To keep things simple to start with, lets just use `Millisecond n` clocks. 
Just as before,

```haskell
type Second1 = Millisecond 1000
type Second2 = Millisecond 2000
type Second3 = Millisecond 3000

rhGiveEvery1Rh :: Monad m => Rhine m Second () Int
rhGiveEvery1Rh = arr (const 5) @@ waitClock

rhGiveEvery3Rh :: Monad m => Rhine m Second3 () Int
rhGiveEvery3Rh = arr (const 5) @@ waitClock

rhTakeEvery2Rh :: Rhine IO Second2 Int ()
rhTakeEvery2Rh = arrMCl print @@ waitClock

rhGiveAndTake1 :: Rhine
  IO
  (SequentialClock IO Second Second2)
  ()
  ()
rhGiveAndTake1 = rhGiveEvery1Rh >-- keepLast 1 -@- scheduleMillisecond --> rhTakeEvery2Rh

rhGiveAndTake2 :: Rhine
  IO
  (SequentialClock IO Second3 Second2)
  ()
  ()
rhGiveAndTake2 = rhGiveEvery3Rh >-- keepLast 1 -@- scheduleMillisecond --> rhTakeEvery2Rh
```

`rhGiveEvery1Rh` and `3Rh` output `5 :: Int` every 1 and 3 seconds.
`rhTakeEvery2Rh` takes and `Int` and prints it every 2 seconds. 
I use the `keepLast` function to create a `ResBuf`. `keepLast` takes an
initial value to use in the case that it has not yet recieved any input, then it keeps the most
recent input value. `rhGiveAndTake1` always print "5" every 2 seconds. `rhGiveAndTake2` prints
"1" once and then prints "5" forever. I used the `scheduleMillisecond` schedule again because
we have `Millisecond n` clocks.

#### `downSampleMillisecond`

There is a specific `ResBuf` just for dealing with multiple `Millisecond n` clocks. 

```haskell
downsampleMillisecond :: (KnownNat n, Monad m) 
  => ResamplingBuffer m (Millisecond k) (Millisecond (n * k)) a (Vector n a)
```

We can only use this when the first clock is faster than the second clock, and when the second
clock is a natural number multiple of the first clock. So we can use this with
`rhGiveEvery1Rh` and `rhTakeEvery2Rh`, but not with `rhGiveEvery3Rh` and `rhTakeEvery2RH`.

Let try writing an alternate version of `rhGiveAndTake1` using `downSampleMillisecond`.

```haskell
rhGiveEvery1Rh >-- downSampleMillisecond -@- scheduleMillisecond --> rhTakeEvery2RH
```

If you try this, it won't type check. Thats because `downSampleMillisecond` tries to give 
`rhTakeEvery2Rh` a vector, but `rhTakeEvery2Rh` wants an `Int`. But we can fix this.

We could create our own `ResBuf` from scratch, but that seems hard. Instead, we have lots
of nice operators for creating a new `ResBuf` from an existing one. We want to use
`downSampleMillisecond`, but we don't want it to spit out a vector. We want to postcompose
a function. `>>-^` allows us to do this. (`^->>` is for precomposition.)

```haskell
(>>-^) :: Monad m 
       => ResamplingBuffer m cl1 cl2 a b 
       -> ClSF m cl2 b c 
       -> ResamplingBuffer m cl1 cl2 a c 
```

We know that the vector will never be empty, so we could safely use `V.head` or `V.last` to
get a specific element. We actually know the size of the vector since we know the clock types,
so we could use `V.index` to get a specific element of the vector. Our vector has length two
so we can just use `V.head` or `V.last`. 

```haskell
rhGiveAndTake3 :: Rhine IO (SequentialClock IO Second Second2) () ()
rhGiveAndTake3 =
  rhGiveEvery1Rh
  >-- (downsampleMillisecond >>-^ arr V.head)
  -@- scheduleMillisecond
  --> rhTakeEvery2Rh
```

#### Different clocks

Now that we know a little bit about `ResBuf`s, lets change up the clocks. 
I've rewritten some got from before that gets user input and quits the program if the
input is "q". Its nothing new, but instead of just printing the string as before, I now 
return the string.

```haskell
rhValidateString :: ClSF (ExceptT () IO) StdinClock () String
rhValidateString = rhGetLine >>> rhValidate

rhGetInput :: ClSFExcept IO StdinClock () String Empty
rhGetInput = do
  try rhValidateString
  once_ exitSuccess

rhGetInputSafeRh :: Rhine IO StdinClock () String
rhGetInputSafeRh = safely rhGetInput @@ StdinClock
```

We don't know when we'll get a string, so we need to handle the case where the second `Rhine`s
clock ticks, but there is no input string from `rhGetInputSafeRh`. Lets start with `keepLast`.
This always will give a string, so we need a `Rhine` that will accept a string.

```haskell
rhPrintStringLnRh :: Rhine IO Second2 String ()
rhPrintStringLnRh = arrMCl putStrLn @@ waitClock
```

And then we can combine them with `keepLast` as the `ResBuf` and `concurrently` as the 
schedule.

```haskell
rhThing1Rh :: Rhine IO (SequentialClock IO StdinClock Second2) () ()
rhThing1Rh =
  rhGetInputSafeRh
  >-- keepLast "Nothing yes."
  -@- concurrently
  --> rhPutStringLnRh
```

This works, but what if we only want to print something if we recieve input? Lets use a bounded
FIFO `ResBuf`. `fifoBounded` outputs a `Maybe` value, so we need a printing function that takes
a `Maybe String`.

```haskell
rhPutStringLnMaybe :: ClSF IO Second2 (Maybe String) ()
rhPutStringLnMaybe = proc mStr ->
  case mStr of
    Just str -> (arrMCl putStrLn) -< str
    Nothing -> (arrMCl putStrLn) -< "Waiting..."

rhPutStringLnMaybeRh :: Rhine IO Second2 (Maybe String) ()
rhPutStringLnMaybeRh = rhPutStringLnMaybe @@ waitClock

rhThing2Rh :: Rhine IO (SequentialClock IO StdinClock Second2) () ()
rhThing2Rh =
  rhGetInputSafeRh
  >-- fifoBounded 5
  -@- concurrently
  --> rhPutStringLnMaybeRh
```

I used arrow notation to handle the case that `rhPutStringLnMaybe` gets `Nothing`. It is more
readable this way. The sequential composition is the same as before except that we now
have `fifoBounded`. `fifoBounded` takes an argument of the maximum number of 
values that should be saved.

#### Interpolation buffers

Interpolation buffers could be useful when the second clock is much faster that the first. In this
case an interpolation buffer can pass approximate values to the second `Rhine` when there are no
new values from the first `Rhine`. Rhine provides three types of interpolation buffers, 
`linear`, `sinc`, and `cubic`. I'll start with `sinc`, and may not cover the other types, but they
all seem similar enough in use (with a few quirks perhaps).

```haskell
sinc
  :: (Monad m
     , Clock m cl1
	 , Clock m cl2
	 , VectorSpace v
	 , Ord (Groundfield v)
	 , Floating (Groundfield v)
	 , Groundfield v ~ Diff (Time cl1)
	 , Groundfield v ~ Diff (Time cl2))
  => Groundfield v
  -> ResamplingBuffer m cl1 cl2 v v
```

Thats quite the type signiture. The first three constraints are nothing new, they ensure
we have a monad and valid clocks. The rest is very new. The important parts are that the values
we can use with this `ResBuf` must be a `VectorSpace`. Additionally the `Groundfield` 
(which is a type not a typeclass) of the values must be `Ord` and `Floating`. What this means
for us is that our values must have the properties of a vector. Rhine has `VectorSpace` instances
for `Double` and `Float`, as well as tuples up to and including the 5-tuple.

So our current string from `rhGetInputSafeRh` won't work, unless we want to make `String` into a
vector space.

```haskell
rhGetDoubleMaybeRh :: Rhine IO StdinClock () (Maybe Double)
rhGetDoubleMaybeRh = (@@ StdinClock) $ safely rhGetInput >>> arr readMaybe 
```

This `Rhine` might give us a double, or not. Since `Maybe Double` is not a `VectorSpace`,
we need a way to always pass a `Double`. This creates a small problem. We need a way to turn
a `Maybe Double` into a double. One possible solution is to create a signal function that
will recieve a `Maybe Double` and in the case that it recieves a `Just`, outputs the value,
in the case that it recieves `Nothing` on the first tick outputs a default value, and if it 
recieves `Nothing`, returns the most recent value. This is like `State`. This might not be the
best solution to the problem, but it is a useful example to introduce some more of rhine's
tools.

In rhine, we can create a stateful `ClSF` using the `feedback` function.

```haskell
feedback :: Monad m => c -> MSF m (a, c) (b, c) -> MSF m a b
```

The type signiture says `MSF`, but a `ClSF` is an `MSF` with a specific monad 
(more on that later), so we can use this function. We can see that we accept a `c`, which
is the initial value, and turns a `ClSF` of one type into another. The `c` in the tuples
is our state. Its a value we can about only withing our signal function. 

```haskell
rhMaybeDoubleStateful :: ClSF IO cl (Maybe Double, Double) (Double, Double)
rhMaybeDoubleStateful = proc (mayBud, last) -> do
  case mayBud of
    Just dub -> returnA -< (dub, dub)
    Nothing  -> returnA -< (last,last)
```

This signal fuction will return the new `Double` in the case that it recieves a `Just x`,
and will otherwise return the previous value.

```haskell
rhMaybeToDouble :: ClSF IO cl (Maybe Double) Double
rhMaybeToDouble = feedback 0 rhMaybeDoubleStateful
``` 

Now we provide an initial value, I went with 0, and we have a safe `Maybe Double` to `Double`
signal function that we can compose with `rhGetDoubleMaybe`.

Before we start messing around with interpolation buffers lets make sure this works

```haskell
rhTestGetDouble = (@@ StdinClock) $ rhGetDoubleMaybe >>> rhMaybeToDouble >>> arrMCl print
```

It works as expected: If I input "q" it quits, some non-number it prints "0.0", some number
that number and it continues to print that number for new non-number inputs.

Now lets see if we can put the pieces together using `sinc`. For now we'll keep the second 
`Rhine` simple and just print values every second.

```haskell
rhPrintDubRh :: Rhine IO Second Double ()
rhPrintDubRh = arrMCl print @@ waitClock
```

We'll use the `concurrently` schedule. The documentation for `sinc` says that the window of
saved values should be much larger than the rate of `cl1`. `cl1` is `StdinClock` so it doesn't
really have a rate. Best to test a few different values.

```haskell
rhSincDubRh :: Rhine IO (SequentialClock IO StdinClock Second) () ()
rhSincDubRh = rhGetDouble >-- sinc 100 -@- concurrently --> rhPrintDubRh
```

Inputing various numbers and watching the output of interpolated values appears to be 
oscillatory, which is to be expected with `sinc`.

Why don't we give `cubic` a try.

```haskell
cubic :: (Monad m, VectorSpace v, Groundfield v ~ Diff (Time cl1), Groundfield v ~ Diff (Time cl2)) 
      => ResamplingBuffer m cl1 cl2 v v
```

`cubic` also requires that `v` is a `VectorSpace`, but it takes no arguments. The documentation
notes that since `cubic` approximates derivatives, it is delayed by two inputs from the first
`Rhine`. 

```haskell
rhCubicDubRh :: Rhine IO (SequentialClock IO StdinClock Second) () ()
rhCubicDubRh = rhGetDouble >-- cubic -@- concurrently --> rhPrintDubRh
```

Well it appears to be interpolating away. Before we provide any values, `cubic` passes "0.0"
to the second `Rhine`, which makes sense. 

Finally lets do `linear`. 

```haskell
linear
  :: (Monad m, Clock m cl1, Clock m cl2, VectorSpace v, Groundfield v ~ Diff (Time cl1), Groundfield v ~ Diff (Time cl2))
  => v
  -> v
  -> ResamplingBuffer m cl1 cl2 v v
```

The first argument is the initial velocity and the second is the initial position. Lets start
off with both equal to 0.

```haskell
rhLinearDubRh :: Rhine IO (SequentialClock IO StdinClock Second) () ()
rhLinearDubRh = rhGetDouble >-- linear 0 0 -@- concurrently --> rhPrintDubRh
```

It's more clear how `linear` works than how `sinc` or `cubic` do. Linear bases its 
approximation based on the two most recent inputs. If we input "1" first, then we should
be outputing points on the line y = x. If we input 1 a second time, we now only output 1.
The new approximation uses the most recent inputs, 1 and 1, which is just a horizontal line.
The implementation of `linear` is to start with a `keepLast` buffer, and compose it with functions
to create a linear approximation. 

## Behaviors

In rhine a `BehaviorF` is a `ClSF` 
["which is clock polymorphic over a given time domain."](https://www.manuelbaerenz.de/files/Rhine.pdf) 
What does this mean? We first need to dive a little deeper into the world of clocks. In rhine 
`Clock` is a typeclass rather than a type.

```haskell
class TimeDomain (Time cl) => Clock m cl where
  type Time cl
  type Tag cl
  initClock
    :: cl
    -> RunninClockInit m (Time cl) (Tag cl)
```

We've already talked about the `Time` and `Tag` associated types and I think we know enough about
them for now. The `initClock` function just starts the clock up. What we really care about at
this moment is the constraint on `cl`. The `Time` of the `cl` must have an instance in
`TimeDomain`.

```haskell
class TimeDomain time where
  type Diff time
  diffTime :: time -> time -> Diff time
```

So to have a valid clock, we need to be able to calculate the difference between any two times.
Rhine provides `TimeDomain` instances for `Double`, `Float`, `Integer`, `()`, `UTCTime`, for
`Num a`, but this last one is a special case thats a bit more involved. For `Double`, `Float`,
and `Integer` the `diffTime` function is `(-)`. For `()` the `diffTime` is always just `()`.
For `UTCTime`, `Diff UTCTime = Double`, and 
`diffTime t1 t2 = realToFrac $ diffUTCTime t1 t2`. Seems fairly straighforward. 

We know the `Tag` for `StdinClock` is a `String`, we've used this a lot. Its `Time` is 
`UTCTime`, which as we've learned has a valid instance in `TimeDomain`. 
`Millisecond n` also uses `UTCTime`. When we say that a `BehaviorF` is clock polymorphic
over a given time doamin we mean that it should behave the exact same for different clocks so
long as they have the same `Time cl` type, which ensures they have the same `Diff time` type.

```haskell
type BehaviorF m time a b = forall cl. time ~ Time cl => ClSF m cl a b
```

So we could write a `BehaviorF` using `UTCTime`, that behaves the same for `StdinClock` and
`Millisecond n`. Rhine provides some useful behavior functions already such as 
`integral`, `derivative`, and `average`. More can be found [here](https://hackage.haskell.org/package/rhine-0.5.1.1/docs/FRP-Rhine-ClSF-Util.html#v:integral).

Since a `BehaviorF` is a `ClSF`, we can compose the two.

```haskell
rhPrintIntegral :: ClSF IO Second () ()
rhPrintIntegral = arr (const (10 :: Double)) >>> integral >>> arrMCl print
```

