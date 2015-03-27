# Simple concurrency in Haskell

This tutorial will introduce you to simple mechanisms for concurrency in Haskell.
It's a fairly direct translation of the concurrency-related chapters of [Go by Example](http://gobyexample.com), which I found to be a great reference to Go's concurrency primitives.

I'm writing this to:

 1. Learn more about Go's concurrency primitives by translating them.
    Go is frequently praised for its built-in channels and goroutines, so I was naturally interested.
    But at the same time, I'd like to see how we can build up equivalent features in another language.
 2. Learn more about Haskell's concurrency libraries - and hopefully teach you about them, too.
    I haven't had much cause to use them, so I'm keen to get my hands dirty and solidify my knowledge.
 3. Hopefully reach out to interested non-Haskellers who may not know about some of these features.
    Ah yes, my secret motives are revealed.

This tutorial will probably not end up covering STM (at least not for the forseeable future), even though it definitely counts as simple concurrency.
This is simply because there's no real equivalent in Go by Example, so I'd have to actually think of my own tutorial structure.
Ain't nobody got the energy for that.

## Running the code

If you've cloned this repository to run locally, once you have the Haskell platform installed, you can run any of the example files by entering, for example,

    $ runhaskell src/Ex1Threads.hs

at the command-line.
You can also open modules in GHCI to play around with them in a REPL:

    $ ghci
    ghci> :load src/Ex1Threads.hs
    ghci> main

Note that you cannot compile the example files directly, but I have provided a `Main.hs` file which runs all the examples, and which can be compiled into an executable if you wish.

## Basic threading

Let's write a simple `IO` action that prints some stuff:

``` haskell
print3MessagesFrom name = void (sequence (map printMessage [1..3]))
    where printMessage i = do
            putStrLn (name ++ " number " ++ show i)
            sleepMs 1
```

> **I'm new to Haskell! What's all this?**
> `print3MessagesFrom` is a function that takes a single argument, `name`, and prints out `"<name> number <x>"` three times, where `x` counts from 1 to 3.
> It looks a little messy, if you're not used to functional programming.
> 
> Let's read the top line after the `=` from the inside out:
> 
> `map printMessage [1..3]` says 'call the `printMessage` function on each element of the list `[1..3]`, which you can probably guess expands to `[1, 2, 3]`.
> 
> Next, `sequence (...)` simply performs all the `IO` actions in the list you give it, in the order they are in the list.
> 
> Finally, `void (...)` simply says 'discard the return value, I don't want it!'.
> We do this to signal explicitly that we are throwing away the return value, otherwise Haskell's type inference will remind us that we've forgotten about it!

We can call this in the usual synchronous fashion inside any other `IO` action:

``` haskell
main = do
    print3MessagesFrom "main"
```

However, to fork a thread and run a function concurrently with the main thread, we can use the `forkIO` function on our `IO` action:

``` haskell
    forkIO (print3MessagesFrom "fork")
```

This non-blocking call starts up a new lightweight thread and runs the given action.
The main thread can then go on and do other things.

[See the whole program.](./Ex1Threads.hs)
