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

 * [Basic threading](#basic-threading)
 * [Thread synchronisation with MVars](#thread-synchronisation-with-mvars)
 * [Channels](#channels)

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

Since this is the first tutorial, I'm going to explain a couple of things I'll skim over the rest of the time.
Each of the files in [this directory](.) is a module, and as such it starts with a module definition:

``` haskell
module Ex1Threads where
```

We then must import any libraries we're going to use.
In this small program, we'll learn how to use `forkIO`, which is the Haskell equivalent of goroutines, or any lightweight 'green thread' you might know of from other languages.

``` haskell
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (sequence, void)
```

> **Erk, monads**
> 
> Don't panic!
> `Control.Monad` is just a library implementing some useful combinators we'll use in a second.
> I promise, this will be the last time you hear my say "monad".

With those imports out of the way, let's write a simple `IO` action that prints some stuff:

``` haskell
print3MessagesFrom name = void (sequence (map printMessage [1..3]))
    where printMessage i = do
            putStrLn (name ++ " number " ++ show i)
            sleepMs 1
```

> **Wait wait, too fast**
> 
> Sorry, let me explain that a bit better.
> `print3MessagesFrom` is a function that takes a single argument, `name`, and prints out `"<name> number <x>"` three times, where `x` counts from 1 to 3.
> It looks a little messy, if you're not used to functional programming.
> Let's read the top line after the `=` from the inside out:
> 
>  * `map someFunction someList` says 'call `someFunction` on each element of the list', which you can probably guess in this case is `[1, 2, 3]`.
> 
>  * Next, `sequence someList` simply performs all the `IO` actions in `someList`, in the order they come.
>    In this case, our list of `IO` actions is `[printMessage 1, printMessage 2, printMessage 3]`.
> 
>  * Finally, `void someAction` simply says 'discard the return value of `someAction`, I don't want it!'.
>    We do this to signal explicitly that we are throwing away the return value, otherwise Haskell's type inference will remind us that we've forgotten about it!
> 
> Other little bits and pieces - `putStrLn` prints a `String` to standard output, and `sleepMs` is a utility function that pauses execution for some amount of milliseconds.
> That's not so bad, right?
> If you need a gentler and more in-depth introduction to `IO` actions, have a read of [this excellent article](http://blog.jle.im/entry/first-class-statements).

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
For example, we could fork another thread, and watch their outputs interleave:

``` haskell
    forkIO (do
        putStrLn "starting!"
        sleepMs 2
        putStrLn "ending!")
```

All we need to do now is define `sleepMs`:

``` haskell
sleepMs n = threadDelay (n * 1000)
```

And we can run our program!

    $ runhaskell Ex1Threads.hs
    main number 1
    main number 2
    main number 3
    fork number 1
    starting!
    fork number 2
    ending!
    fork number 3

[See the whole program](./Ex1Threads.hs), and the [GbE chapter on goroutines](https://gobyexample.com/goroutines).

## Thread synchronisation with MVars

At this point in Go by Example, the reader [is introduced](https://gobyexample.com/channels) to the concept of channels, which are used to pass a message between threads.
Here I must diverge slightly from my source material, and instead of introducing channels, introduce the humble `MVar`, which is used for that purpose in Haskell.

An `MVar` is a box which may hold a single element, or may be empty.
It is safe to access from multiple threads with its operations `takeMVar`, which takes a value out of the box if one is there, and `putMVar`, which puts a value into the box if there is none.

`MVar` lives in a library, much like `forkIO`:

``` haskell
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
```

Without further ado, let's see it in action!

``` haskell
main = do
    message <- newEmptyMVar

    forkIO (do
        sleepMs 5
        putStrLn "Sending message!"
        putMVar message "Do the thing!")

    putStrLn "Waiting..."
    result <- takeMVar message
    putStrLn ("Received message: " ++ result)
```

Running this, you should see the following output:

    $ runhaskell Ex2MVars.hs
    Waiting...
    Sending a message!
    Received message: Do the thing!

I should note here that `takeMVar` and `putMVar` both block if the `MVar` is absent or present respectively, much in the same way that pulling from a Go channel blocks until a value is put into it.

However, there is a significant difference to Go here, in that an `MVar` is effectively a single-element buffer, whereas Go channels are completely unbuffered by default.

That is, if you write to a Go channel and nobody is listening, you will block.
If you put to an `MVar` which is empty, then you will not block, and another thread can read that value at its leisure.

[See the whole program](./Ex2MVars.hs) and the [GbE chapters on channels](https://gobyexample.com/channels) and [channel synchronisation](https://gobyexample.com/channel-synchronization).

## Channels

Now that we've talked about `MVar`s, let's get up to speed with channels in Haskell.
A channel, as I'm sure you're aware, is a way to pass several messages between threads.
Values are written into the channel, and can be read out of it in a first-in-first-out fashion.

Haskell's channel implementation, `Chan`, lives in:

``` haskell
import Control.Concurrent.Chan (newChan, writeChan, readChan)
```

And using it looks like this:

``` haskell
main = do
    messages <- newChan
    writeChan messages "unbounded"
    writeChan messages "channels"
```

To read from the channel, we use `readChan`:

``` haskell
    msg <- readChan messages
    putStrLn msg
```

Or, more concisely:

``` haskell
    putStrLn =<< readChan messages
```

> **=<<?**
> 
> Some of you following along at home might have expected to be able to write something like
> 
> `putStrLn (readChan messages)`
> 
> If you did, you'd have seen a type error along the lines of `couldn't match type 'IO [Char]' with '[Char]'`.
> What it's saying is that `putStrLn` expects a string (a list of `Char`), but you've given it an `IO` action that returns a string.
> The first solution is to _bind_ that `IO` action to get the string out of it, which is what the `<-` operator does in Haskell.
> The second way around it is to use another way of binding using the `=<<` operator.
> 
> If you squint, it looks a bit like regular function application.
> I won't go into too much depth here.

[See the whole program](./Ex3Channels.hs) and the [GbE chapter on buffered channels](https://gobyexample.com/channel-buffering).
