# Read me first

See the [readme](../readme.md) for details on how to read and run this tutorial.

 * [Chapter 1. Base only](#1-base-only)
 * Chapter 2. Haskell Platform only
 * Chapter 3. Libraries

# 1. Base only

The sections in this chapter will introduce you to what might be described as GHC's concurrency primitives.
This section explores what you can get if you just install GHC, for example from the Ubuntu package repositories.
The availability of these features is comparable to built-in language features like Go's goroutines, or `pthread.h` which is available by default on almost any POSIX platform.

> ##### Is it really built-in?
> 
> An important thing to note is that most of the concurrency features I'll be talking about are specific to GHC, which is just one Haskell compiler, and are not specified in the Haskell language standard.
> GHC's runtime has support for lightweight 'green' threads and mutexed variables, which I'm about to introduce.
> These primitives are then used to build up abstrations like channels.
> It also has primitives used to implement software transactional memory (STM), but the `base` library that comes with GHC does not include libraries that use these primitives yet.

## Contents

 * [Basic threading](#basic-threading)
 * [Thread synchronisation with MVars](#thread-synchronisation-with-mvars)
 * [Sharing state with MVars](#sharing-state-with-mvars)
 * [Channels](#channels)
 * [Duplicating channels](#duplicating-channels)

## Basic threading

[See the whole program.](./Threads.hs)

Since this is the first tutorial, I'm going to explain a couple of things that I'll skim over in future sections.
Each of the files in [this directory](.) is a module, and starts with a module definition:

``` haskell
module Threads where
```

Now we import all the libraries we're going to make use of.
This program will introduce you to `forkIO`, which is used to start new runtime threads.
Let's import the `forkIO` function from the `Control.Concurrent` module, along with `threadDelay`, which we'll use to make threads slow down so we can see them acting simultaneously.

``` haskell
import Control.Concurrent (forkIO, threadDelay)
import Data.Foldable (for_)
```

With those imports out of the way, let's write a simple `IO` action that prints some stuff:

``` haskell
printMessagesFrom name = for_ [1..3] printMessage
    where printMessage i = do
            sleepMs 1
            putStrLn (name ++ " number " ++ show i)
```

> ##### Why does `for` have an underscore?
> 
> The underscore is simply a convention among Haskell's base libraries that the result of the comprehension is discarded.
> It keeps the type-checker satisfied that we're not throwing away results we intended to keep.
> And, you may also be wondering, why did we have to import it?
> Many of Haskell's 'control structures' are actually just regular functions, and `for_` is no exception.

We can call this in the usual synchronous fashion inside any other `IO` action, like the `main` action:

``` haskell
main = do
    printMessagesFrom "main"
```

Or, we can run it concurrently using `forkIO`:

``` haskell
    forkIO (printMessagesFrom "fork")
```

This non-blocking call starts up a new lightweight thread and runs the given action.
The main thread can then go on and do other things.
For example, we could fork _another_ thread, and watch their outputs appear interleaved with each other:

``` haskell
    forkIO (do
        putStrLn "starting!"
        sleepMs 5
        putStrLn "ending!")
```

I've left the definition of `sleepMs` til now.
`threadDelay` takes a delay in microseconds, which isn't super helpful, so I like to use this function in examples:

``` haskell
sleepMs n = threadDelay (n * 1000)
```

And now we can run our program!

    $ runhaskell Threads.hs
    main number 1
    main number 2
    main number 3
    starting!
    fork number 1
    fork number 2
    fork number 3
    ending!

> ##### A note on interleaving
> 
> If you play with the placement of the calls to `sleepMs` in this example, you might notice that sometimes words get mangled.
> Haskell's output functions aren't actually atomic, so if two threads try to print at the same time, you might see only a few characters printed from one thread before another thread takes over and prints a few.

## Thread synchronisation with MVars

[See the whole program.](./MVars.hs)

An `MVar` is a box which may hold a single element, or may be empty.
The documentation calls it a 'mutable variable', but I prefer 'mutexed variable' for a mnemonic.
The `MVar` being empty equates to the value being 'locked', unable to be modified.

It is safe to access an `MVar` from multiple threads, unline an `IORef`.
`takeMVar` removes the value from the box if one is there, and otherwise blocks until the box is full.
`putMVar` puts a value into the box if there is none, and otherwise blocks until the box is empty.
Without further ado, let's see it in action!

``` haskell
main = do
    result <- newEmptyMVar

    forkIO (do
        sleepMs 5
        putStrLn "Calculated result!"
        putMVar result 42)

    putStrLn "Waiting..."
    value <- takeMVar result
    putStrLn ("The answer is: " ++ show value)
```

Running this, you should see the following output:

    $ runhaskell MVars.hs
    Waiting...
    Calculated result!
    The answer is: 42

## Sharing state with MVars

[See the whole program.](./MVarSharedState.hs)

In the [previous example](#thread-synchronisation-with-mvars), we used an `MVar` as a way to wait for a concurrent action's result.
Sometimes we want long-running actions to share state, and we want to make sure they don't run into race conditions when modifying that state.
`MVars` can also fill this role, though we'll see a much better way to do it when we look at STM later.

For now, we'll reproduce a classic concurrency example: we have a single counter, which multiple threads try to increment several times.
In the presence of race conditions, errors will mean often the final result isn't what you would expect, if you were to run each thread synchronously.

```haskell
main = do
    counter <- newMVar 0

    let increment = do
            count <- takeMVar counter
            putMVar counter $! count + 1
        incrementer = do
            replicateM 1000 increment
            return ()

    threads <- replicateM 5 (forkIO incrementer)

    sleepMs 100
    count <- takeMVar counter
    print count
```

`increment` uses `$!`, which is a _strict_ form of function application, to avoid simply storing a huge `(0 + (1 + (1 + ...` thunk inside `counter`.
This strict application ensures that `c + 1` is evaluated before it is stored in the `MVar`.

Note that in this case, our application is essentially single-threaded, because all threads block on `counter`.
So we gain no _time_ efficiency from using strict application (`$!`) - we could just as easily build up a huge thunk, then have the main thread evaluate it when we `print count`.
However, this costs memory, so evaluating the counter strictly at every step makes the program more _space_ efficient.

Let's give this example a go:

    $ runhaskell MVarSharedState.hs
    5000

## Channels

[See the whole program.](./Channels.hs)

Now that we've talked about `MVar`s, let's get up to speed with channels in Haskell.
A channel, as I'm sure you're aware, is a way to pass several messages between threads.
Whereas an `MVar` can only contain a single value, a channel can have a whole stream of values.
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

> ##### =<<?
> 
> Some of you following along at home might have expected to be able to write something like
> 
> `putStrLn (readChan messages)`
> 
> If you did, you'd have seen a type error along the lines of `couldn't match type 'IO [Char]' with '[Char]'`.
> What it's saying is that `putStrLn` expects a string (a list of `Char`), but you've given it an `IO` action that returns a string.
> The solution is to _bind_ that `IO` action to get the string out of it, which is what the `<-` operator does in Haskell.
> The `=<<` operator is simply another way of writing the same equation.
> 
> If you squint, it looks a bit like regular function application.
> I won't go into too much depth here.
> If you need a gentler and more in-depth introduction to `IO` actions, have a read of [this excellent article](http://blog.jle.im/entry/first-class-statements).

## Duplicating channels

[See the whole program.](./DuplicatingChannels.hs)

Data pushed through a channel is not duplicated - that is, if two threads are waiting for a channel, and one piece of data is put into the channel, only one thread will see the data.
Here's a quick function that demonstrates that property:

```haskell
nonDuplicatedTest = do
    messages <- newChan
    forkIO (messageReader messages "First")
    forkIO (messageReader messages "Second")
    writeChan messages "Hi!"

messageReader channel name = do
    msg <- readChan channel
    putStrLn (name ++ " read: " ++ msg)
```

If you were to run this, you'd see something like:

    First read: Hi!

The message is only printed by one of the threads.
If we want 'broadcast' behaviour, where all threads have access to the messages on a channel, we need to use `dupChan`.
As in:

```haskell
duplicatedTest = do
    broadcast <- newChan
    forkIO (broadcastReader broadcast "Third")
    forkIO (broadcastReader broadcast "Fourth")
    writeChan broadcast "Bye!"

broadcastReader channel name = do
    channel' <- dupChan channel
    messageReader channel' name
```

Here, `broadcastReader` is using `dupChan` to create a _duplicate channel_ of `channel`, which means that every message sent over the original channel is duplicated to a new channel.
It then involes the rest of `messageReader` on that new channel.
Let's run both of these two examples at once:

```haskell
main = do
    nonDuplicatedTest
    duplicatedTest
    sleepMs 5
```

    $ runhaskell DuplicatingChannels.hs
    First read: Hi!
    Third read: Bye!
    Fourth read: Bye!

Note that sometimes you may see `Second` getting `Hi!`, or you may see `Hi!` being printed after `Bye!`.
This is due to the vagaries of thread execution, since we didn't do any synchronisation in these examples.
(It's also why I added a `sleepMs` in there - just to make sure the main thread waits for all the messages to filter through.)

# 2. Haskell Platform only

In this section, we'll expand our horizons and start using some libraries that come with the [Haskell Platform](https://www.haskell.org/platform/).
Specifically, we'll cover [async](http://hackage.haskell.org/package/async), and dip our toes into [stm](http://hackage.haskell.org/package/stm).
Note that if you installed GHC without the platform, you may need to `cabal install` some libraries.
I'll mention when you should do this as we go.

> ##### What about `parallel`?
>
> The Haskell platform also includes the [parallel](http://hackage.haskell.org/package/parallel) library, but we won't cover it.
> I'm limiting this tutorial to showing examples of concurrency, which is of course [different to parallelism](http://chimera.labs.oreilly.com/books/1230000000929/ch01.html#sec_terminology).
> If you're interested in learning how to parallelise pure or non-concurrent operations, I highly recommend diving into the excellent [parallel and concurrent programming in Haskell](http://community.haskell.org/~simonmar/pcph/).
