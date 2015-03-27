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

> **An aside**
> 
> This tutorial will probably not end up covering STM (at least not for the forseeable future), even though it definitely counts as simple concurrency.
> This is simply because there's no real equivalent in Go by Example, so I'd have to actually think of my own tutorial structure.
> Ain't nobody got the energy for that.

## Contents

 * [Basic threading](#basic-threading)
 * [Thread synchronisation with MVars](#thread-synchronisation-with-mvars)
 * [Channels](#channels)
 * [Directed channels](#directed-channels)
 * [Select](#select)

## Running the code

If you've cloned this repository to run locally, once you have the Haskell platform installed, you can run any of the example files by entering, for example,

    $ runhaskell src/Ex1Threads.hs

at the command-line.
You can also open modules in GHCI to play around with them in a REPL:

    $ ghci
    ghci> :load src/Ex1Threads.hs
    ghci> main

Note that you cannot compile the example files directly, but I have provided a `Main.hs` file which runs all the examples, and which can be compiled into an executable if you wish.

## A brief word on syntax

As a Haskeller learning Go, I found myself regularly forgetting that `<-` and `->` have different meanings in the two languages.

In Haskell, `<-` is a generic operator called _bind_ which runs some sort of action and stores the result in a variable (_binds_ it).
`->` is used in anonymous functions, e.g. `\x -> x + 1`.

In Go, both of these operations refer specifically to channels.

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

## Directed channels

At this point, Go by Example has a chapter on channel directions, which is a feature of Go allowing you to restrict a channel to be read-only or write-only to a function you call.
That is, you can pass in a regular channel to a function which is only allowed to, for example, read from that channel.

Initially I thought about just not translating this example, since Haskell does not have the same ability to restrict its channels.
Then I realised that I was being dumb, and of course Haskell does.
Using types to rule out errors is what Haskell is _all about_.

So without further ado, let's replicate some language features!
This is going to require writing our own little library, which is exciting, but first, I'll show you how it's used:

``` haskell
import DirectedChannels (WriteOnlyChan, writeOnly, writeWOChan,
                         ReadOnlyChan,  readOnly,  readROChan)

main = do
    messages <- newChan
    forkIO (producer (writeOnly messages))
    forkIO (consumer (readOnly  messages))
    sleepMs 5
```

Note how we use `writeOnly` and `readOnly`, which are functions we'll put into our `DirectedChannels` module, to restrict the behaviour of the `producer` and `consumer` threads.

Now let's see what those two functions do:

``` haskell
producer chan = do
    writeWOChan chan "Hello,"
    writeWOChan chan "Dave."

consumer chan = do
    putStrLn =<< readROChan chan
    putStrLn =<< readROChan chan
```

The code is fairly dumb, but the point is the use of `writeWOChan` and `readROChan`.
If I were to try to use `readROChan` in `producer`, or even `writeChan` or `readChan` as we used in the [last tutorial](#channels), it'd be a type error.
How did we make this happen?

Essentially, we make a wrapper type around `Chan` and only define certain operations on it.
We also take care not to export the constructor of this wrapper type, so that you can't pattern-match a normal `Chan` out of a `ReadOnlyChan` or `WriteOnlyChan`.

The details of out are in [DirectedChannels.hs](./DirectedChannels.hs) should you care to read about them.

The output of this program is probably pretty obvious:

    $ runhaskell Ex4DirectedChannels.hs
    Hello,
    Dave.

[See the whole program](./Ex4DirectedChannels.hs) and the [GbE chapter on directed channels](https://gobyexample.com/channel-directions).

## Select

Go has a built-in `select` statement that will wait on one of several channels, and perform some code depending on which one produced a result first (much like the POSIX `select` function).
Haskell has no built-in feature for this, like it doesn't have directed channels - but again, we can write it ourselves if we're willing to put a little bit of time in.

Let's start with how I intend the function to work.
Since `select` is intended to receive a single result, we'll build it on top of `MVar`s rather than channels.
So `select` should take a list of `MVar`s, and return the first value that is put into any of them.
Like so:

``` haskell
main = do
    item1 <- newEmptyMVar
    item2 <- newEmptyMVar

    putStrLn "Let the race begin!"
    forkIO (worker "Harry" item1)
    forkIO (worker "Sally" item2)

    name <- selectNow [item1, item2]
    putStrLn (name ++ " finished first!")
```

The `worker` function does some work, then returns its name, so we can see who did their work the quickest.
It's not a very interesting function, so let's skip it.

The interesting function, `selectNow`, is defined as follows.

``` haskell
selectNow vars = do
    won <- newEmptyMVar
    forM vars (\var -> forkIO (do
        val <- takeMVar var
        putMVar won val))
    winner <- takeMVar won
    return winner
```

This function takes a list, `vars`, of `MVar`s.
The meat of the function involves forking a bunch of threads (one for each `MVar`, in fact!), each of which acynchronously tries to take the value it was assigned, and then put that value into the `won` `MVar`.
Meanwhile, the main thread waits for `won` to be filled, then returns the winner.

> **You just created _how many_ threads?**
> 
> Don't panic, threads are lightweight.

But the key thing to know about it is that it is _blocking_ - it will onyl return once it has a value from one of its `MVar`s, which is why I appended the `Now` to it.
This is the same behaviour as Go's `select` construct, but the reason I chose to leave the name `select` free will become apparent in a [later tutorial](#composable-select).

So now we have the result we wanted!
We can `selectNow` over several `MVar`s, and receive the first result.
Let's run the full code:

    $ runhaskell Ex5Select.hs
    Let the race begin!
    Sally finished first!

Run it a few times to make sure the random number generator is working.

[See the whole program](./Ex5Select.hs) and the [GbE chapter on select](https://gobyexample.com/select).
