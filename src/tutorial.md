# Read me first

See the [readme](../readme.md) for details on reading and running this tutorial.

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
 * [Channels](#channels)
 * [Directed channels](#directed-channels)
 * [Select](#select)
 * [Timeouts](#timeouts)
 * [Composable select](#composable-select)

## Basic threading

Since this is the first tutorial, I'm going to explain a couple of things that I'll skim over in future sections.
Each of the files in [this directory](.) is a module, and starts with a module definition:

``` haskell
module Ex1Threads where
```

Now we import all the libraries we're going to make use of.
This program willintroduce you to `forkIO`, which is used to start new runtime threads.
Let's import the `forkIO` function from the `Control.Concurrent` module, along with `threadDelay`, which we'll use to make threads slow down so we can observe interleaving.

``` haskell
import Control.Concurrent (forkIO, threadDelay)
import Data.Foldable (for_)
```

With those imports out of the way, let's write a simple `IO` action that prints some stuff:

``` haskell
printMessagesFrom name = for_ [1..3] printMessage
    where printMessage i = do
            putStrLn (name ++ " number " ++ show i)
            sleepMs 1
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
For example, we could fork _another_ thread, and watch their outputs interleave:

``` haskell
    forkIO (do
        putStrLn "starting!"
        sleepMs 2
        putStrLn "ending!")
```

I've left the definition of `sleepMs` til now.
`threadDelay` takes a delay in microseconds, which isn't super helpful, so I like to use this function in examples:

``` haskell
sleepMs n = threadDelay (n * 1000)
```

And now we can run our program!

    $ runhaskell Ex1Threads.hs
    main number 1
    main number 2
    main number 3
    fork number 1
    starting!
    fork number 2
    ending!
    fork number 3

[See the whole program.](./Ex1Threads.hs)

## Thread synchronisation with MVars

An `MVar` is a box which may hold a single element, or may be empty.
The documentation calls it a 'mutable variable', but I prefer 'mutexed variable' for a mnemonic.
The box being empty equates to the value being 'locked', unable to be modified.

It is safe to access an `MVar` from multiple threads, unline an `IORef`.
`takeMVar` removes the value from the box if one is there, and otherwise blocks until the box is full.
`putMVar` puts a value into the box if there is none, and otherwise blocks until the box is empty.
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

[See the whole program.](./Ex2MVars.hs)

## Channels

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

[See the whole program.](./Ex3Channels.hs)

## Directed channels

Go by Example has a chapter on channel directions, which is a feature of Go allowing you to restrict a channel to be read-only or write-only to a function you call.
That is, you can pass in a regular channel to a function which is only allowed to, for example, read from that channel.

Initially I thought about not translating this example, since Haskell does not have the same ability to restrict its channels.
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
The details are in [DirectedChannels.hs](./DirectedChannels.hs) should you care to read about them.

The output of this program is probably pretty obvious:

    $ runhaskell Ex4DirectedChannels.hs
    Hello,
    Dave.

[See the whole program.](./Ex4DirectedChannels.hs)

## Select

Go has a built-in `select` statement that will wait on one of several channels, and perform some code depending on which one produced a result first (much like the POSIX `select` function).
Haskell has no built-in feature for this, like it doesn't have directed channels - but again, we can write it ourselves if we're willing to put a tiny bit of effort in.

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
    winner <- newEmptyMVar
    for_ vars (\var -> forkIO (do
        val <- takeMVar var
        tryPutMVar winner val))
    takeMVar winner
```

This function takes a list, `vars`, of `MVar`s.
The meat of the function involves forking a bunch of threads (one for each `MVar`, in fact!), each of which asynchronously tries to take the value it was assigned, and then put that value into the `winner` `MVar`.
Meanwhile, the main thread waits for `winner` to be filled, then returns the value.

> ##### You just created _how many_ threads?
> 
> Don't panic, threads are lightweight.

It's important to notice that instead of using `putMVar`, the contestant threads use `tryPutMVar`, which is a non-blocking alternative.
This way, once one thread has 'won', the other threads will harmlessly end, rather than continuing to try to put a value into an `MVar` which we only ever take one value out of.

`selectNow` blocks - it will only return once it has a value from one of its `MVar`s, which is why I appended the `Now` to it.
This is the same behaviour as Go's `select` construct, but the reason I chose to leave the name `select` free will become apparent in a [later tutorial](#composable-select).

So now we have the result we wanted!
We can `selectNow` over several `MVar`s, and receive the first result.
Let's run the full code:

    $ runhaskell Ex5Select.hs
    Let the race begin!
    Sally finished first!

Run it a few times to make sure the random number generator is working.

[See the whole program.](./Ex5Select.hs)

## Timeouts

When we wrote the `selectNow` function above, I neglected to mention a key difference between it and Go.
Go's `select` structure controls flow of execution, rather like a `switch` statement in other C-like languages.
That means that each branch of the `select` can have a different type of result.
One area this is important is when you want your operation to time out.

If you open up last tutorial's code in GHCI, you can ask GHC what the type of `selectNow` is:

    ghci> :type selectNow
    selectNow :: [MVar a] -> IO a

This means that every `MVar` we give select must have the same type `a`, and the result of binding `selectNow` is an `a` also.
But what can we do if the selection times out somehow?
How can we provide a value of type `a` when we don't even know what that type might be?

There are two approaches I will illustrate now, and at least one more that I won't (using a typeclass like `Default` or `Monoid`).
First, let's do it the easy way - after a certain time, we'll just provide a default value!
This is as easy as adding one more `MVar` to the selection, which is set to be filled at a certain time.
We can write a utility function to provide just such an `MVar`:

``` haskell
timeout delay value = do
    var <- newEmptyMVar
    forkIO (do
        sleepMs delay
        putMVar var value)
    return var
```

So, after sleeping up to `delay`, we `putMVar` and that will cause our `selectNow` to, well, select it.
Here's an example of using that:

``` haskell
main = do
    never <- newEmptyMVar
    timer <- timeout 5 "Too slow!"
    result <- selectNow [never, timer]
    putStrLn result
```

Looks easy, right?
But what if we don't want to provide some default value?
Sometimes we want to be notified of a failure, or simply can't construct a sensible default value.
Well, for these cases, we're going to need a function of a different type.
Because we won't always necessarily return an `a`, we can't say our function returns type `a`: it must return a `Maybe a`.

> `Maybe` is a Haskell type that encodes nullability.
> A type `a` can never be `nil` or `null` or `None`, but a type `Maybe a` can be `Nothing`, or `Just a` if there actually is a value.
> It's a perfect fit for our new selection function which will _maybe_ time out.

First, let's see how we would use this new wrapper function:

``` haskell
    result <- selectNowOrTimeout 5 [never]
    case result of
        Nothing -> putStrLn "Too slow!"
        Just r  -> putStrLn r
```

So instead of calling `selectNow`, we call `selectNowOrTimeout`.
If I had defined this function already, you'd be able to open GHCI to inspect its type:

    ghci> :type selectNowOrTimeout
    selectNowOrTimeout :: Int -> [MVar a] -> IO (Maybe a)

Well, that looks right.
Notice how we inspected `result` by matching against the two potential cases, where there was `Nothing` returned (i.e. the select timed out), and when there was `Just` some result.

Okay, now it's time to see how I actually define `selectNowOrTimeout`:

``` haskell
selectNowOrTimeout delay vars = do
    result <- newEmptyMVar
    -- This thread patiently waits for the actual select that we want to perform.
    waiter <- forkIO (do
        value <- selectNow vars
        putMVar result (Just value))
    -- This thread acts as a watchdog, and kills the other thread if it takes too long.
    killer <- forkIO (do
        sleepMs delay
        killThread waiter
        putMVar result Nothing)
    takeMVar result
```

This is a little long, but take it step by step.
The main logic is two threads, the `waiter` and the `killer`.
The `waiter` actually performs a regular `selectNow` to try to get a result from the `vars` we wanted to wait for.
The `killer` performs a standard-looking timeout, but with the addition of killing the `waiter` thread after it does so.
This isn't technically necessary, but it'd be nice to not have useless threads hanging around, right?

> ##### Well, actually...
> 
> Unfortunately, killing the `waiter` thread doesn't kill the threads it forked.
> Crazy, right?
> The [slave-thread](https://hackage.haskell.org/package/slave-thread) library addresses this, but I didn't want to introduce any additional dependencies into the code.
> Suggestions welcomed.

Critically, after waiting, the `killer` puts the value `Nothing` into the `result`.
This, combined with the use of `Just` in the `waiter`, are that makes the return type `Maybe a`, without needing to pass `MVar (Maybe a)`s into the function.

Finally, the `takeMVar result` blocks until either the timeout happens, or one of the actual values appears.
So, running our demo program, we see that unfortunately we time out both times:

    $ runhaskell Ex6Timeouts.hs
    Too slow!
    Too slow!

[See the whole program.](./Ex6Timeouts.hs)

## Composable select

At this point, I'd like to digress again from GbE's structire and return to the question I left hanging of why I named my `select` implementation `selectNow`.
Basically, though it's nice for writing examples with, it's not composable.
For example, I can't select on the result of a `selectNow`, because `selectNow` blocks.

What can we do about this?
Well, easy - write a composable `select`!
Let's first see an example of how it'll be used.

``` haskell
main = do
    -- Fork a couple of humans to do some work.
    employees <- for ["Harry", "Sally", "Aang"] (\name -> do
        item <- newEmptyMVar
        forkIO (worker name item)
        return item)

    -- A very efficient robot will also do some work.
    robot <- newEmptyMVar
    forkIO (putMVar robot "Bleep bloop, puny humans.")

    -- Let the battle for the future of the Earth begin.
    fastestHuman <- select employees
    battle <- select [fastestHuman, robot]
    result <- takeMVar battle
    putStrLn result
```

We're reusing the `worker` function from when we first introduced `selectNow`.
The critical part of the code is the last paragraph.
`select employees` takes a list of `MVar`s, and critically, _returns another `MVar`_.
We can then use this `MVar`, which will represent the first employee to finish their work, in another `select`, where we compare this human to the robot worker.

How do we implement this composable `select`?
It's very similar to `selectNow`, but we just return the `MVar`, instead of calling `takeMVar` and returning the result:

``` haskell
select vars = do
    won <- newEmptyMVar
    for_ vars (\var -> forkIO (do
        val <- takeMVar var
        putMVar won val))
    return won
```

So, what do we gain from this?
Well, we can compose several selections together, and then only take the final result.
This can sometimes increase our opportunities to do work in parallel.
Of couse, sometimes it just means adding an extra `takeMVar` when you don't want it.
Notice that we could have written, instead:

``` haskell
    fastestHuman <- select employees
    result <- selectNow [fastestHuman, robot]
```

and that `selectNow` becomes trivial to implement in terms of `select`:

``` haskell
selectNow vars = do
    var <- select vars
    takeMVar var
```

Let's run the code, as if it isn't a foregone conclusion:

    $ runhaskell Ex7ComposableSelect.hs
    Bleep bloop, puny humans.

[See the whole program.](./Ex7ComposableSelect.hs)

# 2. Haskell Platform only

In this section, we'll expand our horizons and start using some libraries that come with the [Haskell Platform](https://www.haskell.org/platform/).
Note that if you installed GHC without the platform, you may need to `cabal install` some libraries.
I'll mention when you should do this as we go.
