# Simple concurrency in Haskell

This tutorial will introduce you to simple mechanisms for concurrency in Haskell.
It's a fairly direct translation of the concurrency-related chapters of [Go by Example](http://gobyexample.com), which I found to be a great reference to Go's concurrency primitives.

I'm writing this to:

 1. Learn more about Go's concurrency primitives by translating them.
    Go is frequently praised for its built-in channels and goroutines, so I was naturally interested.
    But at the same time, I'd like to see how we can achieve similar results in Haskell.
 2. Learn more about Haskell's concurrency libraries - and hopefully teach you about them, too.
    I haven't had much cause to use them, so I'm keen to get my hands dirty and solidify my knowledge.
 3. Hopefully reach out to interested non-Haskellers who may not know about some of these features.
    Ah yes, my secret motives are revealed.

Throughout the tutorial, and especially in [the last chapter](#further-reading), I've collected some references for those interested in going further with concurrency and parallelism in Haskell.
This includes recommendations of libraries that can simplify your concurrency experience, or provide higher-level abstractions for you to work with.
In the main body of the tutorial, though, I'll be sticking with the lower-level libraries provided in GHC's `base` package, or in the Haskell Platform.

> ##### An aside
> 
> Note that many of these tutorials end up translating the Go programs from GbE more than the actual semantics of Go itself.
> With Haskell being lazy and pure, there are bound to be some indirect translations, but I've tried to follow the spirit of the examples as much as possible, rather than the fine details of semantics.
> 
> Also, this tutorial will probably not end up covering STM (at least not for the forseeable future), even though it definitely counts as simple concurrency.
> This is simply because there's no real equivalent in Go by Example, so I'd have to actually think of my own tutorial structure.
> Ain't nobody got the energy for that.

## Contents

 * [Basic threading](#basic-threading)
 * [Thread synchronisation with MVars](#thread-synchronisation-with-mvars)
 * [Channels](#channels)
 * [Directed channels](#directed-channels)
 * [Select](#select)
 * [Timeouts](#timeouts)
 * [Composable select](#composable-select)
 * [Coming soon]
 * [Libraries and further reading](#further-reading)

## Running the code

If you've cloned this repository to run locally, once you have the Haskell platform installed, you can run any of the example files by entering, for example,

    $ runhaskell src/Ex1Threads.hs

at the command-line.
You can also open modules in GHCI to play around with them in a REPL:

    $ ghci
    ghci> :load src/Ex1Threads.hs
    ghci> main

Note that you cannot compile the example files directly, but I have provided a `Main.hs` file which runs all the examples, and which can be compiled into an executable if you wish.

    $ ghc --make Main.hs
    $ ./Main

## A brief word on syntax

As a Haskeller learning Go, I found myself regularly forgetting that `<-` and `->` have different meanings in the two languages.
In Haskell, `<-` is a generic operator called _bind_ which runs some sort of action and stores the result in a variable (_binds_ it).
`->` is used in anonymous functions, e.g. `\x -> x + 1`, and in type signatures, like `Int -> Int`.
In Go, both of these operations refer specifically to channels, as far as I'm aware.

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
import Data.Foldable (for_)
```

With those imports out of the way, let's write a simple `IO` action that prints some stuff:

``` haskell
print3MessagesFrom name = for_ [1..3] printMessage
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

> ##### =<<?
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
> 
> If you need a gentler and more in-depth introduction to `IO` actions, have a read of [this excellent article](http://blog.jle.im/entry/first-class-statements).

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
The meat of the function involves forking a bunch of threads (one for each `MVar`, in fact!), each of which asynchronously tries to take the value it was assigned, and then put that value into the `won` `MVar`.
Meanwhile, the main thread waits for `won` to be filled, then returns the winner.

> ##### You just created _how many_ threads?
> 
> Don't panic, threads are lightweight.

> ##### And what's this `forM`?
> 
> I tried to slip that one past without you noticing.
> You can basically read it as the `forEach` that's in most other languages.

But the key thing to know about it is that it is _blocking_ - it will only return once it has a value from one of its `MVar`s, which is why I appended the `Now` to it.
This is the same behaviour as Go's `select` construct, but the reason I chose to leave the name `select` free will become apparent in a [later tutorial](#composable-select).

So now we have the result we wanted!
We can `selectNow` over several `MVar`s, and receive the first result.
Let's run the full code:

    $ runhaskell Ex5Select.hs
    Let the race begin!
    Sally finished first!

Run it a few times to make sure the random number generator is working.

[See the whole program](./Ex5Select.hs) and the [GbE chapter on select](https://gobyexample.com/select).

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

There are two approaches I will illustrate now, and at least one more that I won't (using the `Default` or `Monoid` typeclasses).
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

> ##### You're making this up
> 
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

[See the whole program](./Ex6Timeouts.hs) and the [GbE chapter on timeouts](https://gobyexample.com/timeouts).

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
    employees <- forM ["Harry", "Sally", "Aang"] (\name -> do
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
    forM vars (\var -> forkIO (do
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

## Further reading

This tutorial has covered some of the basic concurrency primitives available in Haskell/GHC, but I've stayed away from mentioning any of the other resources out there, or libraries you could use to simplify your code and amplify its power.
So, I'll do that now.

For a far more in-depth look at a variety of techniques and libraries for doing concurrency and parallelism in Haskell, [Parellel and Concurrent Programming in Haskell](http://community.haskell.org/~simonmar/pcph/) is the authority, and is available for free online!
The book is written by the author of both the [async](https://hackage.haskell.org/package/async) package and GHC's parallel runtime itself.

If you're keen to use channels in your application, then you might want to take a look at the [unagi-chan](https://hackage.haskell.org/package/unagi-chan) library, which provides super-performant channels, and even has directed channels built-in.

And if you want a higher level of composable concurrency, then [STM](https://wiki.haskell.org/Software_transactional_memory) is for you.
It stands for software transactional memory, and it's all the rage these days.
This wouldn't be a Haskell tutorial if I didn't link you to a research paper, so [here's](http://research.microsoft.com/pubs/67418/2005-ppopp-composable.pdf) the classic paper introducing Haskell's implementation of STM.
Trust me, it's a pretty easy read!

The [pipes-concurrency](https://hackage.haskell.org/package/pipes-concurrency) library allows you to build up dynamic networks of concurrent activities.
