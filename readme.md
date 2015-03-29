# Haskell concurrency by example

This repository contains translations of [Go by Example](http://gobyexample.com)'s concurrency chapters, as well as a lengthy tutorial which builds the reader up to them in much the same way as Go by Example.
You can follow along with [the tutorial](./src/tutorial.md), clone this repository and run the code locally, or open it up [in FP Complete's online Haskell IDE](https://www.fpcomplete.com/user/eightyeight/simple-concurrency) to edit and run the code yourself.

This tutorial will introduce you to simple mechanisms for concurrency in Haskell.
It's a fairly direct translation of the concurrency-related chapters of [Go by Example](http://gobyexample.com), which I found to be a great reference to Go's concurrency primitives.

I wrote this to:

 1. Learn more about Go's concurrency primitives by translating them.
    Go is frequently praised for its built-in channels and goroutines, so I was naturally interested.
    But at the same time, I'd like to see how we can achieve similar results in Haskell.
 2. Learn more about Haskell's concurrency capabilities, and hopefully teach you about them, too.
    I haven't had much cause to use them, so I'm keen to get my hands dirty and solidify my knowledge.
 3. Hopefully reach out to interested non-Haskellers who may not know about some of these features.
    Ah yes, my secret motives are revealed.

> ##### An aside
> 
> Note that many of these tutorials end up translating the Go programs from GbE more than the actual semantics of Go itself.
> With Haskell being lazy and pure, there are bound to be some indirect translations, but I've tried to follow the spirit of the examples as much as possible, rather than the fine details of semantics.
> 
> Also, this tutorial will probably not end up covering STM (at least not for the forseeable future), even though it definitely counts as simple concurrency.
> This is simply because there's no real equivalent in Go by Example, so I'd have to actually think of my own tutorial structure.
> Ain't nobody got the energy for that.

## Running the code

If you've cloned this repository to run locally, once you have GHC or the Haskell platform installed, you can run any of the example files by entering, for example,

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
