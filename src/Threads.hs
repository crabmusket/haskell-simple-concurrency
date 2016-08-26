module Threads where

import Control.Concurrent (forkIO, threadDelay)
import Data.Foldable (for_)

main = do
    -- Synchronously perform some work.
    printMessagesFrom "main"

    -- Fork a new thread to do some work in the background.
    forkIO (printMessagesFrom "fork")

    -- Fork another thread using an inline function!
    forkIO (do
        putStrLn "starting!"
        sleepMs 5
        putStrLn "ending!")

    -- Wait for threads to finish.
    sleepMs 10

-- A simple function that prints three messages with a little delay between them.
printMessagesFrom name = for_ [1..3] printMessage
    where printMessage i = do
            sleepMs 1
            putStrLn (name ++ " number " ++ show i)

-- A utility function - threadDelay takes microseconds, which is slightly annoying.
sleepMs n = threadDelay (n * 1000)
