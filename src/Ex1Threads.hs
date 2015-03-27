module Ex1Threads where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (sequence, void)

main = do
    -- Synchronously perform some work.
    print3MessagesFrom "main"

    -- Fork a new thread to do some work in the background.
    forkIO (print3MessagesFrom "fork")

    -- Fork another thread using an inline function!
    forkIO (do
        putStrLn "starting!"
        sleepMs 2
        putStrLn "ending!")

    -- Wait for threads to finish.
    sleepMs 10

print3MessagesFrom name = void (sequence (map printMessage [1..3]))
    where printMessage i = do
            putStrLn (name ++ " number " ++ show i)
            sleepMs 1 -- Opportunity for interleaving

-- A utility function - threadDelay takes microseconds, which is slightly annoying.
sleepMs n = threadDelay (n * 1000)
