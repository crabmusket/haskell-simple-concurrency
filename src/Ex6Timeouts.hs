module Ex6Timeouts where

import Ex1Threads (sleepMs)
import Ex5Select (select)

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)

main = do
    never <- newEmptyMVar

    putStrLn "Using a default value:"
    timer <- timeout 5 "Too slow!"
    result <- select [never, timer]
    putStrLn result

    putStrLn "Using the selectOrTimeout combinator:"
    result <- selectOrTimeout 5 [never]
    case result of
        Nothing -> putStrLn "Too slow!"
        Just r  -> putStrLn r

-- Creates a new MVar that gets filled with 'value' after a given 'delay' in milliseconds.
timeout delay value = do
    var <- newEmptyMVar
    forkIO (do
        sleepMs delay
        putMVar var value)
    return var

-- This combinator provides no value ('Nothing') if the 'select' takes too long.
-- This is useful if we don't want to, or can't, provide a sensible default value.
selectOrTimeout delay vars = do
    result <- newEmptyMVar
    -- This thread patiently waits for the actual select that we want to perform.
    waiter <- forkIO (do
        value <- select vars
        putMVar result (Just value))
    -- This thread acts as a watchdog, and kills the other thread if it takes too long.
    killer <- forkIO (do
        sleepMs delay
        killThread waiter
        putMVar result Nothing)
    takeMVar result
