module Ex5Select where

import Ex1Threads (sleepMs)

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Control.Monad (forM)
import System.Random (randomRIO)

main = do
    item1 <- newEmptyMVar
    item2 <- newEmptyMVar

    putStrLn "Let the race begin!"
    forkIO (worker "Harry" item1)
    forkIO (worker "Sally" item2)

    name <- select [item1, item2]
    putStrLn (name ++ " finished first!")


select vars = do
    won <- newEmptyMVar
    contestants <- forM vars (\var -> forkIO (do
        val <- takeMVar var
        putMVar won val))
    winner <- takeMVar won
    forM contestants killThread
    return winner

-- These workers simply return their name when they're done doing work.
-- Guess they're more concerned with apparances than results!
worker name result = do
    -- Not all work is predictable...
    delay <- randomRIO (50, 100)
    -- ...or useful.
    sleepMs delay
    putMVar result name