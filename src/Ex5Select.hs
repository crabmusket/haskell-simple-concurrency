module Ex5Select where

import Ex1Threads (sleepMs)

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Data.Foldable (for_)
import System.Random (randomRIO)

main = do
    item1 <- newEmptyMVar
    item2 <- newEmptyMVar

    putStrLn "Let the race begin!"
    forkIO (worker "Harry" item1)
    forkIO (worker "Sally" item2)

    name <- selectNow [item1, item2]
    putStrLn (name ++ " finished first!")

-- Wait on each MVar in 'vars', and return the first value which is put into any of them.
selectNow vars = do
    winner <- newEmptyMVar
    for_ vars (\var -> forkIO (do
        val <- takeMVar var
        tryPutMVar winner val))
    takeMVar winner

-- These workers simply return their name when they're done doing work.
-- Guess they're more concerned with apparances than results!
worker name result = do
    -- Not all work is predictable...
    delay <- randomRIO (50, 100)
    -- ...or useful.
    sleepMs delay
    putMVar result name
