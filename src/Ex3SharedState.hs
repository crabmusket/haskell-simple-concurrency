module Ex3SharedState where

import Ex1Threads (sleepMs)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar, takeMVar, modifyMVar_)
import Control.Monad (replicateM)

main = do
    counter <- newMVar 0

    let increment = modifyMVar_ counter (\c -> return $! c + 1)
    let incrementer = do
        replicateM 1000 increment
        return ()

    threads <- replicateM 5 (forkIO incrementer)

    sleepMs 10
    count <- takeMVar counter
    print count
