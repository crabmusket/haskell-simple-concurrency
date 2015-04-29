module MVarSharedState where

import Threads (sleepMs)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar)
import Control.Monad (replicateM)

main = do
    counter <- newMVar 0

    let increment = do
            count <- takeMVar counter
            putMVar counter $! count + 1
        incrementer = do
            replicateM 1000 increment
            return ()

    threads <- replicateM 5 (forkIO incrementer)

    sleepMs 100
    count <- takeMVar counter
    print count
