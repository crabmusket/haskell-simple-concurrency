module MVars where

import Threads (sleepMs)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)

main = do
    result <- newEmptyMVar

    forkIO (do
        -- Pretend there is some actual work to do.
        sleepMs 5
        putStrLn "Calculated result!"
        putMVar result 42)

    putStrLn "Waiting..."
    value <- takeMVar result
    putStrLn ("The answer is: " ++ show value)
