module Ex2MVars where

import Ex1Threads (sleepMs)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)

main = do
    message <- newEmptyMVar

    forkIO $ do
        -- Pretend there is some actual work to do.
        sleepMs 5
        putStrLn "Sending message!"
        putMVar message "Do the thing!"

    putStrLn "Waiting..."
    result <- takeMVar message
    putStrLn ("Received message: " ++ result)