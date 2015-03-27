module Ex7ComposableSelect where

import Ex1Threads (sleepMs)
import Ex5Select (worker)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Data.Foldable (for_)
import Data.Traversable (for)

main = do
    -- Fork a couple of humans to do some work.
    employees <- for ["Harry", "Sally", "Aang"] (\name -> do
        item <- newEmptyMVar
        forkIO (worker name item)
        return item)

    -- A very efficient robot will also do some work.
    robot <- newEmptyMVar
    forkIO (putMVar robot "Bleep bloop, puny humans.")

    -- Let the battle for the future of the Earth begin.
    fastestHuman <- select employees
    battle <- select [fastestHuman, robot]
    result <- takeMVar battle
    putStrLn result

select vars = do
    won <- newEmptyMVar
    for_ vars (\var -> forkIO (do
        val <- takeMVar var
        putMVar won val))
    -- Return the MVar, rather than awaiting its result.
    return won
