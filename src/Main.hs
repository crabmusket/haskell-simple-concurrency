-- Main entry point to the application.
module Main where

import qualified Threads (main)
import qualified MVars (main)
import qualified MVarSharedState (main)
import qualified Channels (main)
import qualified DuplicatingChannels (main)

-- The main entry point.
main :: IO ()
main = do
    example "Threads" Threads.main
    example "MVar" MVars.main
    example "Shared state" MVarSharedState.main
    example "Channels" Channels.main
    example "Duplicating channels" DuplicatingChannels.main

example title code = do
    putStrLn ("~~~ Running " ++ title ++ " example! ~~~")
    code
    putStrLn ""
