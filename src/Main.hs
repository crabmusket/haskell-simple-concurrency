-- Main entry point to the application.
module Main where

import qualified Ex1Threads (main)
import qualified Ex2MVars (main)
import qualified Ex3Channels (main)
import qualified Ex4DuplicatingChannels (main)

-- The main entry point.
main :: IO ()
main = do
    example "Threads" Ex1Threads.main
    example "MVar" Ex2MVars.main
    example "Channels" Ex3Channels.main
    example "Duplicating channels" Ex4DuplicatingChannels.main

example title code = do
    putStrLn ("~~~ Running " ++ title ++ " example! ~~~")
    code
    putStrLn ""
