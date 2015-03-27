-- Main entry point to the application.
module Main where

import qualified Ex1Threads (main)
import qualified Ex2MVars (main)
import qualified Ex3Channels (main)
import qualified Ex4DirectedChannels (main)
import qualified Ex5Select (main)
import qualified Ex6Timeouts (main)
import qualified Ex7ComposableSelect (main)

-- The main entry point.
main :: IO ()
main = do
    example "Threads" Ex1Threads.main
    example "MVar" Ex2MVars.main
    example "Channels" Ex3Channels.main
    example "Directed channels" Ex4DirectedChannels.main
    example "Select" Ex5Select.main
    example "Timeouts" Ex6Timeouts.main
    example "Composable select" Ex7ComposableSelect.main

example title code = do
    putStrLn ("~~~ Running " ++ title ++ " example! ~~~")
    code
    putStrLn ""
