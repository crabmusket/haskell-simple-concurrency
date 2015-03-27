-- | Main entry point to the application.
module Main where

import qualified Ex1Threads (main)
import qualified Ex2MVars (main)
import qualified Ex5Select (main)

-- | The main entry point.
main :: IO ()
main = do
    --example "Threads" Ex1Threads.main
    --example "MVar" Ex2MVars.main
    example "Select" Ex5Select.main

example title code = do
    putStrLn ("~~~ Running " ++ title ++ " example! ~~~")
    code
    putStrLn ""
