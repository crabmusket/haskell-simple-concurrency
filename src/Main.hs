-- | Main entry point to the application.
module Main where

import qualified Ex1Threads (main)

-- | The main entry point.
main :: IO ()
main = do
    example "Threads" Ex1Threads.main

example title code = do
    putStrLn ("~~~ Running " ++ title ++ " example! ~~~")
    code
    putStrLn ""
