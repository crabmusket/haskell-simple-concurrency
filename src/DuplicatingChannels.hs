module DuplicatingChannels where

import Threads (sleepMs)

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, writeChan, readChan, dupChan)

nonDuplicatedTest = do
    messages <- newChan
    forkIO (messageReader messages "First")
    forkIO (messageReader messages "Second")
    writeChan messages "Hi!"

messageReader channel name = do
    msg <- readChan channel
    putStrLn (name ++ " read: " ++ msg)

duplicatedTest = do
    broadcast <- newChan
    forkIO (broadcastReader broadcast "Third")
    forkIO (broadcastReader broadcast "Fourth")
    writeChan broadcast "Bye!"

broadcastReader channel name = do
    channel' <- dupChan channel
    messageReader channel' name

main = do
    nonDuplicatedTest
    duplicatedTest
    sleepMs 5
