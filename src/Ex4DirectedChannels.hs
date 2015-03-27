module Ex4DirectedChannels where

import Ex1Threads (sleepMs)
import DirectedChannels (WriteOnlyChan, writeOnly, writeWOChan,
                         ReadOnlyChan,  readOnly,  readROChan)

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan)

main = do
    messages <- newChan
    forkIO (producer (writeOnly messages))
    forkIO (consumer (readOnly  messages))
    sleepMs 5

producer chan = do
    writeWOChan chan "Hello,"
    writeWOChan chan "Dave."

consumer chan = do
    putStrLn =<< readROChan chan
    putStrLn =<< readROChan chan
