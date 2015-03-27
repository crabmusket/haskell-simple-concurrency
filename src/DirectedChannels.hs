module DirectedChannels (
    WriteOnlyChan, writeOnly, writeWOChan,
    ReadOnlyChan,  readOnly,  readROChan
) where

import Control.Concurrent.Chan (Chan, writeChan, readChan)

newtype WriteOnlyChan a = WriteOnlyChan (Chan a)

writeOnly :: Chan a -> WriteOnlyChan a
writeOnly = WriteOnlyChan

writeWOChan :: WriteOnlyChan a -> a -> IO ()
writeWOChan (WriteOnlyChan c) = writeChan c

newtype ReadOnlyChan a = ReadOnlyChan (Chan a)

readOnly :: Chan a -> ReadOnlyChan a
readOnly = ReadOnlyChan

readROChan :: ReadOnlyChan a -> IO a
readROChan (ReadOnlyChan c) = readChan c
