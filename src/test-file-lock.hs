{-# language RankNTypes, ScopedTypeVariables #-}

import System.IO

filePath = "test.txt"

main = do
    fh1 <- openFile filePath ReadMode -- ReadWriteMode -- AppendMode
    fh2 <- openFile filePath ReadMode
    l1 <- hGetLine fh1
    print l1
    l2 <- hGetLine fh2
    print l2

