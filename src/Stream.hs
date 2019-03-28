-- Copyright 2018 LuoChen (luochen1990@gmail.com). Licensed under the Apache License 2.0.

{-# language RankNTypes, ScopedTypeVariables, BlockArguments #-}

module Stream where

import System.Environment
import System.IO
import System.IO.Temp
import System.Directory
import Data.IORef
import Control.Monad
import Data.List (sort)
import Data.Maybe (fromJust)
import GHC.Exts (sortWith)
import GHC.Stack (HasCallStack)
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Min as PQ
import Control.Arrow ((&&&))

-- * definition of Stream

-- | Stream is an abstract data type, supports a lot of list-like operations
-- , it is fragile just like an Iterator in C++/Rust, you must use it very carefully
-- , a stream can only be consumed once
-- , and must be iterated till end, or it will cause resource leak.
newtype Stream a = Stream {next :: IO (Maybe a)} --TODO: add field: close ?

-- * tool functions about Stream

-- | mock a Stream from a List,
-- , this can be used as a tool function for test
fromListS :: HasCallStack => [a] -> IO (Stream a)
fromListS initXs = do
    l <- newIORef (Just initXs)
    pure . Stream $ do
        r <- readIORef l
        case r of
            Just (x:xs') -> writeIORef l (Just xs') >> pure (Just x)
            Just [] -> writeIORef l Nothing >> pure Nothing
            Nothing -> error "call next on an empty Stream is invalid!"

printS :: HasCallStack => Show a => Stream a -> IO ()
printS s = do
    putStr "Stream ["
    forEachS_ s $ \x -> do
        putStr (show x)
        putStr ", "
    putStr "]"

-- * Iterate Through a Stream

forEachS :: HasCallStack => Stream a -> (a -> IO b) -> IO [b]
forEachS s proc = loop where
    loop = do
        r <- next s
        case r of
            Nothing -> pure []
            Just x -> (:) <$> proc x <*> loop

forEachS_ :: HasCallStack => Stream a -> (a -> IO b) -> IO ()
forEachS_ s proc = loop where
    loop = do
        r <- next s
        case r of
            Nothing -> pure ()
            Just x -> proc x >> loop

-- * Fold & Scan

foldS :: (a -> r -> r) -> r -> Stream a -> IO r
foldS op init s = do
    r <- next s
    case r of
        Nothing -> pure init
        Just x -> op x <$> foldS op init s

scanS :: (a -> r -> r) -> r -> Stream a -> IO (Stream r)
scanS op init s = do
    visd <- newIORef False
    stat <- newIORef init
    pure . Stream $ do
        v0 <- readIORef visd
        case v0 of
            False -> (writeIORef visd True >> pure (Just init))
            True -> do
                r <- next s
                case r of
                    Just x -> do
                        st0 <- readIORef stat
                        let st1 = op x st0
                        writeIORef stat st1
                        pure (Just st1)
                    Nothing -> pure Nothing

collectS :: HasCallStack => Stream a -> IO [a]
collectS = foldS (:) []

sumS :: (Stream Int) -> IO Int
--sumS s = sum <$> collectS s
sumS = foldS (+) 0

accumlateS :: (Stream Int) -> IO (Stream Int)
accumlateS = scanS (+) 0

-- * File Operations

-- | read lines from given file
-- , the function f is used to preprocess each line
-- , the procedure close is used to clean up resources
fetchLinesS :: HasCallStack => (String -> a) -> ((String, Handle) -> IO ()) -> String -> IO (Stream a)
fetchLinesS f close filePath = do
    fh <- openFile filePath ReadMode
    hSetBuffering fh LineBuffering
    pure . Stream $ do
        flag <- hIsEOF fh
        case flag of
            True -> close (filePath, fh) >> pure Nothing
            False -> hGetLine fh >>= (pure . Just . f)

-- | get lines from given file.
getLinesS :: HasCallStack => String -> IO (Stream String)
getLinesS = fetchLinesS id (\(fn, fh) -> hClose fh)

-- | get lines from given file.
-- , the file is treated as temp and will be deleted after this iteration
getLinesS' :: HasCallStack => String -> IO (Stream String)
getLinesS' = fetchLinesS id (\(fn, fh) -> hClose fh >> removeFile fn)

-- | read lines from given file.
readLinesS :: forall a. HasCallStack => Read a => String -> IO (Stream a)
readLinesS = fetchLinesS read (\(fn, fh) -> hClose fh)

-- | read lines from given file
-- , the file is treated as temp and will be deleted after this iteration
readLinesS' :: forall a. HasCallStack => Read a => String -> IO (Stream a)
readLinesS' = fetchLinesS read (\(fn, fh) -> hClose fh >> removeFile fn)

writeLinesS :: HasCallStack => Show a => String -> (Stream a) -> IO ()
writeLinesS filePath s = do
    fh <- openFile filePath ReadWriteMode
    hSetBuffering fh LineBuffering
    let loop = do
            r <- next s
            case r of
                Just x -> hPutStrLn fh (show x) >> loop
                Nothing -> hClose fh
    loop

-- | a lightweight wrapper of 'emptySystemTempFile'
newTempFile :: IO String
newTempFile = do
    fPath <- emptySystemTempFile "Tmp"
    putStrLn ("New Temp File: " ++ fPath)
    pure fPath

-- | a lightweight wrapper of 'writeLinesS'
-- , write lines to a temp file and return this file name
writeLinesS' :: Show a => (Stream a) -> IO String
writeLinesS' s = do
    fPath <- newTempFile
    writeLinesS fPath s
    pure fPath

-- | 'writeLines' is for List
writeLines :: Show a => String -> [a] -> IO ()
writeLines filePath xs = do
    fh <- openFile filePath ReadWriteMode
    hSetBuffering fh LineBuffering
    forM_ xs $ \x -> hPutStrLn fh (show x)
    hClose fh

-- | a lightweight wrapper of 'writeLines'
-- , write lines to a temp file and return this file name
writeLines' :: Show a => [a] -> IO String
writeLines' xs = do
    fPath <- newTempFile
    writeLines fPath xs
    pure fPath

-- * Core Tool Functions about Stream

-- | turn a stream into chunks sized n
-- , notice that the order in the resulting list is reversed
chunksOfS :: HasCallStack => Int -> (Stream a) -> IO (Stream [a])
chunksOfS n s = do
    cnt <- newIORef 0
    buf <- newIORef [] --TODO: use better container

    let collectChunk = do
            c <- readIORef cnt
            if c < 0 then pure Nothing
            else do
                l <- readIORef buf
                r <- next s
                case r of
                    Just x -> do
                        if c < n
                        then do
                            writeIORef buf (x : l)
                            writeIORef cnt (c + 1)
                            collectChunk
                        else do
                            writeIORef buf [x]
                            writeIORef cnt 1
                            pure (Just l)
                    Nothing -> do
                        if c <= 0 then pure Nothing
                        else do
                            writeIORef cnt (-1)
                            pure (Just l)

    pure . Stream $ collectChunk

-- | merge n sorted streams
mergeSortedWithS :: HasCallStack => Ord a => Ord b => (a -> b) -> [(Stream a)] -> IO (Stream a)
mergeSortedWithS f ss = do
    let mp = M.fromList (zip [0..] ss)
    initXs <- forM ss next
    pq <- newIORef (PQ.fromList (zip initXs [0..]))

    let findNext = do
            pq0 <- readIORef pq
            if PQ.null pq0
            then pure Nothing
            else let ((r, i), pq1) = PQ.deleteFindMin pq0 in
                case r of
                    Just x -> do
                        x' <- next (mp M.! i)
                        writeIORef pq (PQ.insert (x', i) pq1)
                        pure (Just x)
                    Nothing -> do
                        writeIORef pq pq1
                        findNext

    pure . Stream $ findNext

-- | sort a stream 's' with a weight function 'f'
-- , the argument 'maxNumEstimate' means the size estimation of 's'
-- , this size estimation is used to decide the 'chunkSize'
sortWithS :: HasCallStack => (Show a, Read a, Ord a, Ord b)
    => (a -> b) -> Int -> (Stream a) -> IO (Stream a)
sortWithS f maxNumEstimate s = do
    let chunkSize = (max 1000 (floor (sqrt (fromIntegral maxNumEstimate))))
    putStrLn ("Sorting With Chunk Size: " ++ show chunkSize)
    chunks <- chunksOfS chunkSize s
    sortedChunks <- forEachS chunks $ \chunk -> do
        fn <- writeLines' (sortWith f chunk)
        readLinesS' fn
    mergeSortedWithS f sortedChunks

-- | count continuous equivalent elements in a stream
-- , along with number of the continuous groups
countContinuousS :: HasCallStack => Eq a => (Stream a) -> IO (Stream (a, Int), Int)
countContinuousS s = do
    latest <- newIORef Nothing
    cnt <- newIORef 0
    gnum <- newIORef 0

    let findNext = do
            c <- readIORef cnt
            if c < 0 then pure Nothing
            else do
                lat <- readIORef latest
                r <- next s
                case r of
                    Just x -> do
                        if lat == Just x then modifyIORef cnt (+1) >> findNext
                        else do
                            writeIORef latest (Just x)
                            writeIORef cnt 1
                            case lat of
                                Just lx -> modifyIORef gnum (+1) >> pure (Just (lx, c))
                                Nothing -> findNext
                    Nothing -> do
                        if c <= 0 then pure Nothing
                        else do
                            writeIORef cnt (-1)
                            modifyIORef gnum (+1)
                            pure (Just (fromJust lat, c))

    gn <- readIORef gnum
    pure (Stream findNext, gn)

