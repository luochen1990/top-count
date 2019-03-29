-- Copyright 2018 LuoChen (luochen1990@gmail.com). Licensed under the Apache License 2.0.

{-# language RankNTypes, ScopedTypeVariables, BlockArguments, OverloadedStrings #-}

module Stream where

import Prelude hiding (catch, hPutStr, hPutStrLn)
import Data.ByteString (ByteString, hPutStr, hGet, hGetLine, append)
import Data.ByteString.Char8 (pack, unpack, hPutStrLn)
import System.IO (Handle, openFile, openBinaryFile, IOMode(..), hSetBuffering, BufferMode(..), hIsEOF, hClose)
import System.IO.Temp
import System.IO.Error hiding (catch)
import System.Environment
import System.Directory
import System.Random (randomIO)
import System.FilePath.Posix ((</>))
import Data.Char
import Data.IORef
import Data.Function (on)
import Control.Monad
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.List (sort)
import Data.Maybe (fromJust, fromMaybe)
import GHC.Exts (sortWith)
import GHC.Stack (HasCallStack)
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Min as PQ
import Control.Arrow ((&&&))

import Data.Bytes.Serial
import Data.Bytes.Put
import Data.Bytes.Get
import Data.Bytes.VarInt
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize.Put (Put)
import Data.Serialize.Get (Get, Result(..), runGetPartial)

-- * Definition of Stream

-- | Stream is an abstract data type, supports a lot of list-like operations
-- , it is fragile just like an Iterator in C++/Rust, you must use it very carefully
-- , a stream can only be consumed once
-- , and must be closed after iteration, or it will cause resource leak
-- , the field 'sizeEstimation' means the size estimation of the Stream, Nothing means unknown
data Stream a = MkStream {next :: IO (Maybe a), close :: IO (), sizeEstimation :: Maybe Int}

-- | merge two sizeEstimation into one, the tight one is preserved
mergeSzEst :: Maybe Int -> Maybe Int -> Maybe Int
mergeSzEst (Just x) (Just y) = Just (min x y)
mergeSzEst (Just x) Nothing = Just x
mergeSzEst Nothing (Just y) = Just y
mergeSzEst Nothing Nothing = Nothing

-- * Tool Functions about Stream

-- | mock a Stream from a List,
-- , this can be used as a tool function for test
fromListS :: HasCallStack => [a] -> IO (Stream a)
fromListS initXs = do
    l <- newIORef (Just initXs)
    let getNext = do
            r <- readIORef l
            case r of
                Just (x:xs') -> writeIORef l (Just xs') >> pure (Just x)
                Just [] -> writeIORef l Nothing >> pure Nothing
                Nothing -> error "call next on an empty Stream is invalid!"
    pure (MkStream {next = getNext, close = pure (), sizeEstimation = Nothing})

printS :: HasCallStack => Show a => Stream a -> IO ()
printS s = do
    putStr "Stream ["
    forEachS_ s $ \x -> putStr (show x) >> putStr ", "
    close s
    putStr "]"

-- * Iterate Through a Stream

forEachS :: HasCallStack => Stream a -> (a -> IO b) -> IO [b]
forEachS s proc = loop where
    loop = do
        r <- next s
        case r of
            Just x -> (:) <$> proc x <*> loop
            Nothing -> close s >> pure []

forEachS_ :: HasCallStack => Stream a -> (a -> IO b) -> IO ()
forEachS_ s proc = loop where
    loop = do
        r <- next s
        case r of
            Just x -> proc x >> loop
            Nothing -> close s >> pure ()

-- * Fold & Scan

foldS :: (a -> r -> r) -> r -> Stream a -> IO r
foldS op init s = do
    r <- next s
    case r of
        Just x -> init `seq` foldS op (op x init) s
        Nothing -> close s >> pure init

scanS :: (a -> r -> r) -> r -> Stream a -> IO (Stream r)
scanS op init s = do
    visd <- newIORef False
    stat <- newIORef init
    let getNext = do
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
    pure (MkStream {next = getNext, close = close s, sizeEstimation = (+) <$> sizeEstimation s <*> Just 1})

collectS :: HasCallStack => Stream a -> IO [a]
collectS s = reverse <$> foldS (:) [] s

sumS :: (Stream Int) -> IO Int
--sumS s = sum <$> collectS s
sumS = foldS (+) 0

accumlateS :: (Stream Int) -> IO (Stream Int)
accumlateS = scanS (+) 0

-- * File Operations

bigBufferSize = (10 * 1024 * 1024)
bigBufferMode = BlockBuffering (Just bigBufferSize)

smallBufferSize = (4 * 1024 * 10)
smallBufferMode = BlockBuffering (Just smallBufferSize)

-- | read lines from given file
-- , the function f is used to preprocess each line
-- , the procedure closeProc is used to clean up resources
fetchLinesS :: HasCallStack => (ByteString -> a) -> ((String, Handle) -> IO ()) -> String -> IO (Stream a)
fetchLinesS f closeProc filePath = do
    fh <- openBinaryFile filePath ReadMode
    hSetBuffering fh bigBufferMode

    let getNext = do
            flag <- hIsEOF fh
            case flag of
                False -> hGetLine fh >>= (pure . Just . f)
                True -> pure Nothing

    pure (MkStream {next = getNext, close = closeProc (filePath, fh), sizeEstimation = Nothing})

-- | get lines from given file.
getLinesS :: HasCallStack => FilePath -> IO (Stream ByteString)
getLinesS = fetchLinesS id (\(fn, fh) -> hClose fh)

-- | get lines from given file.
-- , the file is treated as temp and will be deleted after this iteration
getLinesS' :: HasCallStack => FilePath -> IO (Stream ByteString)
getLinesS' = fetchLinesS id (\(fn, fh) -> hClose fh >> removeFile fn)

-- | read lines from given file.
readLinesS :: forall a. HasCallStack => Read a => FilePath -> IO (Stream a)
readLinesS = fetchLinesS (read . unpack) (\(fn, fh) -> hClose fh)

-- | read lines from given file
-- , the file is treated as temp and will be deleted after this iteration
readLinesS' :: forall a. HasCallStack => Read a => FilePath -> IO (Stream a)
readLinesS' = fetchLinesS (read . unpack) (\(fn, fh) -> hClose fh >> removeFile fn)

hWriteLinesS :: HasCallStack => Show a => Handle -> (Stream a) -> IO ()
hWriteLinesS fh s = do
    hSetBuffering fh bigBufferMode
    forEachS_ s $ \x -> hPutStrLn fh (pack (show x))

writeLinesS :: HasCallStack => Show a => FilePath -> (Stream a) -> IO ()
writeLinesS filePath s = do
    fh <- openBinaryFile filePath WriteMode
    hSetBuffering fh bigBufferMode
    forEachS_ s $ \x -> hPutStrLn fh (pack (show x))
    hClose fh

-- a robust version to create a unique temp file
newTempFile :: IOMode -> IO (String, Handle)
newTempFile mode = do
    tmpDir <- getCanonicalTemporaryDirectory
    (num :: Int) <- randomIO
    let fn = tmpDir </> ('T' : show (abs num `mod` 1000000))
    conflicted <- doesPathExist fn
    if conflicted then newTempFile mode
    else do {
        fh <- openBinaryFile fn mode;
        putStr ("(new temp file: " ++ fn ++ " " ++ show mode ++ ")");
        pure (fn, fh)
    } `catch` \e -> do
        if isDoesNotExistError e then pure () else pure ()
        putStrLn ("[WARN] newTempFile Error: " ++ show e)
        threadDelay 1000000
        newTempFile mode

-- | a lightweight wrapper of 'writeLinesS'
-- , write lines to a temp file and return this file name
writeLinesS' :: Show a => (Stream a) -> IO String
writeLinesS' s = do
    (fn, fh) <- newTempFile WriteMode
    hWriteLinesS fh s
    hClose fh
    pure fn

-- * Core Tool Functions about Stream

-- | turn a stream into chunks sized n
chunksOfS :: HasCallStack => Int -> (Stream a) -> IO (Stream (V.IOVector a))
chunksOfS n s = do
    cnt <- newIORef 0
    buf <- V.new n

    let collectChunk = do
            c <- readIORef cnt
            if c < 0 then pure Nothing
            else do
                r <- next s
                case r of
                    Just x -> do
                        if c < n
                        then do
                            V.write buf c x
                            writeIORef cnt (c + 1)
                            collectChunk
                        else do
                            l <- V.clone buf
                            V.write buf 0 x
                            writeIORef cnt 1
                            pure (Just l)
                    Nothing -> do
                        if c <= 0 then pure Nothing
                        else do
                            l <- V.clone (V.slice 0 c buf)
                            writeIORef cnt (-1)
                            pure (Just l)

    pure (MkStream {next = collectChunk, close = close s, sizeEstimation = (sizeEstimation s >>= \x -> Just (ceiling (fromIntegral x / fromIntegral n)))})

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

    pure (MkStream {next = findNext, close = forM_ ss close, sizeEstimation = foldr mergeSzEst Nothing (map sizeEstimation ss)})

-- | swap out a chunk from memory to disk
swapOutChunk :: HasCallStack => Serial a => (V.IOVector a) -> IO (Stream a)
swapOutChunk v = serialChunk' v >>= \fn -> deserialS' fn >>= \s -> pure (s {sizeEstimation = Just (V.length v)})
    where
        serialChunk' :: HasCallStack => Serial a => (V.IOVector a) -> IO FilePath
        serialChunk' chunk = do
            (fn, fh) <- newTempFile WriteMode
            hSetBuffering fh bigBufferMode
            forM_ [0 .. V.length chunk - 1] $ \i -> V.read chunk i >>= (hPutStr fh . runPutS . serialize)
            hClose fh
            pure fn

        deserialS' :: forall a. HasCallStack => Serial a => FilePath -> IO (Stream a)
        deserialS' fn = do
            fh <- openBinaryFile fn ReadMode
            hSetBuffering fh smallBufferMode
            buffer <- newIORef BS.empty

            let getNext = do
                    buf <- readIORef buffer
                    let go r = case r of
                            Done x left -> do
                                writeIORef buffer left
                                pure (Just x)
                            Partial cont -> do
                                buf2 <- hGet fh smallBufferSize
                                if BS.null buf2 then pure Nothing else go (cont buf2)
                            Fail e left -> error "parse failed in readS"
                    go (runGetPartial (deserialize :: Get a) buf)

            pure (MkStream {next = getNext, close = hClose fh >> removeFile fn, sizeEstimation = Nothing})

maxHandleNum :: Int
maxHandleNum = 1000 --NOTE: create too many Handle will cause some weild error...

-- | sort a stream 's' with a weight function 'f'
-- , this size estimation is used to decide the 'chunkSize'
sortWithS :: HasCallStack => (Serial a, Ord a, Ord b)
    => (a -> b) -> (Stream a) -> IO (Stream a)
sortWithS f s = do
    let szEst = (fromMaybe 100000000 (sizeEstimation s))
    let minChunkSize = (max (szEst `div` maxHandleNum) 1000)
    let chunkSize = (max minChunkSize (floor (sqrt (fromIntegral szEst))))
    putStrLn ("Sorting With Chunk Size: " ++ show chunkSize)
    gnum <- newIORef 1
    chunks <- chunksOfS chunkSize s
    sortedChunks <- forEachS chunks $ \chunk -> do
        gn <- readIORef gnum
        putStr ("  Sorting Chunk " ++ show gn ++ "(sz: " ++ show (V.length chunk) ++ ") -- ")
        Intro.sortBy (compare `on` f) chunk
        swapped <- swapOutChunk chunk
        putStrLn (" -- Done.")
        writeIORef gnum (gn+1)
        pure swapped
    gn <- readIORef gnum
    s' <- (mergeSortedWithS f sortedChunks)
    pure (s' {sizeEstimation = mergeSzEst (sizeEstimation s) (Just (gn * chunkSize))})

-- | take first n elements of a Stream
takeS :: HasCallStack => Int -> (Stream a) -> IO (Stream a)
takeS n s = do
    cnt <- newIORef 0

    let getNext = do
            c <- readIORef cnt
            if c < n
            then do
                r <- next s
                case r of
                    (Just x) -> do
                        writeIORef cnt (c+1)
                        pure (Just x)
                    Nothing -> pure Nothing
            else
                pure Nothing

    pure (MkStream {next = getNext, close = close s, sizeEstimation = Just n}) --TODO: consider sizeEst s

-- | count continuous equivalent elements in a Stream
countContinuousS :: HasCallStack => Eq a => (Stream a) -> IO (Stream (a, Int))
countContinuousS s = do
    latest <- newIORef Nothing
    cnt <- newIORef 0

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
                                Just lx -> pure (Just (lx, c))
                                Nothing -> findNext
                    Nothing -> do
                        if c <= 0 then pure Nothing
                        else do
                            writeIORef cnt (-1)
                            pure (Just (fromJust lat, c))

    pure (MkStream {next = findNext, close = close s, sizeEstimation = sizeEstimation s})

