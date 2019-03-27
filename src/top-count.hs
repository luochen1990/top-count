-- Copyright 2018 LuoChen (luochen1990@gmail.com). Licensed under the Apache License 2.0.

{-# language RankNTypes, ScopedTypeVariables #-}

import System.Environment
import System.IO
import System.IO.Temp
import Data.IORef
import Control.Monad
import Data.List (sort)
import Data.Maybe (fromJust)
import GHC.Exts (sortWith)
import GHC.Stack (HasCallStack)
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Min as PQ

-- | a stream can only be consumed once
-- , and must be iterated till end, or it will cause resource leak.
newtype Stream a = Stream {next :: IO (Maybe a)} --TODO: add field: close ?

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

foldS :: (a -> r -> r) -> r -> Stream a -> IO r
foldS op init s = do
    r <- next s
    case r of
        Nothing -> pure init
        Just x -> op x <$> foldS op init s

collectS :: HasCallStack => Stream a -> IO [a]
collectS = foldS (:) []

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

readLinesS :: forall a. HasCallStack => Read a => String -> IO (Stream a)
readLinesS filePath = do
    fh <- openFile filePath ReadMode
    hSetBuffering fh LineBuffering
    pure . Stream $ do
        flag <- hIsEOF fh
        case flag of
            True -> hClose fh >> pure Nothing
            False -> hGetLine fh >>= \l ->
                --print l >>
                Just <$> (readIO l :: IO a)

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

newTempFile :: IO String
newTempFile = do
    fPath <- emptySystemTempFile "Tmp"
    print ("New Temp File : " ++ fPath)
    pure fPath

-- | write lines to a temp file and return this file name
writeLinesS' :: Show a => (Stream a) -> IO String
writeLinesS' s = do
    fPath <- newTempFile
    writeLinesS fPath s
    pure fPath

writeLines :: Show a => String -> [a] -> IO ()
writeLines filePath xs = do
    fh <- openFile filePath ReadWriteMode
    hSetBuffering fh LineBuffering
    forM_ xs $ \x -> hPutStrLn fh (show x)
    hClose fh

writeLines' :: Show a => [a] -> IO String
writeLines' xs = do
    fPath <- newTempFile
    writeLines fPath xs
    pure fPath

---- | turn a stream into chunks sized n (stream of streams)
--chunksOfS :: Int -> (Stream a) -> IO (Stream (Stream a))
--chunksOfS n s = do
--    cnt <- newIORef 0
--    buf <- newIORef []
--
--    let collectChunk = do
--        c <- readIORef cnt
--        l <- readIORef buf
--        r <- next s
--        case r of
--            Just x -> do
--                if c < n
--                then do
--                    writeIORef buf (x : l)
--                    writeIORef cnt (c + 1)
--                    collectChunk
--                else do
--                    writeIORef buf []
--                    writeIORef cnt 0
--                    pure (Just l)
--            Nothing -> do
--                if c <= 0 then pure Nothing
--                else do
--                    writeIORef buf []
--                    writeIORef cnt 0
--                    pure (Just l)
--
--    pure . Stream $ collectChunk

-- | turn a stream into chunks sized n
-- , notice that the order in the resulting list is reversed
chunksOfS :: HasCallStack => Int -> (Stream a) -> IO (Stream [a])
chunksOfS n s = do
    cnt <- newIORef 0
    buf <- newIORef []

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


sumS :: (Stream Int) -> IO Int
--sumS s = sum <$> collectS s
sumS = foldS (+) 0

accumlateS :: (Stream Int) -> IO (Stream Int)
accumlateS = scanS (+) 0

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

-- | count continuous equivalent elements in a stream
-- , along with numbers of the continuous groups
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

---- | transpose a stream of list into a list of stream
---- , notice that the order of the elements of the outer container is reversed
--transposeS :: (Show a, Read a) => (Stream [a]) -> IO [(Stream a)]
--transposeS s = do
--    res <- newIORef []
--    forEachS s $ \chunk -> do
--        fn <- writeLines' chunk
--        s0 <- readLinesS fn
--        modifyIORef res (s0:)
--    r <- readIORef res
--    pure r

sortWithS :: HasCallStack => (Show a, Read a, Ord a, Ord b) => (a -> b) -> (Stream a) -> IO (Stream a)
sortWithS f s = do
    chunks <- chunksOfS 1000 s
    sortedChunks <- forEachS chunks $ \chunk -> do
        fn <- writeLines' (sortWith f chunk)
        readLinesS fn
    mergeSortedWithS f sortedChunks

printS :: HasCallStack => Show a => Stream a -> IO ()
printS s = do
    putStr "Stream ["
    forEachS_ s $ \x -> do
        putStr (show x)
        putStr ", "
    putStr "]"

test0 = do
    rst <- sumS =<< (readLinesS "test.txt")
    print rst
    rst2 <- accumlateS =<< (readLinesS "test.txt")
    --writeLinesS "rst.txt" rst2
    --(xs :: [Int]) <- collectS =<< (readLinesS "rst.txt")
    forEachS_ rst2 $ \x -> do
        print x
    --xs <- collectS rst2
    --print xs

test1 = do
    (inputs :: Stream Int) <- (readLinesS "test.txt")
    chunks <- chunksOfS 10 inputs
    printS chunks

test2 = do
    (inputs :: Stream Int) <- (readLinesS "test.txt")
    sorted <- sortWithS id inputs
    printS sorted

main = do
    args <- getArgs
    fname <- if length args >= 1 then pure (head args) else getLine
    putStrLn ("Input File: " ++ fname)

    (inputs :: Stream Int) <- (readLinesS fname)
    sorted <- sortWithS id inputs
    let outfn = (fname ++ ".out")
    writeLinesS outfn sorted
    putStrLn ("Output File: " ++ outfn)

