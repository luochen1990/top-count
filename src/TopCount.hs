-- Copyright 2018 LuoChen (luochen1990@gmail.com). Licensed under the Apache License 2.0.

{-# language RankNTypes, ScopedTypeVariables #-}

module TopCount (topCountS) where

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
import Control.Arrow ((&&&))
import Stream

-- | this function is used to find the element that has the most occurrences in a stream
-- , the argument 'maxNumEstimate' means the size estimation of 's'
-- , this size estimation is used to decide the 'chunkSize'
topCountS :: HasCallStack => (Show a, Read a, Ord a) => Int -> (Stream a) -> IO (Stream (a, Int))
topCountS maxNumEstimate s = do
    s' <- sortWithS id maxNumEstimate s
    (groups, gn) <- countContinuousS s'
    groups' <- sortWithS ((negate . snd) &&& fst) gn groups
    pure groups'

test0 = do
    rst <- sumS =<< (readLinesS "t100.in")
    print rst
    rst2 <- accumlateS =<< (readLinesS "t100.in")
    --writeLinesS "rst.txt" rst2
    --(xs :: [Int]) <- collectS =<< (readLinesS "rst.txt")
    forEachS_ rst2 $ \x -> do
        print x
    --xs <- collectS rst2
    --print xs

test1 = do
    (inputs :: Stream Int) <- (readLinesS "t100.in")
    chunks <- chunksOfS 10 inputs
    printS chunks

test2 = do
    (inputs :: Stream Int) <- (readLinesS "t100.in")
    sorted <- sortWithS id 1000000 inputs
    printS sorted

test3 = do
    args <- getArgs
    fname <- if length args >= 1 then pure (head args) else getLine
    putStrLn ("Input File: " ++ fname)

    (inputs :: Stream Int) <- (readLinesS fname)
    sorted <- sortWithS id 1000000 inputs
    let outfn = (fname ++ ".out")
    writeLinesS outfn sorted
    putStrLn ("Output File: " ++ outfn)

main = do
    args <- getArgs
    fname <- if length args >= 1 then pure (head args) else getLine
    putStrLn ("Input File: " ++ fname)

    (inputs :: Stream Int) <- (readLinesS fname)
    counted <- topCountS 300000000 inputs
    let outfn = (fname ++ ".out")
    writeLinesS outfn counted
    putStrLn ("Output File: " ++ outfn)

