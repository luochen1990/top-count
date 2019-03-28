-- Copyright 2018 LuoChen (luochen1990@gmail.com). Licensed under the Apache License 2.0.

{-# language RankNTypes, ScopedTypeVariables #-}

module TopCount where

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

main = do
    args <- getArgs
    putStr "Input File: "
    fname <- if length args >= 1 then pure (head args) else getLine
    putStrLn fname

    let outfn = (fname ++ ".top-count.out")
    putStr ("Output File: " ++ outfn)
    (inputs :: Stream Int) <- (readLinesS fname)
    counted <- topCountS 300000000 inputs
    writeLinesS outfn counted
    putStrLn " Done!"

