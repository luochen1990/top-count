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

-- | this function is used to find the Top n element that has the most occurrences in a stream
topCountS :: HasCallStack => (Show a, Read a, Ord a) => Int -> (Stream a) -> IO (Stream (a, Int))
topCountS n s = do
    s' <- sortWithS id s
    groups <- countContinuousS s'
    groups' <- sortWithS ((negate . snd) &&& fst) groups
    takeS n groups'

main = do
    args <- getArgs
    putStrLn "Input File:"
    fname <- if length args >= 1 then pure (head args) else getLine
    putStrLn ("\t" ++ fname)

    inputs <- (getLinesS fname)
    let outfn = (fname ++ ".top-count.out")
    putStrLn ("Output File:\n\t" ++ outfn ++ " ...")
    counted <- topCountS 100 inputs
    writeLinesS outfn counted
    putStrLn "Done!"

