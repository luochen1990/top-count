-- Copyright 2018 LuoChen (luochen1990@gmail.com). Licensed under the Apache License 2.0.

{-# language RankNTypes, ScopedTypeVariables #-}

module TopCount where

import System.Environment
import System.IO
import System.IO.Temp
import Data.IORef
import Control.Monad
import Data.Maybe (fromJust)
import GHC.Stack (HasCallStack)
import qualified Data.Map.Strict as M
import qualified Data.PQueue.Min as PQ
import Control.Arrow ((&&&))
import Data.Bytes.Serial
import Stream

-- | this function is used to find the Top n element that has the most occurrences in a stream
topCountS :: HasCallStack => (Serial a, Ord a) => Int -> (Stream a) -> IO (Stream (a, Int))
topCountS n s = do
    putStrLn $ "[Begin] sortWithS s"
    s' <- sortWithS id s
    putStrLn $ "[ End ] sortWithS s  (szEst: " ++ show (sizeEstimation s') ++ ")"
    putStrLn $ "[Begin] countContinuousS s"
    groups <- countContinuousS s'
    putStrLn $ "[ End ] countContinuousS s  (szEst: " ++ show (sizeEstimation groups) ++ ")"
    putStrLn $ "[Begin] sortWithS groups"
    groups' <- sortWithS ((negate . snd) &&& fst) groups
    putStrLn $ "[ End ] sortWithS groups  (szEst: " ++ show (sizeEstimation groups') ++ ")"
    putStrLn $ "[Begin] takeS groups"
    res <- takeS n groups'
    putStrLn $ "[ End ] takeS groups  (szEst: " ++ show (sizeEstimation res) ++ ")"
    pure res

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
    putStrLn "All Done!"

