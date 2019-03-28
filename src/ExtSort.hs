-- Copyright 2018 LuoChen (luochen1990@gmail.com). Licensed under the Apache License 2.0.

{-# language RankNTypes, ScopedTypeVariables #-}

module ExtSort where

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

main = do
    args <- getArgs
    putStr "Input File: "
    fname <- if length args >= 1 then pure (head args) else getLine
    putStrLn fname

    (inputs :: Stream Int) <- (readLinesS fname)
    let outfn = (fname ++ ".sort.out")
    putStr ("Output File: " ++ outfn)
    sorted <- sortWithS id inputs
    writeLinesS outfn sorted
    putStrLn " Done!"

