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
    putStrLn "Input File:"
    fname <- if length args >= 1 then pure (head args) else getLine
    putStrLn ("\t" ++ fname)

    inputs <- (getLinesS fname)
    let outfn = (fname ++ ".sort.out")
    putStrLn ("Output File:\n\t" ++ outfn ++ " ...")
    sorted <- sortWithS id inputs
    writeLinesS outfn sorted
    putStrLn "Done!"

