-- Copyright 2018 LuoChen (luochen1990@gmail.com). Licensed under the Apache License 2.0.

{-# language RankNTypes, ScopedTypeVariables, BlockArguments, OverloadedStrings #-}

module Gen where

import Prelude hiding (catch, hPutStr, hPutStrLn)
import Data.ByteString (ByteString, hPutStr, hGetLine, append)
import Data.ByteString.Char8 (pack, unpack, hPutStrLn)
import System.IO (Handle, withBinaryFile, IOMode(..), hSetBuffering, BufferMode(..), hIsEOF, hClose)
import System.IO.Temp
import System.Environment
import Test.QuickCheck
import Generic.Random
import Control.Monad (replicateM_)

arbNat :: Int -> Gen Int
arbNat n = resize n arbitrary `suchThat` (>= 0)

arbChar :: Gen Char
arbChar = elements ('/' : '_' : '.' : ['a' .. 'z'])

arbStr :: Int -> Gen ByteString
arbStr n = pack <$> resize n (listOf arbChar)

arbURL :: Int -> Gen ByteString
arbURL n = oneof [ ("http://" `append`) <$> arbStr (n-7), ("https://" `append`) <$> arbStr (n-8) ]

genNumbers :: Int -> Int -> String -> IO ()
genNumbers m n filePath = withBinaryFile filePath WriteMode $ \fh -> do
    replicateM_ n ((pack . show) <$> generate (arbNat m) >>= hPutStrLn fh)

genStrings :: Int -> Int -> String -> IO ()
genStrings m n filePath = withBinaryFile filePath WriteMode $ \fh -> do
    replicateM_ n (generate (arbStr m) >>= hPutStrLn fh)

genURLs :: Int -> Int -> String -> IO ()
genURLs m n filePath = withBinaryFile filePath WriteMode $ \fh -> do
    replicateM_ n (generate (arbURL m) >>= hPutStrLn fh)

genFile :: Maybe String -> IO ()
genFile Nothing = putStrLn "Sample Input:\n\tn100 (100 numbers)\n\ts1000 (1000 strings)\n\tu10 (10 URLs)" >> getLine >>= (genFile . Just)
genFile (Just arg) = do
    let (prefix : numlit) = arg
    if prefix `elem` ['n', 's', 'u'] && (head numlit) `elem` ['1'..'9']
    then do
        let num = read numlit
        let outfn = (prefix : show num ++ ".in")
        putStrLn ("Output File: " ++ outfn)
        case prefix of
            'n' -> genNumbers 1000 num outfn
            's' -> genStrings 1000 num outfn
            'u' -> genURLs 1000 num outfn
    else genFile Nothing

main = do
    args <- getArgs
    genFile (if length args >= 1 then Just (head args) else Nothing)

