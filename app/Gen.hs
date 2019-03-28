-- Copyright 2018 LuoChen (luochen1990@gmail.com). Licensed under the Apache License 2.0.

module Gen where

import System.Environment
import System.IO
import Test.QuickCheck
import Generic.Random
import Control.Monad (replicateM_)

arbNat :: Int -> Gen Int
arbNat n = resize n arbitrary `suchThat` (>= 0)

arbChar :: Gen Char
arbChar = elements ('/' : '_' : '.' : ['a' .. 'z'])

arbStr :: Int -> Gen String
arbStr n = resize n (listOf arbChar)

arbURL :: Int -> Gen String
arbURL n = oneof [ ("http://" ++) <$> arbStr (n-7), ("https://" ++) <$> arbStr (n-8) ]

genNumbers :: Int -> Int -> String -> IO ()
genNumbers m n filePath = withFile filePath WriteMode $ \fh -> do
    replicateM_ n (show <$> generate (arbNat m) >>= hPutStrLn fh)

genStrings :: Int -> Int -> String -> IO ()
genStrings m n filePath = withFile filePath WriteMode $ \fh -> do
    replicateM_ n (generate (arbStr m) >>= hPutStrLn fh)

genURLs :: Int -> Int -> String -> IO ()
genURLs m n filePath = withFile filePath WriteMode $ \fh -> do
    replicateM_ n (generate (arbURL m) >>= hPutStrLn fh)

genFile :: String -> IO ()
genFile arg = do
    let (prefix : numlit) = arg
    if prefix `elem` "nsu" && (head numlit) `elem` ['1'..'9']
    then do
        let num = read numlit
        let outfn = (prefix : show num ++ ".in")
        putStrLn ("Output File: " ++ outfn)
        case prefix of
            'n' -> genNumbers 1000 num outfn
            's' -> genStrings 1000 num outfn
            'u' -> genURLs 1000 num outfn
    else putStrLn "Sample Input:\n n100 (100 numbers)\n s1000 (1000 strings)" >> getLine >>= genFile

main = do
    args <- getArgs
    (if length args >= 1 then pure (head args) else getLine) >>= genFile

