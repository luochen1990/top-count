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

genFile :: Maybe String -> IO ()
genFile Nothing = putStrLn "Sample Input:\n\tn100 (100 numbers)\n\ts1000 (1000 strings)\n\tu10 (10 URLs)" >> getLine >>= (genFile . Just)
genFile (Just arg) = do
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
    else genFile Nothing

main = do
    args <- getArgs
    genFile (if length args >= 1 then Just (head args) else Nothing)

