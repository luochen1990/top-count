{-# language RankNTypes, ScopedTypeVariables, BlockArguments #-}

import Debug.Trace
import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Exception hiding (assert)
import Test.Hspec hiding (Spec, example)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Stream
import TopCount

(<?>) :: (Testable p) => p -> String -> Property
(<?>) = flip (Test.QuickCheck.counterexample . ("Extra Info: " ++))
infixl 2 <?>

main :: IO ()
main = hspec $ do
    describe "Arith" $ do
        prop "plus-comm" $
            \(x :: Int) (y :: Int) ->
                collect (x == y) $ x + y === y + x

    describe "Stream" $ do
        prop "collectS . fromListS = id" $
            \(xs :: [Int]) -> monadicIO $ do
                xs' <- run ((collectS <=< fromListS) xs)
                assert (xs == xs')

        prop "sortWithS did sort things" $
            \(xs :: [Int]) -> monadicIO $ do
                xs' <- run (fromListS xs >>= (sortWithS id 10) >>= collectS)
                assert (xs' == sort xs)

        prop "mergeSortedWithS did produce ordered stream"
            \(xs :: [Int], ys :: [Int]) -> monadicIO $ do
                let xs' = sort xs
                let ys' = sort ys
                s1 <- run $ fromListS xs'
                s2 <- run $ fromListS ys'
                zs <- run $ mergeSortedWithS id [s1, s2] >>= collectS
                assert (zs == sort zs)

        prop "chunksOfS keeps the elem number"
            \(k :: Int) -> k > 0 && k < 10 ==>
                \(xs :: [Int]) -> monadicIO $ do
                    s <- run $ fromListS xs
                    ss <- run $ chunksOfS k s
                    xss <- run $ collectS ss
                    assert (sum (map length xss) == length xs)

