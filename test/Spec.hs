{-# language RankNTypes, ScopedTypeVariables, BlockArguments #-}

import Debug.Trace
import qualified Data.Map as M
import Data.List
import GHC.Exts (sortWith)
import Control.Monad
import Control.Exception hiding (assert)
import Test.Hspec hiding (Spec, example)
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Stream
import TopCount hiding (main)
import qualified Data.Vector as IV
import qualified Data.Vector.Mutable as V
import Control.Arrow ((&&&))

(<?>) :: (Testable p) => p -> String -> Property
(<?>) = flip (Test.QuickCheck.counterexample . ("Extra Info: " ++))
infixl 2 <?>

nat :: Int -> Gen Int
nat n = (resize n arbitrary `suchThat` (>= 0))

nat' :: Int -> Gen Int
nat' n = (resize n arbitrary `suchThat` (> 0))

main :: IO ()
main = hspec $ do
    describe "Arith" $ do
        prop "plus-comm" $
            \(x :: Int) (y :: Int) ->
                collect (x == y) $ x + y === y + x

    describe "Stream" $ do
        prop "readLinesS' . writeLinesS' = id" $
            forAll (listOf (nat' 100)) $ \xs -> monadicIO $ do
                fn <- run $ fromListS xs >>= writeLinesS'
                xs' <- run $ readLinesS' fn >>= collectS
                monitor (<?> ("xs': " ++ show xs'))
                monitor (<?> ("xs: " ++ show xs))
                assert (xs' == xs)

        prop "collectS . fromListS = id" $
            forAll (listOf (nat' 100)) $ \xs -> monadicIO $ do
                xs' <- run ((collectS <=< fromListS) xs)
                assert (xs == xs')

        prop "sortWithS did sort things" $
            forAll (listOf (nat' 100)) $ \xs -> monadicIO $ do
                xs' <- run (fromListS xs >>= (sortWithS id) >>= collectS)
                monitor (<?> ("xs': " ++ show xs'))
                monitor (<?> ("sort xs: " ++ show (sort xs)))
                --[28,39,72,5]
                assert (xs' == sort xs)

        prop "mergeSortedWithS did produce ordered stream" $
            forAll (listOf (nat' 100)) $ \xs ->
                forAll (listOf (nat' 100)) $ \ys -> monadicIO $ do
                    let xs' = sort xs
                    let ys' = sort ys
                    s1 <- run $ fromListS xs'
                    s2 <- run $ fromListS ys'
                    zs <- run $ mergeSortedWithS id [s1, s2] >>= collectS
                    assert (zs == sort zs)

        prop "chunksOfS keeps the elem number" $
            forAll (nat' 10) $ \k ->
                forAll (listOf (nat' 100)) $ \xs -> monadicIO $ do
                    s <- run $ fromListS xs
                    ss <- run $ chunksOfS k s
                    xss <- run $ collectS ss
                    assert (sum (map V.length xss) == length xs)

        prop "takeS ~= take" $
            forAll (nat 100) $ \k ->
                forAll (listOf (nat' 100)) $ \xs -> monadicIO $ do
                    s <- run $ fromListS xs
                    s' <- run $ takeS k s
                    xs' <- run $ collectS s'
                    assert (xs' == take k xs)

        prop "swapOutChunk ~= id" $
            forAll (listOf (nat' 100)) $ \xs -> monadicIO $ do
                v <- run $ IV.thaw (IV.fromList xs)
                s <- run $ swapOutChunk v
                xs' <- run $ collectS s
                assert (xs' == xs)

    describe "TopCount" $ do
        prop "topCountS works" $
            forAll (nat 100) $ \k ->
                forAll (listOf (nat' 100)) $ \xs -> monadicIO $ do
                    s <- run $ fromListS xs
                    s' <- run $ topCountS k s
                    groups <- run $ collectS s'
                    assert (groups == sortWith ((negate . snd) &&& fst) groups)

