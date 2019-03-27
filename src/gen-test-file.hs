import System.Environment
import System.IO
import Test.QuickCheck
import Generic.Random
import Control.Monad (replicateM_)

genTestFile :: Int -> String -> IO ()
genTestFile n filePath = withFile filePath WriteMode $ \fh -> do
    replicateM_ n (show <$> generate ((resize 100 arbitrary `suchThat` (>0)) :: Gen Int) >>= hPutStrLn fh)

main = do
    args <- getArgs
    num <- read <$> (if length args >= 1 then pure (head args) else getLine)
    let outfn = ("t" ++ show num ++ ".in")
    genTestFile num outfn
    putStrLn ("Output File: " ++ outfn)

