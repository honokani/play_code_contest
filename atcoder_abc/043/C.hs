module C
    ( main
    ) where

import Data.List
import Control.Applicative
import Control.Monad

main :: IO ()
main = do
    ns <- getN_and_Nums
    print $ minCost ns



getN_and_Nums :: (Num a, Read a, Enum a, Show a) => IO [a]
getN_and_Nums = do
    n <- read.head.words <$> getLine
    take n <$> getNumbers
    where
        getNumbers :: (Num a, Read a) => IO [a]
        getNumbers = map read.words <$> getLine

--minCost :: (Num a, Fractional a, RealFrac a, Num b, Integral b) => [a] -> b
minCost :: (Integral a) => [a] -> a
minCost ns = sum costs
    where
        avg = (fromIntegral.sum $ ns) / (fromIntegral.length $ ns)
        nearestAvg = round avg
        costs = map ((^2).(-) nearestAvg) ns

