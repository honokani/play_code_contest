module D
    ( main
    ) where

import Data.List

main :: IO ()
main = do
    hands <- getLine
    print $ culcPoint hands

culcPoint :: String -> Int
culcPoint = culcPoint.countExtraG
    where
        countExtraG :: String -> Int
        countExtraG [] = 0
        countExtraG (x:xs) = (if x=='g' then (+) else (-)) (countExtraG xs) 1
        culcPoint :: Int -> Int
        culcPoint n = floor $ fromIntegral n / 2

