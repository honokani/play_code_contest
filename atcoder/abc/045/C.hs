module C
    ( main
    ) where

import Control.Arrow
import Control.Monad

main :: IO ()
main = do
    cs <-  getWord
    print $ culcSums cs

getWord :: IO String
getWord = head.words <$> getLine


culcSums :: String -> Int
culcSums =  sum.map (sum.init.map read).splitNums

splitNums :: String -> [[String]]
splitNums [] = return [[]]
splitNums s = do
    [f,s] <- map (\x -> [take x s, drop x s]) [1..length s]
    map ((:) f) $ splitNums s

