module C
    ( main
    ) where

import Data.List

main :: IO ()
main = do
    [h,w,fh,fw] <- getNumbers
    print $ culcAllWay h w

getNumbers :: (Num a, Read a) => IO [a]
getNumbers = map read.words <$> getLine

fact :: (Num a, Eq a) => a -> a
fact 1 = 1
fact n = n * fact (n-1)

culcAllWay h w = (fact (h+w-2)) / (fact (h-1) * fact (w-1))

