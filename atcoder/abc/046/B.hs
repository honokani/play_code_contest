module B
    ( main
    ) where

import Data.List
import Control.Monad

main :: IO ()
main = do
    (n:c:[]) <- getNumbers
    print $ culcColor n c

culcColor :: Int -> Int -> Int
culcColor n c = product $ c:(take (n-1) $ cycle [(c-1)])

----------------

getNumbers :: (Num a, Read a, Enum a) => IO [a]
getNumbers = map read.words <$> getLine

