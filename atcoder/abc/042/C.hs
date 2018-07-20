module C
    ( main
    ) where

import Data.List

main :: IO ()
main = do
    (min, fobs) <- getData_and_Nnums
    putStrLn $ show.searchMin min $ [0..9] \\ fobs


getData_and_Nnums :: (Num a, Read a, Enum a, Show a) => IO (Int, [a])
getData_and_Nnums = do
    (d, n) <- mapT2 read.head.listToT2s.words <$> getLine
    xs <- take n <$> getNumbers
    return (d, xs)
    where
        mapT2 :: (a -> b) -> (a,a) -> (b,b)
        mapT2 f (a1,a2) = (f a1, f a2)
        listToT2s :: [a] -> [(a,a)]
        listToT2s [] = []
        listToT2s (x:[]) = []
        listToT2s (x:y:zs) = (x,y) : listToT2s zs
        getNumbers :: (Num a, Read a) => IO [a]
        getNumbers = map read.words <$> getLine

searchMin :: (Num a, Ord a) => a -> [a] -> a
searchMin m ns = pickNum.genNum.makeList $ ns
    where
        pickNum = head.filter (m<=).map (foldl (\acc x -> acc*10+x) 0)
        makeList = map (\x -> [x])
        fstNl = makeList $ delete 0 ns
        genNum nl = fstNl ++ [ x++n | x <- genNum nl, n <- nl]

