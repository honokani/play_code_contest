module B
    ( main
    ) where

import Data.List
import Control.Monad

main :: IO ()
main = do
    ss <- getLN_and_Chrs
    putStrLn $ (concat.unfoldr takeMinStr) ss

getLN_and_Chrs :: IO [String]
getLN_and_Chrs = do
    (l, n) <- mapT2 read.head.listToT2s.words <$> getLine
    replicateM l $ take n.head <$> getWords
    where
        mapT2 :: (a -> b) -> (a,a) -> (b,b)
        mapT2 f (a1,a2) = (f a1, f a2)
        listToT2s :: [a] -> [(a,a)]
        listToT2s [] = []
        listToT2s (x:[]) = []
        listToT2s (x:y:zs) = (x,y) : listToT2s zs
        getWords :: IO [String]
        getWords = words <$> getLine

takeMinStr :: [String] -> Maybe (String, [String])
takeMinStr [] = Nothing
takeMinStr ss = Just (s, delete s ss)
    where
        s = minimum ss

