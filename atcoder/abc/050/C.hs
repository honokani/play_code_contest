module C where

import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict as IntMap


limit = 7+10^9

main :: IO ()
main = do
    ins <- getInputs
    print $ uncurry solve ins


getInputs :: IO (Int, [Int])
getInputs = do
    n <- read <$> getLine
    as <- take n.map read.words <$> getLine
    return (n, as)


solve :: Int -> [Int] -> Int
solve n as = if checkElms makeElems as then mod (2 ^ size) limit else 0
    where
        size = div n 2
        makeElems
            | odd n     = IntMap.fromList $ ((0,1) :) $ zip [2,4..size+1] $ repeat 2
            | otherwise = IntMap.fromList $ zip [1,3..size+1] $ repeat 2
        checkElms :: IntMap Int -> [Int] -> Bool
        checkElms eMemo xs = ((==) IntMap.empty) $ IntMap.filter (/=0) $ checkElmsC eMemo xs
        checkElmsC :: IntMap Int -> [Int] -> IntMap Int
        checkElmsC eM [] = eM
        checkElmsC eM (e:es) = checkElmsC (IntMap.update (\x -> Just $ x-1) e eM) es

