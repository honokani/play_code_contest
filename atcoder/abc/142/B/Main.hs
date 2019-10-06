module Main where

import Data.Maybe (fromJust)
import Data.List
import qualified Data.ByteString.Char8 as BS

bytestr2Int = fst . fromJust . BS.readInt
getInt = bytestr2Int <$> BS.getLine
getIntL_H = map bytestr2Int. BS.words <$> BS.getLine
getIntL_V n = replicate n $ getInt
getInt2D n = replicate n $ getIntL_H


main :: IO ()
main = do
    n <- getInt
    ns <- getIntL_H
    putStrLn . makeList . map (show . snd) $ sortStudents n $ zip ns [1..]

makeList :: [String] -> String
makeList [x]  = x
makeList (x:xs) = x ++ ' ':(makeList xs)

sortStudents :: Int -> [(Int,Int)] -> [(Int,Int)]
sortStudents 0 _ = []
sortStudents 1 x = x
sortStudents n nps = mysort (sortStudents (hn) l) (sortStudents (n-hn) r)
    where
        hn = div n 2
        (l,r) = splitAt hn nps


mysort :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
mysort [] r = r
mysort l [] = l
mysort lnps@((ln,lp):lr) rnps@((rn,rp):rr) =
    if ln < rn then
        (ln,lp):(mysort lr rnps)
    else
        (rn,rp):(mysort lnps rr)

