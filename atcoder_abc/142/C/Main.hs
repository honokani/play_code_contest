module Main where

import Data.Maybe (fromJust)
import Data.List
import Control.Monad
import qualified Data.ByteString.Char8 as BS

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

bytestr2Int = fst . fromJust . BS.readInt
getInt = bytestr2Int <$> BS.getLine
getIntL_H = map bytestr2Int. BS.words <$> BS.getLine
getIntL_V n = replicate n $ getInt
getInt2D n = replicate n $ getIntL_H

main :: IO ()
main = do
    [x,y] <- getIntL_H
    print . findPGrp $ VU.fromList . factors $ gcd x y

findPGrp vec = findPGrpN vec 0 0 0 $ VU.length vec - 1

findPGrpN vec curr s e lim
    | 0 == length (findPGrpCore vec s e) =
        if curr < e-s then
                        findPGrpNxtCore vec (e-s) s e lim
                    else
                        findPGrpNxtCore vec curr s e lim
    | otherwise = findPGrpNxtCore vec s e curr lim

findPGrpNxtCore curr s e lim =
    if lim == e then
                    findPGrpN curr (s+1) (s+1) lim
                else
                    findPGrpN curr s (e+1) lim

--findPGrpCore :: VU.Vector Int -> Int -> [()]
findPGrpCore vec s e = do
    tgt <- [s..e]
    guard ( 0 /= mod (vec VU.! n) (vec VU.! tgt) )
    pure ()

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

