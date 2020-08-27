{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as BS
bytestr2Int = fst . fromJust . BS.readInt
getInt = bytestr2Int <$> BS.getLine
getIntL_H = map bytestr2Int. BS.words <$> BS.getLine
getIntL_V n = replicate n $ getInt
getInt2D n = replicate n $ getIntL_H


data Curr = Zero
          | One Int
          | Filled Int Int Int
          deriving (Show)

main :: IO ()
main = do
    [k,s] <- getIntL_H
    putStrLn . show $ check k s 0 Zero

check k s a c =
    if s<0 then
        0
    else if k<a || a<0 then
        0
    else case c of
        Zero    -> check k s (a+1) c + check k (s-a) 0 (One a)
        (One x) -> check k s (a+1) c + check k 0 (s-a) (Filled x a (s-a))
        _       -> 1

