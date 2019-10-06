{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad

import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as BS

getStr = BS.getLine
getStrL_H = map BS.unpack . BS.words <$> getStr
getStrL_V n = replicate n $ getStr

bytestr2Int = fst . fromJust . BS.readInt
getInt = bytestr2Int <$> BS.getLine
getIntL_H = map bytestr2Int . BS.words <$> BS.getLine
getIntL_V n = replicate n $ getInt
getInt2D n = replicate n $ getIntL_H

main :: IO ()
main = do
    [sx,sy,tx,ty] <- getIntL_H
    putStrLn $ getLoad sx sy tx ty

getLoad sx sy tx ty = join [l1,l2]
    where
        h = ty-sy
        w = tx-sx
        l1gh = take h $ repeat 'U'
        l1gw = take w $ repeat 'R'
        l1bh = take h $ repeat 'D'
        l1bw = take w $ repeat 'L'
        l1 = join [l1gh,l1gw,l1bh,l1bw]
        l2gh = 'L' : l1gh
        l2gw = 'U' : l1gw
        l2bh = 'R' : l1bh
        l2bw = 'D' : l1bw
        l2 = join [l2gh,l2gw,"RD",l2bh,l2bw,"LU"]
