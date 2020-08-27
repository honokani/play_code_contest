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
    [_,k] <- getIntL_H
    hs <- getIntL_H
    print $ check k hs

check k = length . filter (k<=)

