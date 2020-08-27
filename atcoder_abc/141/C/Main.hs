module Main where

import Data.Maybe (fromJust)
import Control.Monad

import Data.List
import qualified Data.ByteString.Char8 as BS

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM


bytestr2Int = fst . fromJust . BS.readInt
getInt = bytestr2Int <$> BS.getLine
getIntL_H = map bytestr2Int. BS.words <$> BS.getLine
getIntL_V n = replicateM n $ getInt
getInt2D n = replicateM n $ getIntL_H


main :: IO ()
main = do
    [n,k,q] <- getIntL_H
    qs <- getIntL_V q
    mapM_ putStrLn =<< checkLive <$> (reducePoint qs $ makeInitilPoints n k q)


checkLive :: VU.Vector Int -> [String]
checkLive vec = map sayYes . VU.toList $ VU.map ((<)0) vec

sayYes True = "Yes"
sayYes _ = "No"


makeInitilPoints :: Int -> Int -> Int -> VU.Vector Int
makeInitilPoints n k q = VU.replicate n (k-q)

reducePoint :: [Int] -> VU.Vector Int -> IO (VU.Vector Int)
reducePoint qs vec = do
    mvec <- VU.thaw vec
    forM_ qs $ \i -> do
        VUM.modify mvec (+1) (i-1)
    VU.freeze mvec

