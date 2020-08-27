{-# Language MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module D where

import           Control.Monad
import           Data.Array
import           Data.Array.IO

type UArrBools2D = IOUArray (Int, Int) Bool

main :: IO ()
main = print =<< countConnectedBoth =<< culcConnection =<< getInput


getInput :: IO (Int, [[Int]], [[Int]])
getInput = do
    [n,k,l] <- getInt_N 3
    ks <- getInt_LN k 2
    ls <- getInt_LN l 2
    return (n, ks, ls)
    where
        getInt_LN :: Int -> Int -> IO [[Int]]
        getInt_LN l n = replicateM l $ getInt_N n
        getInt_N :: Int -> IO [Int]
        getInt_N n = take n.map read.words <$> getLine


culcConnection :: (Int, [[Int]], [[Int]]) -> IO (Int, IOUArray Int Int, IOUArray Int Int)
culcConnection (n,ks,ls) = do
    arK <- connect ks =<< initializeMap
    arL <- connect ls =<< initializeMap
    return (n, arK, arL)
    where
        initializeMap :: IO (IOUArray Int Int)
        initializeMap = thaw $ listArray (1,n) [1..n]
        connect :: [[Int]] -> (IOUArray Int Int) -> IO (IOUArray Int Int)
        connect (([x,y]):[]) ar = connectCore ar x y
        connect (([x,y]):cs) ar = connect cs =<< connectCore ar x y
        connectCore :: (IOUArray Int Int) -> Int -> Int -> IO (IOUArray Int Int)
        connectCore ar x y = do
            vx <- readArray ar x
            vy <- readArray ar y
            if vx < vy  then writeArray ar y vx
                        else writeArray ar x vy
            return ar


countConnectedBoth :: (Int, IOUArray Int Int, IOUArray Int Int) -> IO [Int]
countConnectedBoth (n, arK, arL) = do
    resultMap <- thaw $ listArray (1,n) $ repeat 1 :: IO (IOUArray Int Int)
    forM_ [1..n] $ \x -> do
        vk_x <- readArray arK x
        vl_x <- readArray arL x
        rx <- forM [(x+1)..n] $ \y -> do
            vk_y <- readArray arK y
            vl_y <- readArray arL y
            if (vk_x == vk_y) && (vl_x == vl_y) then do
                vr_y <- readArray resultMap y
                writeArray resultMap y (vr_y + 1)
                return 1
            else return 0
        vr_x <- readArray resultMap x
        writeArray resultMap x $ vr_x + sum rx
    getElems resultMap

