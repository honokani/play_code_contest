{-# LANGUAGE BangPatterns #-}
module C
    ( main
    ) where

import           Control.Monad            (forM_)
import qualified Data.IntMap        as DM
import qualified Data.Array.IO      as AI
import qualified Data.Array.Unboxed as AU

main :: IO ()
main = do
    (ns,i) <- getNI_and_NumsWithInfo
    print =<< selectCardArr (read i) ns

getNI_and_NumsWithInfo:: (Num a, Read a, Enum a) => IO ([a], String)
getNI_and_NumsWithInfo = do
    (n, i) <- mapT2fst read.listToT2.take 2.words <$> getLine
    xs <- take n <$> getNumbers
    return (xs, i)
    where
        listToT2 :: [a] -> (a,a)
        listToT2 (x:y:_) = (x,y)
        mapT2fst :: (a -> b) -> (a,a) -> (b,a)
        mapT2fst f (a1,a2) = (f a1, a2)
        getNumbers :: (Num a, Read a) => IO [a]
        getNumbers = map read.words <$> getLine


type Range = (Int, Int)
type UArr2D = AI.IOUArray Range Int

selectCardArr avg ls
    | checkRange avg = return [0]
    | otherwise = do
        n <- countCard
        print $ n - 1
        return lsFlatten
    where
        checkRange x = x < mini || maxi < x
        n = length ls
        mini = minimum ls
        maxi = maximum ls
        normalize x = x - avg
        lsFlatten = map normalize ls
        negaMax = (foldr (+) 0).filter (0>) $ lsFlatten
        posiMax = (foldr (+) 0).filter (0<) $ lsFlatten
        countCard = do
            memo <- AI.newArray ( (0,negaMax),(n,posiMax) ) 0 :: IO UArr2D
            initializeMemo memo
            forM_ (zip [1..] lsFlatten) $ updateMemo memo
            AI.readArray memo (n,0)
            where
                initializeMemo mArr = AI.writeArray mArr (0,0) 1
                updateMemo mArr (i,elem) = do
                    forM_ [negaMax .. posiMax] (\x -> do
                            old <- AI.readArray mArr (i-1,x)
                            b <- return $ (x-elem) < negaMax || posiMax < (x-elem)
                            if not b then do
                                up <- AI.readArray mArr (i-1,x-elem)
                                AI.writeArray mArr (i,x) $ old + up
                            else do
                                AI.writeArray mArr (i,x) old
                        )




-- TLE
selectCard avg ls = if (avg < mini || maxi < avg)
    then
        0
    else
        snd.comb $ lsFlatten
    where
        mini = minimum ls
        maxi = maximum ls
        normalize x = x - mini
        avgNorm = normalize avg
        lsNorm = map normalize ls
        lsFlatten = map (flip (-) avgNorm) lsNorm
        comb [] = ([0],0)
        comb allx@(x:xs) = (combCore++memo, i+newZero)
            where
                (!memo, !i) = comb xs
                !newZero = length.filter (0==) $ combCore
                combCore = do
                    b <- memo
                    return $ x+b

