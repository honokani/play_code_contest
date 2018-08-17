module C where
    --( main
    --) where

import Data.List
import Control.Monad
import Control.Arrow ((***))


main :: IO ()
main = do
    nss <- getL_and_Nums
    print $ countTotalVotes nss

----
countTotalVotes :: (Integral a, Read a) => [[a]] -> a
countTotalVotes = addT2.checkVoteNum.pairsToT2s
    where
        addT2 (f,s) = f+s

pairsToT2s :: [[a]] -> [(a,a)]
pairsToT2s ls = unfoldr p2t ls
    where
        pairToT2 :: [a] -> (a,a)
        pairToT2 (f:s:_) = (f,s)
        p2t [] = Nothing
        p2t (x:xs) = Just (pairToT2 x, xs)

data CompSize a = BothBig
                | ExistSmall a

checkVoteNum :: (Integral a, Read a) => [(a, a)] -> (a, a)
checkVoteNum xs = checkVoteNumCore xs (1,1)
    where
        checkVoteNumCore [] pre = pre
        checkVoteNumCore (x:xs) pre = checkVoteNumCore xs $ estimateMaxMin pre x
        estimateMaxMin pOld@(ol,or) pNew@(nl,nr) = case checkChange pOld pNew of
            BothBig -> pNew
            ExistSmall n -> join (***) (*n) pNew

checkChange :: (Integral a, Read a, Enum a, Ord a) => (a,a) -> (a,a) -> CompSize a
checkChange pOld@(ol,or) pNew@(nl,nr) =
    if ol < nl && or < nr
    then BothBig
    else
        let rl = realToFrac ol / realToFrac nl
            rr = realToFrac or / realToFrac nr
        in  if rl < 1
            then ExistSmall $ ceiling rr
            else
                if rr < 1
                then ExistSmall $ ceiling rl
                else ExistSmall $ if rl < rr then culcRatio or nr else culcRatio ol nl
    where
        culcRatio o n = div (lcm o n) n

----
getL_and_Nums :: (Num a, Read a, Enum a) => IO [[a]]
getL_and_Nums = do
    l <- read.head.words <$> getLine
    replicateM l getNumbers
    where
        getNumbers :: (Num a, Read a) => IO [a]
        getNumbers = map read.words <$> getLine

