module Main where

main :: IO ()
main = do
    _ <- getLine
    ns <- (map read) . words <$> getLine
    print . mixObjs . msort $ ns


mixObjs = mixObjsCore Nothing
    where
        mixObjsCore :: Maybe Float -> [Float] -> Float
        mixObjsCore keep (l1:[l2]) = case keep of
            Nothing -> mix l1 l2
            (Just x) -> if l2 < x
                then
                    mixObjsCore Nothing $ (mix l1 l2) : [x]
                else
                    mixObjsCore Nothing $ (mix l1 x) : [l2]
        mixObjsCore keep (l1:l2:ls) = case keep of
            Nothing -> mixObjsCore keep $ (mix l1 l2) : ls
            (Just x) -> if l2 < x
                then
                    mixObjsCore (Just x) $ (mix l1 l2) : ls
                else
                    mixObjsCore (Just l2) $ (mix l1 x) : ls
        mix x y = (x+y)/2


msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
    where
        xlen_h = div (length xs) 2
        (ys, zs) = splitAt xlen_h xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] =  xs
merge [] ys =  ys
merge (x:xs) (y:ys)
    | x < y     = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

