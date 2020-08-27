module D
    ( main
    ) where

import Data.List

mul_m x y = mod (x * y) 1000000007

main :: IO ()
main = do
    [h,w,fh,fw] <- getNumbers
    print $ culcWay h w fh fw

getNumbers :: (Num a, Read a) => IO [a]
getNumbers = map read.words <$> getLine

culcC x 1 = 1
culcC 1 y = 1
culcC x y = case y<=x of
    True  -> div (foldr (*) 1 [(x)..(x+y-2)]) (foldr (*) 1 [1..(y-1)])
    False -> culcC y x

culcWay h w fh fw = foldr ((+).snd) 0 $ take (aw) culcAll
    where
        ah = h - fh
        aw = w - fw
        culcAll = iterate ( \(i,x) -> ( i+1
                                      , div (x*(ah+aw+i)*(aw-i)) ((aw+i+1)*(fh+aw-1-i))
                                      )
                          ) (1,culcInitial)
        culcInitial = (culcC ah (fw+1)) * (culcC fh aw)

