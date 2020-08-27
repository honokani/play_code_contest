{-# Language MultiWayIf #-}

module C where

main :: IO ()
main = do
    m <- last.map read.words <$> getLine
    cs <- map read.words <$> getLine
    print $ flattenCandyNum m cs

flattenCandyNum m cs = foldr removeCandy (Nothing,0) cs
    where
        removeCandy x (Nothing, _) = if
            | (m < x)   -> ((Just m), x-m)
            | otherwise -> ((Just x), 0)
        removeCandy x (Just p,rem) = if
            | (m < p+x) -> ((Just  $m-p), rem+x+p-m)
            | otherwise -> ((Just x), rem)

