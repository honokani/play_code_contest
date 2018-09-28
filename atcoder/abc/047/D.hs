{-# Language MultiWayIf #-}

module D
    ( main
    ) where


main :: IO ()
main = do
    _ <- getLine
    costs <- (map read).words <$> getLine
    print $ getEarnTimings costs

getEarnTimings :: [Int] -> Int
getEarnTimings cs = bot $ foldr findCity init cs
    where
        bot (_,_,b) = b
        init = (0,0,0)
        findCity x (0,0,0)      = (x,0,0)
        findCity x (max,earn,n) = if
            | max < x   -> (x,earn,n)
            | otherwise -> if
                | max - x == earn -> (max,earn,n+1)
                | max - x <  earn -> (max,earn,n)
                | otherwise       -> (max,max-x,1)

