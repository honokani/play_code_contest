{-# Language MultiWayIf #-}

module C where

main :: IO ()
main = do
    l <- getLine
    print $ countHands l

countHands :: String -> Int
countHands l = (flip (-) 1).length $ shrinkStones l
    where
        shrinkStones [x] = [x]
        shrinkStones (x:y:zs) = if
            | x == y    -> shrinkStones (y:zs)
            | otherwise -> (x:) $ shrinkStones (y:zs)

