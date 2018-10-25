{-# Language MultiWayIf #-}

module C where

main :: IO ()
main = do
    s <- getLine
    print =<< runGame s True

data Winner = First
            | Second
            deriving Show

runGame :: String -> Bool -> IO Winner
runGame s b = if
    | isGameOver s -> return $ checkWiner b
    | otherwise -> do
        nextS <- pickChar s
        runGame (head nextS) (not b)
    where
        checkWiner b = if b then Second else First

pickChar :: String -> IO [String]
pickChar s = return $ concat.map charangePick $ [1..(sLen - 2)]
    where
        sLen = length s
        charangePick n = if col == cor then [] else [rest]
            where
                rest = top ++ bot
                col = s !! (n-1)
                cor = s !! (n+1)
                top = take n s
                bot = drop (n+1) s

isGameOver :: String -> Bool
isGameOver = uncurry (&&).mapT isAllSameElement.snd.foldr splitToggle (True,([],[]))
    where
        splitToggle :: a -> (Bool,([a],[a])) -> (Bool,([a],[a]))
        splitToggle x (b,(o,e)) = if
            | b -> (not b, (x:o, e))
            | otherwise -> (not b, (o, x:e))
        isAllSameElement :: (Eq a) => [a] -> Bool
        isAllSameElement [x] = True
        isAllSameElement (x:y:zs) = (x == y) && isAllSameElement (y:zs)
        mapT :: (a -> b) -> (a,a) -> (b,b)
        mapT f (fst,snd) = (f fst, f snd)

