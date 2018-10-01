{-# Language MultiWayIf #-}

module C where

main :: IO ()
main = do
    n <- read <$> getLine
    ns <- (map read).words <$> getLine
    print $ countToMakeZ n ns


countToMakeZ :: Int -> [Int] -> Int
countToMakeZ n = uncurry countDivide.mapT pickMajor2.split
    where
        mapT f (x,y) = (f x,f y)
        split xs = foldr switch ([],[]) ys
            where
                ys = zip xs $ cycle [True,False]
                switch (x,b) (f,s) = if
                    | b         -> (x:f,s)
                    | otherwise -> (f,x:s)
        pickMajor2 = takeTill2.foldr countSort []
            where
                countSort x acc = replace $ countup x acc
                countup :: Int -> [(Int,Int)] -> [(Int,Int)]
                countup x [] = [(x,1)]
                countup x ((y,j):ys) = if
                    | x == y    -> (y,j+1):ys
                    | otherwise -> (y,j):countup x ys
                replace :: [(Int,Int)] -> [(Int,Int)]
                replace [xi] = [xi]
                replace ((x,i):(y,j):zs) = if
                    | i < j     -> (y,j) : (replace ((x,i):zs))
                    | otherwise -> (x,i) : (replace ((y,j):zs))
                takeTill2 [] = []
                takeTill2 [x] = [x]
                takeTill2 (x:y:zs) = x:[y]
        countDivide [(fF1,fN1)] [(sF1,sN1)] = if
            | fF1 == sF1 -> n - sN1
            | otherwise  -> n - (fN1+sN1)
        countDivide [(fF1,fN1)] ((sF1,sN1):[(sF2,sN2)]) = if
            | fF1 == sF1 -> n - (sN2+fN1)
            | otherwise  -> n - (fN1+sN1)
        countDivide ((fF1,fN1):[(fF2,fN2)]) [(sF1,sN1)] = if
            | fF1 == sF1 -> n - (fN2+sN1)
            | otherwise  -> n - (fN1+sN1)
        countDivide ((fF1,fN1):[(fF2,fN2)]) ((sF1,sN1):[(sF2,sN2)]) = if
            | fF1 == sF1 -> if
                | fN1 < sN1 -> n - (fN2+sN1)
                | otherwise -> n - (sN2+fN1)
            | otherwise  -> n - (fN1+sN1)

