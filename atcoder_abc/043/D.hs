module D
    ( main
    ) where

import Data.List

mul_m x y = mod (x * y) 1000000007

main :: IO ()
main = do
    h <- head <$> getWords
    echoUnbarancePoint $ checkBarance 1 h

getWords :: IO [String]
getWords = words <$> getLine

echoUnbarancePoint :: (Integer, Integer) -> IO ()
echoUnbarancePoint (f, s) = print $ show f ++ " " ++ show s

checkBarance :: (Eq a) => Integer -> [a] -> (Integer,Integer)
checkBarance _ [] = (-1, -1)
checkBarance _ [x] = (-1, -1)
checkBarance i (x:y:[]) = if x==y
    then (i, i+1)
    else (-1, -1)
checkBarance i (x:xs@(y:z:s)) = if x==y
    then (i, i+1)
    else if x==z
        then (i, i+2)
        else checkBarance (i+1) $ xs


