module B
    ( main
    ) where

import           Control.Monad       (forM,liftM)
import qualified Data.Char     as DC

main :: IO ()
main = do
    ls <- forM [0..2] (\_ -> getLine)
    putStrLn $ runGame ls


runGame :: [String] -> String
runGame ls = checkWinner $ runGameCore 0 $ map (str2Ints 'a') ls
    where
        checkWinner :: [[Int]] -> String
        checkWinner n = head $ map (ints2Str 'A') n
        runGameCore :: Int -> [[Int]] -> [[Int]]
        runGameCore i n = checkFinished $ n !! i
            where
                checkFinished :: [Int] -> [[Int]]
                checkFinished [] = [[i]]
                checkFinished (x:xs) = if i==0
                    then runGameCore x $ xs : tail n
                    else runGameCore x $ take i n ++ (xs : drop (i-1) n)

str2Ints :: Char -> String -> [Int]
str2Ints c = map ( flip (-) (DC.ord c).DC.ord )
ints2Str :: Char -> [Int] -> String
ints2Str c = map ( DC.chr.(+ DC.ord c) )

