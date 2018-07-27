module B
    ( main
    ) where

import Data.List
import Control.Monad

main :: IO ()
main = do
    s <- head <$> getWords
    putStrLn $ getDisplayStr s

getWords :: IO [String]
getWords = words <$> getLine

getDisplayStr :: String -> String
getDisplayStr = reverse.foldl (\acc x -> getDisplayStrCore x acc) []
    where
        getDisplayStrCore :: Char -> String -> String
        getDisplayStrCore x [] = case x=='B' of
            True  -> []
            False -> [x]
        getDisplayStrCore x cAll@(c:cs) = case x=='B' of
            True  -> cs
            False -> x:cAll

