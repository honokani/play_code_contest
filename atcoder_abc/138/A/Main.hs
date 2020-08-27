module Main where

thresh :: Integer 
thresh= 3200

main :: IO ()
main = do
    n <- read <$> getLine
    s <- getLine
    putStrLn $ echoStr n s

echoStr :: Integer -> String -> String
echoStr n s = if n < thresh then "red" else s

