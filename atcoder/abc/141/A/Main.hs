module Main where
-- A

main :: IO ()
main = do
    n <-getLine
    putStrLn $ if n == "Sunny"
            then
                "Cloudy"
            else if n == "Cloudy"
                then
                    "Rainy"
                else
                    "Sunny"

