module Main where
-- B

main :: IO ()
main = do
    s <- getLine
    putStrLn . sayYes . checkEzStep .reverse $ s

sayYes True = "Yes"
sayYes _ = "No"

checkEzStep :: String -> Bool
checkEzStep = fst . foldr f (True,True)
    where
        f x (isEz,isOdd) = if isOdd
            then
                (isEz && isin "RUD" x, False)
            else
                (isEz && isin "LUD" x, True)
        isin w x = foldr (\y acc -> acc || x==y) False w

