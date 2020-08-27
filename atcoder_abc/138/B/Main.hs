module Main where

main :: IO ()
main = do
    _ <- getLine
    ns <- (map read) . words <$> getLine
    putStrLn . show . makeInv . sumInvs $ ns

makeInv :: Fractional a => a -> a
makeInv x = 1 / x

sumInvs = foldr f 0
    where
        f a b = makeInv a + b
