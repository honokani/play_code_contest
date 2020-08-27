{-# Language MultiWayIf #-}

module C where

import Data.List     (isPrefixOf)
import Data.Maybe    (isNothing)
import Control.Arrow ((&&&))

main :: IO ()
main = do
    l <- getLine
    putStrLn $ isDayDream l

isDayDream :: String -> String
isDayDream word = echoResult.appendUntilEOS dropIfExistKwd $ reverse word
    where
        kwds = ["dream", "dreamer", "erase", "eraser"]
        revKVs = uncurry zip.(map reverse &&& map length) $ kwds
        dropIfExistKwd s = foldr (scanUntilDrop s) Nothing revKVs
        scanUntilDrop s x acc = if isNothing acc then dropKwd x s else acc
        dropKwd (k,v) s = if
            | isPrefixOf k s -> Just $ drop v s
            | otherwise -> Nothing
        appendUntilEOS f x = case f x of
            (Just "") -> True
            (Just s) -> appendUntilEOS f s
            Nothing -> False
        echoResult True = "Yes"
        echoResult False = "No"

