module B
    ( main
    ) where

import qualified Data.Char   as DC
import qualified Data.IntMap as DM

main :: IO ()
main = do
    s <- getLine
    print $ isBeautifulW s


isBeautifulW :: String -> String
isBeautifulW cs = if (DM.null.leaveOdd.map culcNum $ cs) then "Yes" else "No"
    where
        culcNum :: Char -> Int
        culcNum c = (DC.ord c) - (DC.ord 'a')
        leaveOdd [n] = DM.singleton n ()
        leaveOdd (n:ns) = case DM.lookup n tgt of
            Nothing  -> DM.insert n () tgt
            (Just _) -> DM.delete n tgt
            where
                tgt = leaveOdd ns

