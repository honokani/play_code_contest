module B
    ( main
    ) where

import Data.List
import Control.Monad

main :: IO ()
main = do
    (i,ns) <- getIIL_and_InfosWithNums
    print $ culcArea i ns

culcArea (ix,iy) ns = culcAreaC $ foldl shrinkArea (0,ix,0,iy) $ fixNs ns
    where
        fixNs []     = []
        fixNs (y:ys) = (splitBotAndMapT3 id.listToT3.take 3 $ y) : fixNs ys
        shrinkArea (sx,ex,sy,ey) ((_nx,_ny),dir)
            | dir == 1 = (_nx,ex,sy,ey)
            | dir == 2 = (sx,_nx,sy,ey)
            | dir == 3 = (sx,ex,_ny,ey)
            | dir == 4 = (sx,ex,sy,_ny)
        culcAreaC (sx,ex,sy,ey) = if (0 < x && 0 < y) then x*y else 0
            where
                x = (ex-sx)
                y = (ey-sy)


getIIL_and_InfosWithNums :: IO ((Int,Int),[[Int]])
getIIL_and_InfosWithNums = do
    (i,l) <- splitBotAndMapT3 read.listToT3.take 3.words <$> getLine
    ns <- replicateM l getNumbers
    return (i,ns)
    where
        getNumbers :: (Num a, Read a) => IO [a]
        getNumbers = map read.words <$> getLine

listToT3 :: [a] -> (a,a,a)
listToT3 (x:y:z:_) = (x,y,z)
splitBotAndMapT3 :: (a -> b) -> (a,a,a) -> ((b,b),b)
splitBotAndMapT3 f (t,m,b) = ((f t,f m), (f b))

