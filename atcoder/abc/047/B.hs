module B
    ( main
    ) where
-- Fri Aug 17 16:38:49 JST 2018

import Data.List
import Control.Monad

main :: IO ()
main = do
    (i,ns) <- getIIL_and_InfosWithNums
    print $ culcArea i ns

culcArea (ix,iy) ns = culcArea $ foldl shrinkArea (0,0,ix,iy) $ fixNs ns
    where
        fixNs [] = ([],[])
        fixNs (y:ys) = (ps:pss,dir:dirs)
            where
                (ps,dir) = splitBotAndMapT3 id.listToT3.take 3 $ y
                (pss,dirs) = fixNs ys
        shrinkArea (sx,sy,ex,ey) [((nx,ny),dir)]
            | dir == 1 = (nx,sy,ex,ey)
            | dir == 2 = (sx,sy,nx,ey)
            | dir == 3 = (sx,ny,ex,ey)
            | dir == 4 = (sx,sy,ex,ny)
        culcArea (sx,sy,ex,ey) = (ex-sx) * (ey-sy)


            





getIIL_and_InfosWithNums :: (Num a, Read a, Enum a) => IO (((Int,Int),[[a]]))
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

