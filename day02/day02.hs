import Data.List (nub)
import Dist (dist)

count l = length . filter (==l)
countAll lst = map (\x -> (x, count x lst)) (nub lst)

exactly n = (1 <=) . length . filter (\(a, b)-> b == n) 

partOne ids = (twos, threes)
    where
        cids = map countAll ids
        twos = length $ filter (exactly 2) cids
        threes = length $ filter (exactly 3) cids

partTwo ids = head [(x, y) | x<-ids, y<-ids, dist x y == 1]

main = do
    ids <- fmap words getContents
    print $ partOne ids
    print $ partTwo ids