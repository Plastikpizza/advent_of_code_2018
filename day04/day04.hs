import qualified Data.Map as Map

import Data.List (nub, sortBy, sort)

data Sleepkeep = Sleepkeep {guard::Int, start::Int, end::Int} deriving (Eq, Show, Ord)

data Line = Guard Int | Sleep Int | Wake Int deriving (Show, Eq)

extractMinute str = read $ tail $ dropWhile (/=':') $ takeWhile (/=']') str

parseLine l
    | elem "Guard" (words l) = Guard (read (tail ((words l)!!3)))
    | elem "falls" (words l) = Sleep (extractMinute ((words l)!!1))
    | elem "wakes" (words l) = Wake (extractMinute ((words l)!!1))

sleeps [] _ = []
sleeps ((Guard n):ls) sl = sleeps ls sl{guard=n}
sleeps ((Sleep n):ls) sl = sleeps ls sl{start=n}
sleeps ((Wake n):ls)  sl = (sl{end=n}:sleeps ls sl)

sleepLength (Sleepkeep _ a b) = b - a

sleepMinutes (Sleepkeep _ a b) = [a..b]

sleepsAt n (Sleepkeep _ a b) = a <= n && n < b

minutes = Map.fromList [(i, 0)|i<-[0..59]]

allSleepsOfGuard g sl = filter (\a -> guard a == g) sl

absoluteTimesAGuardSleptAtCertainTime t g sl = length $ filter (==True) $ map (sleepsAt t) $ allSleepsOfGuard g sl

main = do
    rawTimes <- getContents
    let intermediate = map parseLine (lines rawTimes)
    let sl = sleeps intermediate (Sleepkeep 0 0 0)
    let (id,_) = head $ sortBy (\(_, i) (_, j) -> compare j i) [(i, sum [sleepLength s | s<-sl, guard s == i])| i <- (nub (map guard sl))]
    let mins = concat $ map sleepMinutes $ filter (\x-> guard x == id) sl
    print $ fst $ head $ sortBy (\(_, i) (_, j) -> compare j i) $ Map.toList $ foldr (\x -> Map.adjust (+1) x) minutes mins
    print id
    
    let distinctGuards = nub $ map guard sl
    let (a,b,c) = last $ sort $ [(absoluteTimesAGuardSleptAtCertainTime t g sl, g, t) | t<-[0..59], g<-distinctGuards]
    print (b*c)
    