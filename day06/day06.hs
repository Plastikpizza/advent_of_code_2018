import qualified Data.Map as Map
import Data.List (minimumBy, nub, maximumBy)
import Data.Ord (comparing)

type Point = (Int, Int)

data Spot = Tie | Win Point deriving (Show, Eq)

count l = length . filter (==l)
countAll lst = map (\x -> (x, count x lst)) (nub lst)

-- the manhattan distance between to points
manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2,y2) = (abs (x1-x2)) + (abs (y1-y2))

-- used for parsing the input format
readPair :: [String] -> Point
readPair (a:b:[]) = (read (init a), read b)

-- upper left and lower right corners of a square containing the polygon
extremes :: [Point] -> (Point, Point)
extremes points = ((fst farLeft, snd farTop), (fst farRigh, snd farBott))
    where
        farLeft = minimum points
        farRigh = maximum points
        farBott = head [(x,y) | (x,y)<-points, y == maximum (map snd points)]
        farTop  = head [(x,y) | (x,y)<-points, y == minimum (map snd points)]

evenMoreExtreme n ps = ((a-n, b-n),(c+n,d+n))
    where ((a,b),(c,d)) = extremes ps

nearest :: Point -> [Point] -> [Point]
nearest point points = filter (\p -> manhattan p point == minDist) points
    where
        minDist = minimum $ map (manhattan point) points

rateSpot :: Point -> [Point] -> Spot
rateSpot p ps = case n of
    (a:[]) -> Win a
    otherwise -> Tie
    where n = nearest p ps

totalDistance :: Point -> [Point] -> Int
totalDistance p = sum . map (manhattan p)

partOne ps = maximumBy (comparing snd) (countAll all)
    where
        all = [i | i <- map (flip rateSpot ps) spots, not $ elem i infinite]
        ((mix,miy),(mx, may)) = extremes ps
        spots = [(i,j) | i<-[mix..mx], j<-[miy..may]]
        outerRing n = [(i,miy-n) | i <- [mix-n..mx+n]] ++ [(i,may+n) | i <- [mix-n..mx+n]] ++ [(mix-n,i) | i <- [miy-n..may+n]] ++ [(mx+n,i) | i <- [mix..mx]]
        infinite = filter (isWin) (map (flip rateSpot ps) (outerRing 40))
        isWin (Win _) = True
        isWin _ = False
        countWins = length . filter isWin

partTwo :: [Point] -> Int
partTwo ps = sum [1 | let ((a,b),(c,d)) = evenMoreExtreme 500 ps, i <- [a..c], j <- [b..d],  totalDistance (i,j) ps < 10000]

main :: IO ()
main = do
    points <- fmap (map readPair . map words . lines) getContents
    print $ partTwo points