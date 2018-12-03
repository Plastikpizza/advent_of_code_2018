import Data.Char (isDigit)
import qualified Data.Map as Map

data Rect = Rect {id::Int, x::Int, y::Int, width::Int, height::Int} deriving (Eq, Show, Ord)

fieldsOfRect r = [((x r)+i,(y r)+j) | i<-[1..(width r)], j<-[1..(height r)] ]

rectFromLine line = Rect id x y w h
    where
        nums :: [Int]
        nums = map read $ words $ map (\x -> if isDigit x then x else ' ') line
        [id, x, y, w, h] = nums

insertPoint :: Map.Map (Int, Int) Int -> (Int, Int) -> Map.Map (Int, Int) Int
insertPoint m p = Map.insertWithKey (\k n o -> o + n) p 1 m
insertRect r m = foldl insertPoint m (fieldsOfRect r)

partOne rects = helper rects (Map.empty)
    where
        -- helper :: [Rect] -> Map.Map (Int, Int) Int -> Int
        helper [] m = (Map.size $ Map.filter (>=2) m, m)
        helper (r:rs) m = helper rs (insertRect r m)

intacat :: Map.Map (Int, Int) Int -> Rect -> Bool
intacat m = and . (map (\x -> (Map.!) m x == 1)) . fieldsOfRect

partTwo rects m = filter (intacat m) rects

main = do
    rects <- fmap (map rectFromLine . lines) getContents
    let (a, b) = partOne rects
    print a
    print $ partTwo rects b
