import System.IO
import qualified Data.Map as Map
import Data.List (sortBy)

important_symbols = ['/', '\\', '+']
cars              = ['>', '<', '^', 'v']

class (Enum a, Bounded a, Eq a) => Circ a where
    next :: a -> a
    next a = if a == maxBound then minBound else succ a
    prev :: a -> a
    prev a = if a == minBound then maxBound else pred a

instance Circ Turn
instance Circ Dir 

data Dir  = LEFT | UP | RIGHT | DOWN deriving (Show, Eq, Bounded, Enum)
data Turn = LEFT_TURN | STRAIGHT | RIGHT_TURN deriving (Show, Eq, Bounded, Enum)
data Kart = Kart {pos::(Int, Int), dir::Dir, nxt::Turn} deriving (Show, Eq)

kart '>' p = Kart p RIGHT LEFT_TURN
kart '<' p = Kart p LEFT  LEFT_TURN
kart '^' p = Kart p UP  LEFT_TURN 
kart 'v' p = Kart p DOWN LEFT_TURN

bounds upper = [0..(upper-1)]

fromTuple f (a,b) = f a b

crash karts = or [fromTuple (==) a | a <- (pairs $ map pos karts)]

drive k = k{pos=newPos} 
    where 
        (x, y) = pos k
        d      = dir k
        newPos = case d of 
            LEFT -> (x-1, y)
            RIGHT -> (x+1, y)
            UP    -> (x  , y-1)
            DOWN  -> (x  , y+1)

intersection k = k{dir=newDir, nxt=turnedNxt}
    where
        turnedNxt = next (nxt k)
        newDir    = case (nxt k) of
            STRAIGHT      -> (dir k)
            LEFT_TURN     -> prev (dir k)
            RIGHT_TURN    -> next (dir k)

turn k@(Kart _ d _) '/' = case d of
    LEFT  -> k{dir=DOWN}
    UP    -> k{dir=RIGHT}
    RIGHT -> k{dir=UP}
    DOWN  -> k{dir=LEFT}
turn k@(Kart _ d _) '\\' = case d of
    LEFT  -> k{dir=UP}
    UP    -> k{dir=LEFT}
    RIGHT -> k{dir=DOWN}
    DOWN  -> k{dir=RIGHT}

move i k = drive step1
    where 
        step1 = case (Map.lookup (pos k) i) of
            Nothing  -> k
            Just '+' -> intersection k
            Just  a  -> turn k a

sortGT (a1, b1) (a2, b2)
  | b1 < b2 = GT
  | b1 > b2 = LT
  | b1 == b2 = compare a1 a2

moveAllOrCrash ms is [] = ms
moveAllOrCrash ms is (k:ks) = case ms of 
    Right a -> if crash (movedKart:a) || crash (movedKart:ks) then Left (movedKart, a, ks) else moveAllOrCrash (Right (movedKart:a)) is ks
    otherwise -> ms
    where 
        movedKart = move is k

pairs [] = []
pairs (l:ls) = ((map (\b -> (l,b)) ls) ++ (pairs ls))  

part1 ks is = case moveAllOrCrash (Right []) is (sortBy (\a b -> sortGT (pos a) (pos b)) ks) of 
        Left (p,_,_) -> print (pos p)
        Right nks -> part1 nks is
    
part2 [k] is = print k
part2 ks is = case moveAllOrCrash (Right []) is ks of
    Right nks -> part2 nks is -- fine
    Left  (crashedKart, alreadyMoved, toBeMoved) -> do
        part2 (helper [c | c<-alreadyMoved, (pos c) /= (pos crashedKart)] [c | c<-toBeMoved, (pos c) /= (pos crashedKart)]) is-- crash
    where
        helper :: [Kart] -> [Kart] -> [Kart]
        helper a [] = a
        helper a b = case moveAllOrCrash (Right a) is b of
            Left (i,f,g) -> helper [c | c<-f, (pos c) /= (pos i)] [c | c<-g, (pos c) /= (pos i)]
            Right i -> i
main = do
    handle <- openFile "input13.txt" ReadMode
    contents <- fmap (lines) (hGetContents handle)
    let karts = map (fromTuple kart) [(c,(x,y)) | y <- bounds (length contents), x <- bounds (length (contents!!y)), let c = (contents!!y)!!x, elem c cars]
    let important = Map.fromList     [((x,y),c) | y <- bounds (length contents), x <- bounds (length (contents!!y)), let c = (contents!!y)!!x, elem c important_symbols]
    part1 karts important
    part2 karts important