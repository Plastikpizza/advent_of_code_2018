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

drive kart = kart{pos=newPos} 
    where 
        (x, y) = pos kart
        d      = dir kart
        newPos = case d of 
            LEFT -> (x-1, y)
            RIGHT -> (x+1, y)
            UP    -> (x  , y-1)
            DOWN  -> (x  , y+1)

intersection kart = kart{dir=newDir, nxt=turnedNxt}
    where
        turnedNxt = next (nxt kart)
        newDir    = case (nxt kart) of
            STRAIGHT      -> (dir kart)
            LEFT_TURN     -> prev (dir kart)
            RIGHT_TURN    -> next (dir kart)

turn kart@(Kart _ d _) '/' = case d of
    LEFT  -> kart{dir=DOWN}
    UP    -> kart{dir=RIGHT}
    RIGHT -> kart{dir=UP}
    DOWN  -> kart{dir=LEFT}

turn kart@(Kart _ d _) '\\' = case d of
    LEFT  -> kart{dir=UP}
    UP    -> kart{dir=LEFT}
    RIGHT -> kart{dir=DOWN}
    DOWN  -> kart{dir=RIGHT}

move i kart = drive step1
    where 
        step1 = case (Map.lookup (pos kart) i) of
            Nothing  -> kart
            Just '+' -> intersection kart
            Just  a  -> turn kart a

sortGT (a1, b1) (a2, b2)
  | b1 > b2 = GT
  | b1 < b2 = LT
  | b1 == b2 = compare a1 a2

moveAllOrCrash movedKarts tracks [] = movedKarts
moveAllOrCrash movedKarts tracks (kart:karts) = case movedKarts of 
    Right a ->  if crash (movedKart:a) || crash (movedKart:karts) 
                then Left (movedKart, a, karts) 
                else moveAllOrCrash (Right (movedKart:a)) tracks karts
    otherwise -> movedKarts
    where 
        movedKart = move tracks kart

pairs [] = []
pairs (l:ls) = ((map (\b -> (l,b)) ls) ++ (pairs ls))  

part1 karts tracks = case moveAllOrCrash (Right []) tracks (sortBy (\a b -> sortGT (pos a) (pos b)) karts) of 
        Left (p,_,_) -> print (pos p)
        Right nks -> part1 nks tracks
    
part2 [] tracks = putStrLn "all karts crashed."
part2 [kart] tracks = print kart
part2 karts tracks = 
    case moveAllOrCrash (Right []) tracks (sortBy (\a b -> sortGT (pos a) (pos b)) karts) of
    Right nks -> part2 nks tracks -- fine
    Left  (crashedKart, alreadyMoved, toBeMoved) -> do
        putStrLn ("crash at " ++ (show $ pos $ crashedKart))
        part2 
            (helper [c | c<-alreadyMoved, (pos c) /= (pos crashedKart)] [c | c<-toBeMoved, (pos c) 
            /= (pos crashedKart)]) tracks-- crash
    where
        helper :: [Kart] -> [Kart] -> [Kart]
        helper a [] = a
        helper a b = case moveAllOrCrash (Right a) tracks b of
            Left (i,f,g) -> helper [c | c<-f, (pos c) /= (pos i)] [c | c<-g, (pos c) /= (pos i)]
            Right i -> i
main = do
    handle <- openFile "input13.txt" ReadMode
    contents <- fmap (lines) (hGetContents handle)
    let karts = map (fromTuple kart) [(c,(x,y)) | y <- bounds (length contents), x <- bounds (length (contents!!y)), let c = (contents!!y)!!x, elem c cars]
    let important = Map.fromList     [((x,y),c) | y <- bounds (length contents), x <- bounds (length (contents!!y)), let c = (contents!!y)!!x, elem c important_symbols]
    part1 karts important
    part2 karts important