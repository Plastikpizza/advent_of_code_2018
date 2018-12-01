import Data.List (findIndices)
import qualified Data.Set as Set
import Debug.Trace (trace)

intRead :: String -> Int
intRead = read

data PartTwo = PartTwo {pos::Int, work::[Int], seen::Set.Set Int, deltas::[Int]} deriving (Show, Eq)

partTwo pt
    | work pt == [] = partTwo pt{work=deltas pt}
    | Set.member nextPos (seen pt) = nextPos
    | otherwise =  partTwo nt
    where
        nextPos = (pos pt)+(head $ work pt)
        nt = pt{pos=nextPos, work=(tail $ work pt), seen=(Set.insert nextPos (seen pt))}

main = do
    putStrLn "part one"
    deltas <- (fmap (map intRead . words . filter (/='+')) getContents)
    print $ sum deltas
    putStrLn "part two"
    let position = partTwo (PartTwo 0 [] Set.empty deltas)
    print position