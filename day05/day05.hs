import Data.Char (toLower)

react = foldr step ""
  where
    step x (y:ys) | x /= y && toLower x == toLower y = ys
    step x ys = x : ys

fully f i
    | a == i = i
    | otherwise = fully f a
    where a = f i

main = do
    polymer <- getContents
    print $ length $ fully react polymer
    print $ minimum $ map (length . fully react) [filter (\c -> (toLower c) /= i) polymer | i<-['a'..'z']]