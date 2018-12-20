type Metadata = Int
data Tree = Node{children::[Tree],meta::[Metadata]} deriving (Show, Eq)

addChild :: Tree -> Tree -> Tree
addChild t1 t2 = t1{children=t2:(children t1)}

readTrees :: Int -> [Int] -> ([Tree], [Int])
readTrees n tokens
    | n == 1    = addIn newTree ([], newTokens)
    | otherwise = addIn newTree (readTrees (n-1) newTokens)
    where
        (newTree, newTokens) = readTree tokens
        addIn t (ts, tks) = (t:ts, tks)

readTree :: [Int] -> (Tree, [Int])
readTree (c:m:tokens)
    | c == 0 = (Node [] (take m tokens), (drop m tokens))
    | otherwise = (Node kids info, rest)
    where
        (kids, infoAndrest) = readTrees c tokens
        info = take m infoAndrest
        rest = drop m infoAndrest

partOne (Node a b) = (sum (map partOne a)) + sum b

partTwo (Node a b)
    | a == []   = sum b
    | otherwise = sum [partTwo (a!!(i-1)) | i <- b, i <= length a]

main = do
    tokens <- fmap (map read . words) getContents
    let (tree, rst) = readTree tokens
    print $ partOne tree
    print $ partTwo tree