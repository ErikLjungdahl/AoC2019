import Data.Tree

run f = do
    str <- readFile "input6.txt"
    let pairs = map (\line -> ((takeWhile (\c -> c /= ')') line), tail (dropWhile (\c -> c /= ')') line ))) (lines str)
    return $ f pairs

f6_1 pairs = pairs

f1 :: [(String,String)] -> Tree String
f1 ((a,b):ss) = Node a [Node b []] 
