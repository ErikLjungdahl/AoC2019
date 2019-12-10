--import Data.Tree
import qualified Data.HashMap.Strict as M
run f = do
    str <- readFile "input6.txt"
    let pairs = map (\line -> ((takeWhile (\c -> c /= ')') line), tail (dropWhile (\c -> c /= ')') line ))) (lines str)
    return $ f pairs


f6_1 :: [(String,String)] -> Int
f6_1 list =
    let mp = M.fromListWith (++) [(k , [v]) | (k, v) <- list]
        com = "COM"
        acc :: Int -> String -> Int
        acc n key = case M.lookup key mp of
            Just vs -> n + sum (map (acc (n+1)) vs)
            Nothing -> n
    in acc 0 com
