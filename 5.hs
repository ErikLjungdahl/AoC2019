import qualified Data.Text as Text --(map, take, drop, splitAt)
import Data.List
run f = do
    str <- readFile "input5.txt"
    return $ f str
{-
f1 str = f 12 2 str

f :: Int -> Int -> String -> Int
f noun verb str = let ints = toInts str in
    head $ readOp 0 $ (head ints):noun:verb:(drop 3 ints)


f2 str = head [100 * noun + verb | noun <- [0..99], verb <- [0..99], (f noun verb str) == 19690720]
-}
f5 str = readOp 0 (toInts str, [])

type Ptr = Int
type Op = Int -> Int -> Int

toInts :: String -> [Int]
toInts s = map (read . Text.unpack) $ Text.splitOn (Text.singleton ',') (Text.pack s)

readOp :: Ptr -> ([Int], [Int]) -> ([Int], [Int])
readOp p (is,output) =
    let eval :: [Int] -> Ptr -> Op -> Ptr -> Int
        eval list p1 op p2 | p1 >= 0 && p2 >= 0 = eval' list p1 op p2
                           | otherwise = 0
        eval' (0:0:_) p1 op p2 = (is !! p1) `op` (is !! p2)
        eval' (0:1:_) p1 op p2 = (is !! p1) `op` p2
        eval' (1:0:_) p1 op p2 = p1         `op` (is !! p2)
        eval' (1:1:_) p1 op p2 = p1         `op` p2
        putAt :: Int -> Ptr -> [Int]
        putAt i pr = (take pr is) ++ (i:(drop (pr+1) is))
        (o:os) = drop p is
        modOp i = let i' = i `mod` 10
            in i':modOp (i `div` 10)
        inp = 1 -- Input is always 1?
    in case (modOp o, os) of
        (1:modes ,p1:p2:pres:_) -> readOp (p+4) ((putAt (eval modes p1 (+) p2) pres),output)
        (2:modes ,p1:p2:pres:_) -> readOp (p+4) ((putAt (eval modes p1 (*) p2) pres),output)
        (3:modes ,ps:_ )        -> readOp (p+2) ((putAt inp ps),output)
        (4:modes ,pr:_)         -> readOp (p+2) (is, (is !!+ pr):output)

        (9:9:modes,_) -> (is, output)

(!!+) :: [a] -> Int -> a
[]     !!+ _ = error "Index to large"
(x:_)  !!+ 0 = x
(x:xs) !!+ n | n < 0     = error"Negative index"
             | otherwise = xs !! (n-1)
