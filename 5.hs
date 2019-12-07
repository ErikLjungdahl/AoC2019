import qualified Data.Text as Text --(map, take, drop, splitAt)
import Data.List
run f = do
    str <- readFile "input2.txt"
    return (f str)

f1 str = f 12 2 str

f :: Int -> Int -> String -> Int
f noun verb str = let ints = toInts str in
    head $ readOp 0 $ (head ints):noun:verb:(drop 3 ints)


f2 str = head [100 * noun + verb | noun <- [0..99], verb <- [0..99], (f noun verb str) == 19690720]

type Ptr = Int
type Op = Int -> Int -> Int

toInts :: String -> [Int]
toInts s = map (read . Text.unpack) $ Text.splitOn (Text.singleton ',') (Text.pack s)

readOp :: Ptr -> [Int] -> [Int]
readOp p is =
    let eval :: Ptr -> Op -> Ptr -> Int
        eval p1 op p2 = (is !! p1) `op` (is !! p2)
        putAt :: Int -> Ptr -> [Int]
        putAt i pr = (take pr is) ++ (i:(drop (pr+1) is))
    in case splitAt p is of
        (l,1:p1:p2:rest:_) -> readOp (p+4) (putAt (eval p1 (+) p2) rest)
        (l,2:p1:p2:rest:_) -> readOp (p+4) (putAt (eval p1 (*) p2) rest)
        (l,3:ps:rest)      -> readOp (p+2) (putAt inp rest)
        (l,4:pr:rest)      -> readOp (p+2) (putAt inp rest)
        (l,99:_) -> is
