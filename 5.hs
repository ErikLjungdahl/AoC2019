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
f5   str = head $ snd $ readOpWithInput 1 0 (toInts str, [])
f5_2 str = head $ snd $ readOpWithInput 5 0 (toInts str, [])

type Ptr = Int
type Op = Int -> Int -> Int

toInts :: String -> [Int]
toInts s = map (read . Text.unpack) $ Text.splitOn (Text.singleton ',') (Text.pack s)
readOpWithInput inp = readOp
    where
        readOp :: Ptr -> ([Int], [Int]) -> ([Int], [Int])
        readOp p (is,output) =
            let eval :: [Int] -> Ptr -> Op -> Ptr -> Int
                eval (a:b:_) p1 op p2 = (mode a p1) `op` (mode b p2)
                mode :: Int -> Ptr -> Int
                mode 0 p = is !! p
                mode 1 p = p
                putAt :: Int -> Ptr -> [Int]
                putAt i pr = (take pr is) ++ (i:(drop (pr+1) is))
                (o:os) = drop p is
                modOp i = let i' = i `mod` 10
                    in i':modOp (i `div` 10)
                lt = (\x y -> if x < y then 1 else 0)
                eq = (\x y -> if x == y then 1 else 0)
                jumpIf (a:b:_) f p1 pj = if f (mode a p1)
                    then readOp (mode b pj) (is, output)
                    else readOp (p+3) (is,output)
            in case (modOp o, os) of
                (1:0:modes ,p1:p2:pres:_) -> readOp (p+4) ((putAt (eval modes p1 (+) p2) pres),output)
                (2:0:modes ,p1:p2:pres:_) -> readOp (p+4) ((putAt (eval modes p1 (*) p2) pres),output)
                (3:0:modes ,ps:_ )        -> readOp (p+2) ((putAt inp ps),output)
                (4:0:modes ,pr:_)         -> readOp (p+2) (is, (is !! pr):output)

                (5:0:modes, p1:pj:_) -> jumpIf modes (0 /=) p1 pj
                (6:0:modes, p1:pj:_) -> jumpIf modes (0 ==) p1 pj
                (7:0:modes, p1:p2:pres:_) -> readOp (p+4) ((putAt (eval modes p1 lt p2) pres),output)
                (8:0:modes, p1:p2:pres:_) -> readOp (p+4) ((putAt (eval modes p1 eq p2) pres),output)

                (9:9:modes,_) -> (is, output)
