import qualified Data.Text as Text --(map, take, drop, splitAt)
import Data.List
run f = do
    str <- readFile "input7.txt"
    return $ f str

f7 str = maximum $
            map (\list -> foldr (\phase inp ->
                                last $ snd $ readOpWithInput [phase,inp] 0 (toInts str, []))
                                0 list)
                (permutations [0..4])
f7_2 str = maximum $ map (f7_2' str) (permutations [5..9])
f7_2' str phases = let
    a = snd (readOpWithInput (phaseA:0:init e) 0 (toInts str, []))
    b = snd (readOpWithInput (phaseB:a) 0 (toInts str, []))
    c = snd (readOpWithInput (phaseC:b) 0 (toInts str, []))
    d = snd (readOpWithInput (phaseD:c) 0 (toInts str, []))
    e = snd (readOpWithInput (phaseE:d) 0 (toInts str, []))
    (phaseA:phaseB:phaseC:phaseD:phaseE:_) = phases
        in last e


type Ptr = Int
type Op = Int -> Int -> Int

toInts :: String -> [Int]
toInts s = map (read . Text.unpack) $ Text.splitOn (Text.singleton ',') (Text.pack s)


readOpWithInput :: [Int] -> Ptr -> ([Int], [Int]) -> ([Int], [Int])
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
                (3:0:modes ,ps:_ )        -> readOpWithInput (tail inp) (p+2) ((putAt (head inp) ps),output)
                (4:0:modes ,pr:_)         -> readOp (p+2) (is, output ++ [is !! pr])

                (5:0:modes, p1:pj:_) -> jumpIf modes (0 /=) p1 pj
                (6:0:modes, p1:pj:_) -> jumpIf modes (0 ==) p1 pj
                (7:0:modes, p1:p2:pres:_) -> readOp (p+4) ((putAt (eval modes p1 lt p2) pres),output)
                (8:0:modes, p1:p2:pres:_) -> readOp (p+4) ((putAt (eval modes p1 eq p2) pres),output)

                (9:9:modes,_) -> (is, output)
