import qualified Data.Text as Text --(map, take, drop, splitAt)
import qualified Data.Text.IO as Text --(map, take, drop, splitAt)
import Data.List hiding ((!!), take, drop)
import Prelude hiding ((!!), take, drop)
run f = do
    str <- Text.readFile "input9.txt"
    return $ f str

f1 str = head $ snd $ readOpWithInput inp 0 0 (toIntsInf0s str, [])
    where inp = (1:1:[])
f2 str = last $ snd $ readOpWithInput inp 0 0 (toIntsInf0s str, [])
    where inp = (2:[])

ftest = toIntsInf0s

main = do
    res <- run f2
    print res

type Ptr = Integer
type Op = Integer -> Integer -> Integer


toIntsInf0s :: Text.Text -> [Integer]
toIntsInf0s s = (map (read . Text.unpack) $ Text.splitOn (Text.singleton ',') (s)) ++ repeat 0

(!!) = genericIndex
take = genericTake
drop = genericDrop

readOpWithInput :: [Integer] -> Ptr -> Ptr -> ([Integer], [Integer]) -> ([Integer], [Integer])
readOpWithInput inp rel = readOp
    where
        readOp :: Ptr -> ([Integer], [Integer]) -> ([Integer], [Integer])
        readOp p (is,output) =
            let eval :: [Integer] -> Ptr -> Op -> Ptr -> Ptr -> [Integer]
                eval (a:b:mr:_) p1 op p2 pres = putAt val (modePtr mr pres)
                            where val = (mode a p1) `op` (mode b p2)
                mode :: Integer -> Ptr -> Integer
                mode 0 p = is !! p
                mode 1 p = p
                mode 2 p = is !! (p + rel)
                modePtr :: Integer -> Ptr -> Ptr
                modePtr 0 p' = p'
                modePtr 2 p' = p' + rel
                putAt :: Integer -> Ptr -> [Integer]
                (o:os) = drop p is
                putAt i pr = (take pr is) ++ (i:(drop (pr+1) is))
                modOp i = let i' = i `mod` 10
                    in i':modOp (i `div` 10)
                lt = (\x y -> if x < y then 1 else 0)
                eq = (\x y -> if x == y then 1 else 0)
                jumpIf (a:b:_) f p1 pj = if f (mode a p1)
                    then readOp (mode b pj) (is, output)
                    else readOp (p+3) (is,output)
            in case (modOp o, os) of
                -- Addition
                (1:0:modes ,p1:p2:pres:_) -> readOp (p+4) (eval modes p1 (+) p2 pres,output)
                -- Multiplication
                (2:0:modes ,p1:p2:pres:_) -> readOp (p+4) (eval modes p1 (*) p2 pres,output)
                -- Input
                (3:0:ms:_ ,ps:_ ) -> readOpWithInput (tail inp) rel (p+2) ((putAt (head inp) (modePtr ms ps)),output)
                -- Output
                (4:0:mode1:_ ,pr:_)         -> readOp (p+2) (is, output ++ [mode mode1 pr])

                -- Jump if true (!0)
                (5:0:modes, p1:pj:_) -> jumpIf modes (0 /=) p1 pj
                -- Jump if false (0)
                (6:0:modes, p1:pj:_) -> jumpIf modes (0 ==) p1 pj
                -- Less then
                (7:0:modes, p1:p2:pres:_) -> readOp (p+4) (eval modes p1 lt p2 pres,output)
                -- Equals
                (8:0:modes, p1:p2:pres:_) -> readOp (p+4) (eval modes p1 eq p2 pres,output)

                -- Adjust relative base
                (9:0:mode1:_, p1:_) -> readOpWithInput inp (rel + (mode mode1 p1)) (p+2) (is, output)

                -- NOP
                (9:9:modes,_) -> (is, output)
                (0:modes,_) -> (is, output)
