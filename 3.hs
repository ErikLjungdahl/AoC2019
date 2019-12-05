import Data.HashSet hiding (map, foldr)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as Text --(map, take, drop, splitAt)
--import Data.List hiding (insert)


run f = do
    str <- readFile "input3.txt"
    return $ f str

f1 :: String -> Int
f1 str = let a:b:[] = map (\ss -> toMap (toStrings ss) (0,0)) (lines str)
    in foldr1 (\x y -> if x == 0 then y else min x y)
              (map (\(x,y) -> abs x + abs y) (toList (intersection a b)))

f2 :: String -> Int
f2 str = let (as:bs:_) = lines str
             --bs = map (\ss -> toMap2 (toStrings ss) (0,0,0)) (lines str)
             a = toMap2 (toStrings as) (0,0,0)
             b = toMap2 (toStrings bs) (0,0,0)
             comb = M.intersectionWith (\x y -> x + y) a b
             val = (map (snd) (M.toList comb))
    in foldr1 (\x y -> if x == 0 then y else min x y) comb


--data Inst = U Int | D Int | R Int | L Int

toStrings :: String -> [String]
toStrings s = map (Text.unpack) $ Text.splitOn (Text.singleton ',') (Text.pack s)

toMap :: [String] -> (Int, Int) -> HashSet (Int, Int)
toMap [] _  = empty
toMap (i:is) (x,y) = let n = read (tail i) in
    case head i of
        'U' -> foldr (\y' -> insert (x,y')) (toMap is (x,y+n))  [y..(y+n)]
        'D' -> foldr (\y' -> insert (x,y')) (toMap is (x,y-n))  [(y-n)..y]
        'R' -> foldr (\x' -> insert (x',y)) (toMap is (x+n,y))  [x..(x+n)]
        'L' -> foldr (\x' -> insert (x',y)) (toMap is (x-n,y))  [(x-n)..x]

toMap2 :: [String] -> (Int, Int, Int) -> M.HashMap (Int, Int) Int
toMap2 [] _  = M.empty
toMap2 (i:is) (x,y, steps) = let n = read (tail i) in
    case head i of
        'U' -> foldr (\n' -> M.insertWith (\x y -> y) (x,y+n') (steps+n'  )) (toMap2 is (x,y+n, steps + n))  [0..n]
        'D' -> foldr (\n' -> M.insertWith (\x y -> y) (x,y-n') (steps+n'  )) (toMap2 is (x,y-n, steps + n))  [0..n]
        'R' -> foldr (\n' -> M.insertWith (\x y -> y) (x+n',y) (steps+n'  )) (toMap2 is (x+n,y, steps + n))  [0..n]
        'L' -> foldr (\n' -> M.insertWith (\x y -> y) (x-n',y) (steps+n'  )) (toMap2 is (x-n,y, steps + n))  [0..n]
