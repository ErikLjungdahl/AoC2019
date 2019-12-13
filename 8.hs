{-# LANGUAGE LambdaCase #-}
import Data.List
run f = do
    str <- readFile "input8.txt"
    f (init str)

f8 str = return  o*t
    where
        (z,o,t) =minimum (map (foldr (bucket) (0,0,0)) (toLayers str))
        bucket '0' (z,o,t) = (z+1, o , t )
        bucket '1' (z,o,t) = ( z ,o+1, t )
        bucket '2' (z,o,t) = ( z , o ,t+1)
        bucket _   triple  = triple

f2 str = putStr . unlines $ toRows $ map draw (transpose(toLayers str))
    where
        draw :: String -> Char
        draw = \case
            '0':_ -> ' ' -- Black
            '1':_ -> '1' -- White
            '2':rest -> draw rest
            _ -> error "No color encoding"

--toRows :: [Int] -> [[Int]]
toRows [] = []
toRows as = (take (25) as) : (toRows (drop (25) as))

toLayers :: String -> [String]
toLayers [] = []
toLayers as = (take (25*6) as) : (toLayers (drop (25*6) as))
