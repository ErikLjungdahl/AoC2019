
-- main :: IO Int
run1 = do
    str <- readFile "input1.txt"
    return $ sum $ map (f . read) (lines str)
run2 = do
    str <- readFile "input1.txt"
    return $ sum $ map (f2 . read) (lines str)

f :: Int -> Int
f = (\i -> (i `div` 3) -2)

f2 :: Int -> Int
f2 n | a <= 0    = 0
     | otherwise = a + f2 a
        where a = f n
