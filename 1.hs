
run :: (Int -> Int) -> IO Int
run f = do
    str <- readFile "input1.txt"
    return $ sum $ map (f . read) (lines str)

f1 :: Int -> Int
f1 = (\i -> (i `div` 3) -2)

f2 :: Int -> Int
f2 n | a <= 0    = 0
     | otherwise = a + f2 a
        where a = f1 n
