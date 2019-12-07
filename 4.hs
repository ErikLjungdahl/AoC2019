import Data.List
run f = do
    str <- readFile "input4.txt"
    let (start, end) = (take 6 str, drop 7 str)
    return $ f start end

f2 s e= length (filter doubleExists (f s e))
     where
        doubleExists :: String -> Bool
        doubleExists ls = any (\sl -> length sl == 2) (group ls)
f1 s e = length $ f s e

f:: String -> String -> [String]
f start end = (filter (flip (inc)  False) input)
    where
        startInt  = read start :: Int
        endInt    = read end   :: Int
        input = [ show n | n <- [startInt..endInt]]
        inc :: String -> Bool -> Bool
        inc (a:[]) b = True && b
        inc (a1:a2:as) b | a1 == a2 = inc (a2:as) True
                         | a1 <  a2 = inc (a2:as) b
                         | otherwise = False
