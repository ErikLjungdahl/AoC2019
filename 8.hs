run f = do
    str <- readFile "input8.txt"
    return $ f (init str)

f8 str = o*t
    where
        (z,o,t) =minimum (map (foldr (bucket) (0,0,0)) (toLayers str))
        bucket '0' (z,o,t) = (z+1, o , t )
        bucket '1' (z,o,t) = ( z ,o+1, t )
        bucket '2' (z,o,t) = ( z , o ,t+1)
        bucket _   triple  = triple
        toLayers [] = []
        toLayers as = (take (25*6) as) : (toLayers (drop (25*6) as))
