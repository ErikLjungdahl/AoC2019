import qualified Data.HashMap.Strict as M
import Data.Tuple
import Control.Monad
import Data.Foldable
import Data.Maybe
run f = do
    str <- readFile "input6.txt"
    let pairs = map (\line -> ((takeWhile (\c -> c /= ')') line), tail (dropWhile (\c -> c /= ')') line ))) (lines str)
    return $ f pairs


f6_1 :: [(String,String)] -> Int
f6_1 list =
    let mp = M.fromListWith (++) [(k , [v]) | (k, v) <- list]
        com = "COM"
        acc :: Int -> String -> Int
        acc n key = case M.lookup key mp of
            Just vs -> n + sum (map (acc (n+1)) vs)
            Nothing -> n
    in acc 0 com

f6_2 :: [(String,String)] -> Maybe Int
f6_2 list =
    let mp = M.fromListWith (++) [(k , [v]) | (k, v) <- list]
        rev = M.fromList (map swap list)
        goDown :: Int -> String -> Maybe Int
        goDown n key | key == "SAN" = Just n
                     | otherwise = case M.lookup key mp of
                            Just vs -> do
                                let list = map (goDown (n+1)) vs
                                msum list
                            Nothing -> Nothing
        goUp :: Int -> String -> Maybe Int
        goUp n key | key == "SAN" = Just n
                   | otherwise = case M.lookup key rev of
                       Just v -> case goDown (n+1) v of
                           Just n' -> Just n'
                           Nothing -> goUp (n+1) v
                       Nothing -> Nothing
    in case goDown 0 "YOU" of
        Just n -> Just n
        Nothing -> fmap (\i -> i-2) $ goUp 0 "YOU"
