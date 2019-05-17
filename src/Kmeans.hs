module Kmeans
    ( distance_p_to_p,
        near_points,
        nearest
    )  where

import Data.Tuple

first :: (a, b, c) -> a
first (a,_,_) = a

second :: (a, b, c) -> b
second (_,b,_) = b

third :: (a, b, c) -> c
third (_,_,c) = c

distance_p_to_p :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distance_p_to_p pA pB = sqrt( ((first pA) - (first pB))^2 + ((second pA) - (second pB))^2 + ((third pA) - (third pB))^2 )

nearest :: [Float] -> Float -> Int -> Int -> Int
nearest [] short it minit = minit
nearest (x:xs) short it minit
            | (short > x) = nearest xs x (it+1) it
            | otherwise = nearest xs short (it+1) minit

near_points :: [(Float, Float, Float)] -> [(Float, Float, Float)] -> Int -> [Float] -> [(Int, (Float, Float, Float))] -> [(Int, (Float, Float, Float))]
near_points [] k j distances ret = ret
near_points (x:xs) k j distances ret =
            if (j < length k) then
                near_points (x:xs) k (j+1) (distances ++ [distance_p_to_p x (k!!j)]) ret
            else do
                let it = nearest distances (distances!!0) 0 0
                near_points xs k 0 [] (ret ++ [(it, x)])
