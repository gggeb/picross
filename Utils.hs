module Utils
    ( bound
    , align
    , longestLength
    , modifyAt
    , replaceAt
    , rotate
    ) where

bound mi ma v
    | v > ma    = ma
    | v < mi    = mi
    | otherwise = v

align d p xs = replicate (d - length xs) p ++ xs

longestLength xs = maximum $ map (length) xs

modifyAt i f xs = s1 ++ [f x] ++ s2
  where
    (s1, x: s2) = splitAt i xs

replaceAt i n xs = modifyAt i (\_ -> n) xs

rotate xs
    | concat xs == [] = []
    | otherwise       = [rotatedHead xs] ++ (rotate $ rotatedTail xs)
  where
    rotatedHead xs = map head xs
    rotatedTail xs = map tail xs
