module Golf where

skips :: [a] -> [[a]]
skip l n = [ x | (x,i) <- zip l [1..], (i + n ) `mod` n == 0 ]
skips l = map (skip l) [1..length l] 


localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima l@(a:b:c:xs) 
    | a < b && b > c = [b] ++ (localMaxima $ drop 1 l)
localMaxima l = localMaxima $ drop 1 l
