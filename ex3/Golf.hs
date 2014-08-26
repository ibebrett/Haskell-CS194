module Golf where

skips :: [a] -> [[a]]
skip l n = [ x | (x,i) <- zip l [1..], (i + n ) `mod` n == 0 ]
skips l = map (skip l) [1..length l] 


localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima l@(a:b:c:xs) 
    | a < b && b > c = [b] ++ (localMaxima $ drop 1 l)
localMaxima l = localMaxima $ drop 1 l

histogram :: [Integer] -> String
histSpot x y l = if length  (filter (x==) l) > y then "*" else " "
maxHist l = maximum [length (filter (x==) l) | x <- [0..9] ] 
histogram l = foldr (++) [] [ (foldr (++) [] [ histSpot x y l | x <- [0..9] ]) ++ "\n" | y <- reverse [0..(maxHist l) - 1] ] ++ "==========" ++ "\n0123456789\n"
