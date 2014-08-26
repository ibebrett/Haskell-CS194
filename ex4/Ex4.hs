module Ex4 where

fun1 :: [Integer] -> Integer
fun1 = product .  map (\x -> x-2) . filter even

fun2 :: Integer -> Integer
fun2  = until (==0) (\x -> if even x then x `div` 2 else (if x==1 then 0 else 3*x+1))

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

--foldTree :: [a] -> Tree a
--foldTree 

xor :: [Bool] -> Bool
xor = foldr (\x y -> x /= y) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> [f x] ++ y) []

--sieveSundaram :: Integer -> [Integer]
--sieveSundaram n = asFold (filter (\x@(i,j) y -> y /= 2*i*j + i + j) ) [1..n] [(i,j) | i <- [1..n], j <- [1..n]]
