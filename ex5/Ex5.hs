module Ex5 where

import ExprT
import Parser

eval :: ExprT -> Integer

eval (Lit n)   = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s =  case (parseExp Lit Add Mul s) of
    Nothing -> Nothing
    Just x@(Add a b) -> Just $ eval x  -- turn this all into one guard
    Just y@(Mul a b) -> Just $ eval y
    Just z@(Lit a)   -> Just $ eval z

class Expr a where
    add, mul :: a -> a -> a
    lit :: Integer -> a

instance Expr ExprT where
    add a b = Add a b
    mul x y = Mul x y
    lit n   = Lit n

instance Expr Integer where
    add a b = a + b
    mul x y = x * y
    lit n   = n

instance Expr Bool where
    add a b = a || b
    mul x y = x && y
    lit n
        | n <= 0 = False
        | otherwise = True


