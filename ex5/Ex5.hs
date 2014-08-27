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
