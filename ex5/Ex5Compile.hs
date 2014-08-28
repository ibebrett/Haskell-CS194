{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Ex5Compile where

import Parser
import StackVM

class Expr a where
    add, mul :: a -> a -> a
    lit :: Integer -> a

instance Expr Program where
    add pa pb = [Add] ++ pa ++ pb
    mul pa pb = [Mul] ++ pa ++ pb
    lit n = [PushI n]

compile :: String -> Maybe Program

compile s = parseExp lit add mul s
