{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
    mempty = Score 0
    mappend = (+)


score :: Char -> Score
score c
    | x `elem` ['A', 'E', 'I', 'L', 'N', 'O', 'R', 'S', 'T', 'U' ] = 1
    | x `elem` ['D', 'G']                                          = 2
    | x `elem` ['C', 'M', 'P']                                     = 3
    | x `elem` ['F', 'V', 'W', 'Y']                                = 4
    | x `elem` ['K']                                               = 5
    | x `elem` ['J', 'X']                                          = 8
    | x `elem` ['Q', 'Z']                                          = 10
    | otherwise                                                    = 0
    where x = toUpper c

scoreString :: String -> Score
scoreString s = foldl (<>) mempty  (map score s)
