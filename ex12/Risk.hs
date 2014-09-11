{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
    deriving (Show)

aPos :: (Integer, Integer) -> Integer
aPos (x, y) = if x > y then 1 else -1

zipDiceForBattle :: Battlefield -> [DieValue] -> [DieValue] -> [(DieValue, DieValue)] 
zipDiceForBattle battle adice ddice = zip (take (maxAttackers $ attackers battle) adice) (take (maxDefenders $ defenders battle) ddice)

maxAttackers :: Army -> Int
maxAttackers a = max 0 (a  - 1)

maxDefenders :: Army -> Int
maxDefenders a = a

maxLosses :: Battlefield -> Int
maxLosses bf = min 2 (min (maxAttackers $ attackers bf) (maxDefenders $ defenders bf))

countAttackerWins :: [(DieValue, DieValue)] -> Int
countAttackerWins dvs = length $ filter (\(x,y) -> x > y) dvs

countDefenderWins :: [(DieValue, DieValue)] -> Int
countDefenderWins dvs = length $ filter (\(x,y) -> x > y) dvs

battle :: Battlefield -> Rand StdGen Battlefield

battle bf@(Battlefield numA numD) = do
    a0 <- die
    a1 <- die
    a2 <- die
    d0 <- die
    d1 <- die
    usedDice <- return (zipDiceForBattle bf [a0, a1, a2] [d0, d1])
    return (Battlefield 
                ((attackers bf ) - (maxLosses bf - (countAttackerWins usedDice) )) 
                ((defenders bf ) - (countAttackerWins usedDice))
           )

invasionOver :: Battlefield -> Bool
invasionOver bf = (attackers bf) < 2 || (defenders bf) == 0

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
    nbf <- battle bf
    if (invasionOver bf) then return bf else invade nbf

successfulInvasion :: Battlefield -> Bool
successfulInvasion bf = (defenders bf) == 0

successProbNth :: Battlefield -> Int -> Int -> Double -> Rand StdGen Double
successProbNth bf trial numTrials currProb = do
    nth <- invade bf
    if (trial >= numTrials) then return currProb else successProbNth bf (trial+1) numTrials (if successfulInvasion nth then (currProb + 1.0) else currProb)

successProb :: Battlefield -> Rand StdGen Double
--successProb bf = do
--    val <- successProbNth bf 0 1000 0.0
--    return (val/1000.0)

successProb bf =  (successProbNth bf 0 1000 0.0) >>=  (\v -> return (v/1000.0) )
