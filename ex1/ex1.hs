import Data.List.Split

enumerate = zip [0..]

toDigits n = [ (read n)::Integer | n <- chunksOf 1 $ show n ]
toDigitsRev n = reverse $ toDigits n

doubleEveryOther n = [ if i `mod` 2 == 0 then 2*n else n | (i, n) <- enumerate $ toDigits n]

sumDigits l = sum [ sum $ toDigits i | i <- l ]

validate n = sumDigits ( doubleEveryOther n ) `mod` 10 == 0

type Peg = String
type Move = (Integer, Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi 0 _ _ _  = []
hanoi 1 a b c = [(1, a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(n,a,b)] ++ (hanoi (n-1) c b a)

