fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 = [0, 1] ++ [ fibs2!!(n-1) + fibs2!!(n-2) | n <- [2..] ]

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a rest) = [a] ++ streamToList rest

instance Show a => Show (Stream a) where
    show s = show $ take 20 $ streamToList s

streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a rest) = Stream (f a) (streamMap f rest)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Stream a (streamFromSeed f (f a))


