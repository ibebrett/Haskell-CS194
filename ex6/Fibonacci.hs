{-# LANGUAGE FlexibleInstances #-}

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

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a arest) (Stream b brest) = (Stream a (Stream b (interleaveStreams arest brest)))

--ruler :: Stream Integer

--x :: Stream Integer

instance Num (Stream Integer) where
    (+) (Stream a arest) (Stream b brest) = Stream (a+b) (arest + brest)
    (*) (Stream a arest) (Stream b brest) = (Stream (a*b) (streamRepeat 0)) + (Stream 0  ((streamMap (*a) brest) + (arest*(Stream b brest)) ) )
    abs (Stream a rest ) = streamMap abs (Stream a rest)
    signum _ = 1
    fromInteger i = Stream i (streamRepeat 0)
    negate (Stream a rest) = streamMap negate (Stream a rest)

instance Fractional (Stream Integer) where
    (/) (Stream a arest) (Stream b brest) = (Stream (div a b) (streamRepeat 0)) + (Stream 0 ((streamMap (`div` b) (arest - ((Stream a arest)/(Stream b brest))*brest))))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2 ) where
    x = Stream 0 (Stream 1 (streamRepeat 0))
