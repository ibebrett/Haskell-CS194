module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag (Single m a) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

indexJ :: (Sized b, Monoid b) => 
          Int -> JoinList b a -> Maybe a

indexJ index Empty          = Nothing
indexJ index (Single m a)   = if index == 0 then Just a else Nothing
indexJ index (Append m a b) = case compare index sizea of
    LT -> indexJ index a
    GT -> indexJ (index - sizea) b
    EQ -> indexJ (index - sizea) b
    where sizea  = (getSize (size (tag a)))

--dropJ :: (Sized b, Monoid b) =>
--         Int -> JoinList b a -> JoinList b a

testList :: JoinList Size String
testList = (Append (Size 2)
                ( Single (Size 1) "helloworld")
                ( Single (Size 1) "smile corn")
           )

testIndex1 = indexJ 1 testList
