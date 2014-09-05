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

testList :: JoinList Size String
testList = (Append (Size 4)
                ( Append (Size 2) 
                    (Single (Size 1) "trick joke")
                    (Single (Size 1) "happy dude")
                )
                ( Append (Size 2) 
                    (Single (Size 1) "smile corn")
                    (Single (Size 1) "drown duck")
                )
           )

testIndex1 = indexJ 1 testList

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a

dropJ index Empty = Empty
dropJ index s@(Single m a) = if index == 0 then s else Empty
dropJ index (Append m a b) = case compare index sizem of
    GT -> Empty
    EQ -> Empty
    LT -> case compare index sizea of
        GT -> dropJ (index-sizea) b
        EQ -> dropJ (index-sizea) b
        LT -> Append ( (tag (dropJ index a)) <> (tag b)) (dropJ index a) b 
    where sizea = (getSize (size (tag a)))
          sizeb = (getSize (size (tag b)))
          sizem = (getSize (size m))


takeJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> JoinList b a
takeJ index Empty = Empty
takeJ index s@(Single m a) = if index > 0 then s else Empty
takeJ index whole@(Append m a b) = case compare index sizem of
    GT -> whole
    EQ -> whole
    LT -> case compare index sizea of
        GT -> Append ( (tag a) <> (tag (dropJ (index-sizea) b)) ) a (dropJ (index-sizea) b)
        EQ -> takeJ index a
        LT -> takeJ index a
    where sizea = (getSize (size (tag a)))
          sizeb = (getSize (size (tag b)))
          sizem = (getSize (size m))


