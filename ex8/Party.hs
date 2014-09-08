module Party where

import Data.Monoid
import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emps++[emp]) ((empFun emp) + fun)

instance Monoid GuestList where
    mempty  = (GL [] 0)
    mappend (GL emps fun) b = (foldr glCons b emps)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gla@(GL _ fa) glb@(GL _ fb) = if fa > fb then gla else glb 

treeVisit :: Tree b -> [b]
treeVisit t = [rootLabel t] ++ (foldr (++) [] (map (\x -> treeVisit x) (subForest t)) )

z = Node 1 [ (Node 2 []), (Node 3 []), (Node 4 []) ]

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f s t = foldl f s (treeVisit t)

