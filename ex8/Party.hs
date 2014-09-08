module Party where

import Data.Monoid
import Data.Tree
import Data.List
import Text.Printf
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emps++[emp]) ((empFun emp) + fun)

instance Monoid GuestList where
    mempty  = (GL [] 0)
    mappend a (GL emps fun)  = (foldr glCons a emps)

glFun :: GuestList -> Fun
glFun (GL _ f) = f

moreFun :: GuestList -> GuestList -> GuestList
moreFun gla@(GL _ fa) glb@(GL _ fb) = if fa > fb then gla else glb 

treeVisit :: Tree b -> [b]
treeVisit t = [rootLabel t] ++ (foldr (++) [] (map (\x -> treeVisit x) (subForest t)) )

z = Node 1 [ (Node 2 []), (Node 3 []), (Node 4 []) ]

treeFold :: (a -> [b] -> b)  -> Tree a -> b
treeFold f t = f (rootLabel t) (map (treeFold f) (subForest t) )

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (GL [boss] (empFun boss), mempty)
nextLevel boss prevList = 
    (
        foldl (<>) (GL [boss] (empFun boss)) (map snd prevList),  -- if we include the boss, then we have to take the nonboss guys from below
        foldl (<>) mempty (map (\x -> max (fst x) (snd x)) prevList)
    )

maxFun :: Tree Employee -> GuestList
maxFun t = max (fst toplevel) (snd toplevel)
            where toplevel = treeFold nextLevel t


getNames :: GuestList -> String
getNames (GL emps fun) = foldl (\x y -> x++"\n"++y) "" (sort (map empName emps))

main :: IO()
main = readFile "company.txt" >>= (\l -> showMaxFun (maxFun (read l)))

showMaxFun :: GuestList -> IO()
showMaxFun mf = printf "Total fun: %d" (glFun mf) >> putStrLn (getNames mf)
