module Party where

import Data.Monoid
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emps++[emp]) ((empFun emp) + fun)

instance Monoid Employee where
    mempty  = (GL [] 0)
    mappend (GL emps fun) b = (foldl glCons b emps)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gla@(GL _ fa) glb@(GL _ fb) = if fa > fb then gla else glb 

