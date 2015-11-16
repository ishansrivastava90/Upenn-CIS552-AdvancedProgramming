{-# LANGUAGE NoImplicitPrelude #-}

module Monads2 where
import Prelude hiding (getLine,sequence,(>>))
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random (StdGen, next, split, mkStdGen)
import Control.Monad (liftM, ap)

import State

-- import Control.Monad.State

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Eq, Show)

tree :: Tree Char
tree =  Branch (Branch (Leaf 'a') (Leaf 'b')) (Leaf 'c')

countF :: Tree a -> Int
countF (Leaf _) = 1
countF (Branch t1 t2) = countF t1 + countF t2

-- The number of leaves in the tree that we have currently counted
type Store = Int

countI :: Tree a -> Int
countI t = aux t 0 where
  aux :: Tree a -> Store -> Store
  aux (Leaf _)       s = undefined
  aux (Branch t1 t2) s = undefined

label1 :: Tree a -> Tree (a, Int)
label1 t = fst (aux t 0) where
   aux :: Tree a -> Store -> (Tree(a,Int), Store)
   aux (Leaf x)       s = undefined
   aux (Branch t1 t2) s = undefined

type ST a = Store -> (a, Store)

aux ::  Tree a -> Store -> (Tree (a,Int), Store)
aux (Leaf x) = \s -> ( Leaf (x,s+1) ,s)
aux (Branch t1 t2) = \s -> 
     let (t1', s1) = aux t1 s in
     let (t2', s2) = aux t2 s1 in
       (Branch t1' t2', s2)

returnST :: a -> ST a
returnST = undefined

bindST :: ST a -> (a -> ST b) -> ST b
bindST = undefined

-- type ST a = Store -> (a, Store)

newtype ST2 a = S { apply :: Store -> (a, Store) }

instance Monad ST2 where
  -- return :: a -> ST2 a
  return x   = S (\ s -> (x,s))
 
  -- (>>=)  :: ST2 a -> (a -> ST2 b) -> ST2 b
  st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

instance Functor ST2 where
  fmap  = liftM

instance Applicative ST2 where
  pure  = return
  (<*>) = ap

fresh :: ST2 Int  --- Store -> (Int, Store)
fresh = undefined

mlabel            :: Tree a -> ST2 (Tree (a,Int))
mlabel (Leaf x)     = undefined
mlabel (Branch l r) = undefined

label  :: Tree a -> Tree (a, Int)
label t = undefined

freshS :: State Int Int
freshS = undefined

mlabelS :: Tree t -> State Int (Tree (t, Int))
mlabelS (Leaf x)     = undefined
mlabelS (Branch l r) = undefined

data MySt a = M { index :: Int
                , freq  :: Map a Int }
              deriving (Eq, Show)

freshM :: State (MySt a) Int
freshM = undefined 

updFreqM :: Ord a => a -> State (MySt a) ()
updFreqM k = undefined

mlabelM :: Ord a => Tree a -> State (MySt a) (Tree (a, Int))
mlabelM (Leaf x)     =  undefined
mlabelM (Branch l r) =  undefined

initM :: MySt a
initM = M 0 Map.empty

