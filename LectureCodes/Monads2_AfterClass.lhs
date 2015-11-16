Programming With Effects II
===========================

Announcements
-------------

* Grace Hopper Conference - you should go next year (men too)

* Next up: project proposal due next Sunday, October 25th
  - Sit next to your project partner (your choice) today, if you already have one!
  - Post to Piazza if you still need a project partner
  - Project advice?  Emphasis on *design* and *specification*

* Homework 5 due tonight        [2 groups have submitted already, standard late policy]

* Hw 3 peer review due tonight  [10 individual submissions, no late period]


Set-up
------

In this lecture, we'll continue our study of monads via examples of
*specific* monads to try to understand how they work. At this point,
don't panic if you don't understand the big picture: each of the
specific instances is useful in its own right. For the moment, think
of monads as
[burritos](http://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/),
especially since they really
[are](http://blog.plover.com/prog/burritos.html).

> {-# LANGUAGE NoImplicitPrelude #-}

> module Monads2 where
> import Prelude hiding (getLine,sequence,(>>))
> import Data.Map (Map)
> import qualified Data.Map as Map
> import System.Random (StdGen, next, split, mkStdGen)
> import Control.Monad (liftM, ap)

This module depends on an auxiliary module that we will define in class. 

> import State

Note: to load this module into ghci, you must first change into the
current subdirectory so that ghci can find it.  E.g.,

  :cd /Users/sweirich/current/cis552/lectures

Alternatively, the `State` module is a subset of the functionality
in the standard library. We can also replace the import above with:

> -- import Control.Monad.State


The State Monad
===============

Now let us consider the problem of writing functions that manipulate
some kind of state. We're going to start with some examples of state
manipulation, written in an awkward style, and then show how monads
can cleanly abstract the sequencing necessary for such programs.

By way of an example of state manipulation, let's go back to binary
trees whose leaves contains values of some type `a`:

> data Tree a = Leaf a | Branch (Tree a) (Tree a)
>   deriving (Eq, Show)

Here is a simple example:

> tree :: Tree Char
> tree =  Branch (Branch (Leaf 'a') (Leaf 'b')) (Leaf 'c')

> tree1 :: Tree Char
> tree1 =  Branch (tree) (Branch (Branch (Leaf 'x') (Leaf 'y')) (Leaf 'z'))

A functional programmer would count the number of leaves in a tree
like so:

> countF :: Tree a -> Int
> countF (Leaf _) = 1
> countF (Branch t1 t2) = countF t1 + countF t2

On the other hand, consider how a C programmer would count the number of
leaves in a tree. She might create a local reference cell and then then walk
the tree, incrementing the reference cell at each leaf.  In pure code, we
*cannot* modify the values of any variables. However, we can emulate this
pattern with a *state transformer* -- a function that takes an initial state
as an input and returns the new state at every step.

In this example, the state, also called the "store", is an `Int`
(representing the current count) and a state transformer is a function
of type `Int -> Int`:

> -- The number of leaves in the tree that we have currently counted
> type Store = Int

> countI :: Tree a -> Int
> countI t = aux t 0 where
>   aux :: Tree a -> Store -> Store
>   aux (Leaf _)       s = s + 1
>   aux (Branch t1 t2) s = aux t2 s' 
>                          where 
>                            s' = aux t1 s



In general, a state transformer takes a current state as its argument,
and produces a modified state as its result, where the modified state
reflects any side effects performed by the function.

Next consider the problem of defining a function that labels each leaf
in such a tree with a unique or "fresh" integer.  This can be achieved
by taking the next fresh integer as an additional argument to a helper
function, and returning the next fresh integer as an additional
result.

> label1 :: Tree a -> Tree (a, Int)
> label1 t = fst (aux t 0) where
>    aux :: Tree a -> Store -> (Tree(a,Int), Store)
>    aux (Leaf x)       s = (Leaf (x, s), s+1)
>    aux (Branch t1 t2) s = 
>         let (lt,s')  = aux t1 s  in
>         let (rt,s'') = aux t2 s' in
>           ((Branch lt rt), s'')
>                         


This example demonstrates that in general, we may wish to return a
result value in addition to updating the store. For this reason, we
generalize our type of state transformers to also return a result
value, with the type of such values being a parameter of the `ST`
type:

> type ST a = Store -> (a, Store)


The state transformer may also wish to take argument values.  However,
there is no need to further generalize the `ST` type to take account
of this, because this behavior can already be achieved by _currying_
(i.e., treating multi-argument functions as functions returning
functions).  For example, the state transformer for the tree above
takes a tree and returns a labeled tree and has type `Tree a -> ST
(Tree (a,Int))`, which abbreviates the curried function type

~~~~~{.haskell}
Tree a -> Store -> (Tree (a, Int), Store)
~~~~~



The reason we are talking about state transformers is that
parameterized type `ST` is a *monad*.  What are its definitions of
`return` and `bind`? Can we abstract them from the definition of
`label1`?


> aux ::  Tree a -> Store -> (Tree (a,Int), Store)
> aux (Leaf x) = \s -> ( Leaf (x,s+1) ,s)
> aux (Branch t1 t2) = \s -> 
>      let (t1', s1) = aux t1 s in
>      let (t2', s2) = aux t2 s1 in
>        (Branch t1' t2', s2)


How should we fill in the definitions below? How can we rewrite the definition
of `aux` above to use these definitions?

> returnST :: a -> ST a
> -- returnST :: a -> Store -> (a, Store)
> returnST a = \s -> (a, s)

> bindST :: ST a -> (a -> ST b) -> ST b
> --bindST (Store -> (a, Store)) -> (a -> ST b) -> (Store -> (b, Store))
> bindST st f = f $ fst (st 0)



~~~~~{.haskell}
type ST a = Store -> (a, Store)

instance Monad ST where
   -- return :: a -> ST a
   return    = returnST

   -- (>>=)  :: ST a -> (a -> ST b) -> ST b
   st >>= f  = bindST st f
~~~~~

That is, `return` converts a value into a state transformer that
simply returns that value without modifying the state.

In turn, `>>=` provides a means of sequencing state transformers: 
`st >>= f` applies the state transformer `st` to an initial state `s`,
then applies the function `f` to the resulting value `x` to give a
second state transformer `(f x)`, which is then applied to the
modified state `s'` to give the final result.

Note that `return` could also be defined by `return x s = (x,s)`.
However, we prefer the above definition in which the second argument
`s` is shunted to the body of the definition using a lambda
abstraction, because it makes explicit that `return` is a function
that takes a single argument and returns a state transformer, as
expressed by the type `a -> ST a`: A similar comment applies to the
above definition for `>>=`.

We conclude this section with two technicalities.  

1. In Haskell, types defined using the `type` mechanism cannot be made into
   instances of classes.  Hence, in order to make ST into an instance of the
   class of monadic types, in reality it needs to be redefined using the "data"
   (or `newtype`) mechanism, which requires introducing a dummy constructor
   (called `S` for brevity):

> -- type ST a = Store -> (a, Store)

   It is convenient to define our own application function for this type,
   which simply removes the dummy constructor. We do so using a _record
   type_ declaration, which lets us name the arguments to a data
   constructor.

> newtype ST2 a = S { apply :: Store -> (a, Store) }
> -- Alternate Def
> --data ST2 a = S (Store -> (a, Store))

~~~~~~~~~~~~~~~{.haskell}
*Monads2> :type S
S     :: (Store -> (a,Store)) -> ST2 a

*Monads2> :type apply
apply :: ST2 a -> Store -> (a, Store)
~~~~~~~~~~~~~~~

   (Records in Haskell are pretty similar to most other languages, but
   with a few convenient twists.  Chapter 3 of RWH gives more details.)

   `ST2` is now defined as a monadic type (an instance of the `Monad`
    class) as follows:

> instance Monad ST2 where
>   -- return :: a -> ST2 a
>   return x   = S (\ s -> (x,s))

>  -- (>>=)  :: ST2 a -> (a -> ST2 b) -> ST2 b
>   st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

   (*Aside*: there is no runtime overhead for manipulating the dummy
   constructor because we defined ST2 using the `newtype` mechanism of
   Haskell, rather than `data`.)

2. Starting with GHC 7.10, all monads in Haskell must also be applicative
   functors. However, we can declare these instances using `ap` and `liftM`


> instance Functor ST2 where
>   fmap  = liftM

> instance Applicative ST2 where
>   pure  = return
>   (<*>) = ap

> instance Show a => Show (ST2 a) where
>   show (S a) = show (a 0)



Now, let's rewrite the tree labeling function with the State 
monad.

In order to generate a fresh integer, we define a special state
transformer that simply returns the current state as its result, and
the next integer as the new state:

> fresh :: ST2 Int  
> fresh = S (\s -> (s, s+1))



Using this, together with the `return` and `>>=` primitives that
are provided by virtue of `ST` being a monadic type, it is now
straightforward to define a function that takes a tree as its
argument, and returns a state transformer that produces the
same tree with each leaf labelled by a fresh integer:

> {-  st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')

> --mlabel            :: Tree a -> ST2 (Tree (a,Int))
> --mlabel (Leaf x)     = 
> --      fresh >>= (\i -> return (Leaf (x,i))) 
> --mlabel (Branch l r) = undefined
> -}

> mlabel            :: Tree a -> ST2 (Tree (a,Int))
> mlabel (Leaf x)     = do                        ---
>                       i <- fresh                ---
>                       return (Leaf (x,i))       --- (\s -> (Leaf (x,s), s+1)
> mlabel (Branch l r) = do
>                       l' <- mlabel l
>                       r' <- mlabel r
>                       return (Branch l' r')
>                       





Note that the programmer does not have to worry about the tedious
and error-prone task of dealing with the plumbing of fresh labels,
as this is handled automatically by the state monad.

Finally, we can now define a function that labels a tree by
simply applying the resulting state transformer with zero as
the initial state, and then discarding the final state:

> label  :: Tree a -> Tree (a, Int)
> label t = fst $ apply (mlabel t) 0

For example, `label tree` gives the following result:

~~~~~{.haskell}
ghci> label tree
Branch (Branch (Leaf ('a', 0)) (Leaf ('b',1))) (Leaf ('c', 2))
~~~~~

A Generic State Transformer
===========================

Often, the *store* that we want to have will have multiple components
-- e.g., multiple variables whose values we might want to update. This
is easily accomplished by using a different type for `Store` above,
for example, if we want two integers, we might use the definition

~~~~~{.haskell}
type Store = (Int, Int)
~~~~~

and so on. 

However, we would like to write reusable code, which will work with
any store.

The file [State](State.html)  ( [lhs version](State.lhs) ) contains
a generic library for that purpose.


Using a Generic State Transformer
=================================

Let us use our generic state monad to rewrite the tree labeling function 
from above. Note that the actual type definition of the generic transformer
is *hidden* from us, so we must use only the publicly exported functions:
`get`, `put` and `runState` (in addition to the monadic functions we get for
free.)


First, we write an action that returns the next fresh integer. (Note
that the first type argument is the store, while the second is the
result type of the monadic action.)

> -- freshS :: S (Int -> (Int,Int))
> freshS :: State Int Int
> freshS = do
>          i <- get
>          return (i+1)

Now, the labeling function is straightforward

> mlabelS :: Tree t -> State Int (Tree (t, Int))
> mlabelS (Leaf x)     = do
>                        modify (+1) 
>                        i <- get -- put i
>                        return (Leaf (x,i))
> mlabelS (Branch l r) = do
>                        l' <- mlabelS l
>                        r' <- mlabelS r
>                        return (Branch l' r')

Easy enough!

~~~~~{.haskell}
ghci> runState (mlabelS tree) 0
~~~~~






We can *execute* the action from any initial state of our choice

~~~~~{.haskell}
ghci> runState (mlabelS tree) 1000
~~~~~






Now, what's the point of a generic state transformer if we can't have richer
states? Next, let us extend our `fresh` and `label` functions so that 

- each node gets a new label (as before), and

- the state also contains a map of the *frequency* with which each 
  leaf value appears in the tree.

Thus, our state will now have two elements, an integer denoting the *next*
fresh integer, and a `Map a Int` denoting the number of times each leaf
value appears in the tree. (Documentation for the [Data.Map module](http://www.haskell.org/ghc/docs/latest/html/libraries/containers/Data-Map.html). )

> data MySt a = M { index :: Int
>                 , freq  :: Map a Int }
>               deriving (Eq, Show)

We write an *action* that returns the next fresh integer:

> freshM :: State (MySt a) Int
> freshM = undefined
>        --  i <- get
>        --  return ((index i) +1)

Similarly, we want an action that updates the frequency of a given
element `k`. 

> updFreqM :: Ord a => a -> State (MySt a) ()
> updFreqM k = undefined --state (\ ms -> (k,(M (index ms,update k (freq ms)))))

> update :: Ord a => a -> Map a Int -> Map a Int
> update k m = case Map.lookup k m of
>               Just x  -> Map.insert k (x+1) m
>               Nothing -> Map.insert k 0 m



And with these two, we are done

> mlabelM :: Ord a => Tree a -> State (MySt a) (Tree (a, Int))
> mlabelM (Leaf x)     =  undefined
> mlabelM (Branch l r) =  undefined

Now, our *initial* state will be something like

> initM :: MySt a
> initM = M 0 Map.empty

and so we can label the tree

~~~~~{.haskell}
ghci> let tree2   = Branch tree tree 
ghci> let (lt, s) = runState (mlabelM tree) initM 

ghci> lt
Branch (Branch (Branch (Leaf ('a',0)) (Leaf ('b',1))) (Leaf ('c',2))) (Branch (Branch (Leaf ('a',3)) (Leaf ('b',4))) (Leaf ('c',5)))

ghci> s
M {index = 6, freq = fromList [('a',2),('b',2),('c',2)]}
~~~~~


Credit
------

This lecture is a revised version of the lecture notes by [Graham
Hutton][0], January 2011

[0]: http://www.cs.nott.ac.uk/~gmh/monads
