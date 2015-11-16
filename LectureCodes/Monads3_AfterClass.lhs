Monads III
==========

> {-# LANGUAGE NoImplicitPrelude #-}
> module Monads3 where
> import Prelude hiding (getLine, putStrLn, sequence, (>>), getLine)

> import State
> import Control.Monad (liftM, ap)

> import Data.Map (Map)
> import qualified Data.Map as Map

Recap from last time
====================

Last time we talked about "Store transformers", i.e. functions that take a
store and return a result and a new store.

> type Store = Int
> newtype ST2 a = S { apply :: Store -> (a, Store) }

We observed that the store transformer type is a Monad (and
Functor/Applicative):

> instance Functor ST2 where
>   fmap  = liftM
> instance Applicative ST2 where
>   pure  = return
>   (<*>) = ap
> instance Monad ST2 where
>   -- return :: a -> ST2 a
>   return x   = S (\ s -> (x,s))
>   -- (>>=)  :: ST2 a -> (a -> ST2 b) -> ST2 b
>   st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')


And put it to use "labelling" trees with fresh integers.

> data Tree a = Leaf a | Branch (Tree a) (Tree a)
>   deriving (Eq, Show)

> tree :: Tree Char
> tree =  Branch (Branch (Leaf 'a') (Leaf 'b')) (Leaf 'c')


> fresh :: ST2 Int  --- Store -> (Int, Store)
> fresh = S $ \s -> (s, s+1)

> mlabel            :: Tree a -> ST2 (Tree (a,Int))
> mlabel (Leaf x)       =  do 
>   s <- fresh 
>   return (Leaf (x, s))
> mlabel (Branch t1 t2) = do
>   t1' <- mlabel t1
>   t2' <- mlabel t2 
>   return (Branch t1' t2')

> label  :: Tree a -> Tree (a, Int)
> label t = fst (apply (mlabel t) 0)

For example, `label tree` gives the following result:

~~~~~{.haskell}
ghci> label tree
Branch (Branch (Leaf ('a', 0)) (Leaf ('b',1))) (Leaf ('c', 2))
~~~~~

This was great, but our `Store` type was rather constrained: we could only
keep track of a single `Int` at a time.

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


> freshS :: State Int Int
> freshS = do
>      x <- get
>      put (x+1) 
>      return x

Now, the labeling function is straightforward

> mlabelS :: Tree t -> State Int (Tree (t, Int))
> mlabelS (Leaf x)     = do
>   s <- freshS 
>   return (Leaf (x, s))
> mlabelS (Branch t1 t2) =  do
>   t1' <- mlabelS t1
>   t2' <- mlabelS t2 
>   return (Branch t1' t2')


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
> freshM = do 
>   s <- get
>   let i = index s
>   put (s {index = i+1 })        
>   return i

Similarly, we want an action that updates the frequency of a given
element `k`. 

> updFreqM :: Ord a => a -> State (MySt a) ()
> updFreqM k = do
>   s <- get
>   let m = freq s
>   let p = Map.lookup k m        
>   let nv = case p of  {       
>              Just i  -> i + 1 ;
>              Nothing -> 1
>            }
>   put (s {freq = Map.insert k nv m})        


And with these two, we are done

> mlabelM :: Ord a => Tree a -> State (MySt a) (Tree (a, Int))
> mlabelM (Leaf x)     =  do
>    s <- freshM
>    updFreqM x
>    return (Leaf (x, s))
> mlabelM (Branch l r) =  do 
>    l' <- mlabelM l
>    r' <- mlabelM r
>    return (Branch l' r')

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



Random Numbers
==============
See [RandomGen](RandomGen.html)


The IO Monad
============
Recall that interactive programs in Haskell are written using the
type `IO a` of "actions" that return a result of type `a`, but may
also perform some input/output.  A number of primitives are
provided for building values of this type, including:

~~~~~{.haskell}
return  :: a -> IO a
(>>=)   :: IO a -> (a -> IO b) -> IO b
getChar :: IO Char
putChar :: Char -> IO ()
~~~~~

The presence of `return` and `>>=` means that we can treat `IO` as a
monad, and hence (as we've seen) that the `do` notation can be used to
write interactive programs.  For example, the action that reads a
string of characters from the keyboard can be defined as follows:

> getLine :: IO String
> getLine = getChar >>= (\c -> 
>           if c == '\n' then return [] 
>           else getLine >>= (\cs -> return (c:cs)))


It is interesting to note that the `IO` monad can be viewed as a
special case of the State monad, in which the internal state is
a suitable representation of the "state of the world":

~~~~~{.haskell}
   type World = ...

   type IO a  = World -> (a,World)
~~~~~

That is, an action can be viewed as a function that takes the current
state of the world as its argument, and produces a value and a
modified world as its result, in which the modified world reflects any
input/output performed by the action.  In reality, Haskell systems
such as GHC implement actions in a more efficient manner, but for the
purposes of understanding the behavior of actions, the above
interpretation can be useful.


Monads As Programmable Semicolon
--------------------------------

It is sometimes useful to sequence two monadic expressions,
but discard the result value produced by the first:

      f (3);
      g (3);


> (>>) :: Monad m => m a -> m b -> m b
> mx >> my = mx >>= \ _ -> my


For example, in the state monad the `>>` operator is just normal
sequential composition, written as `;` in most languages. Without
using layout for the `do` notation, sequencing is a semicolon too.

> hello :: IO ()
> hello = putChar 'H' >> putChar 'e' >> putChar 'l' >> putChar 'l' >> putChar 'o'

> hi = do
>       putChar 'H'
>       putChar 'i'
>       return ()


Indeed, in Haskell the entire `do` notation with or without `;` is 
just [syntactic sugar][4] for `>>=` and `>>`. For this reason, we can 
legitimately say that Haskell has a [*programmable semicolon*][5].


Other topics
------------

The subject of monads is a large one, and we have only scratched the
surface here.  If you are interested in finding out more, you might
like to look at sections 3 and 7 of the following article, which
concerns the monadic nature of [functional parsers][3].  For a more
in-depth exploration of the IO monad, see Simon Peyton Jones'
excellent article on the ["awkward squad"][2].

[1]: http://en.wikipedia.org/wiki/Gofer_(software) "Gofer Language"
[2]: http://research.microsoft.com/Users/simonpj/papers/marktoberdorf/ "Awkward Squad"
[3]: http://www.cs.nott.ac.uk/~gmh/monparsing.pdf "Functional Parsers"
[4]: http://book.realworldhaskell.org/read/monads.html#monads.do
[5]: http://donsbot.wordpress.com/2007/03/10/practical-haskell-shell-scripting-with-error-handling-and-privilege-separation/
