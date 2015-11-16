Introduction to Monads
======================

> {-# LANGUAGE NoImplicitPrelude, KindSignatures #-}
> module Monads where
> import Prelude hiding (filter,(>>))
> import Data.Char (toUpper)
> import Control.Monad (guard)



Quiz
----

Consider the definition of trees with values at their *leaves*.

> data Tree a = Leaf a | Branch (Tree a) (Tree a)
>    deriving (Eq, Show)

Define the following function that combines together the data stored
in the tree. 


> -- | zip two trees together
> zipTree :: Tree a -> Tree b -> Tree (a,b)
> zipTree = undefined

        o                     o                      o
       / \                   / \                   /   \
     "a"  o        ===>     0   o        ===>  ("a",0)  o
         / \                   / \                     / \
       "b" "c"                1   2             ("b",1)  ("c", 2)

> testZip :: Bool
> testZip = 
>   zipTree (Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c")))
>           (Branch (Leaf 0  ) (Branch (Leaf 1  ) (Leaf 2  )))
>   ==
>   (Branch (Leaf ("a",0)) (Branch (Leaf ("b",1)) (Leaf ("c",2))))














ANNOUNCEMENTS
=============

* HW #5 available   (due in one week)
* HW #3 peer review (due in one week)
* Project proposal  (due in two weeks, choose your own group)
* SCW at Grace Hopper Conference Wed-Sun
  No office hours Wed, moved to today
  Richard Eisenberg lecture Wed. 















Abstracting programming patterns
================================

Monads are an example of the idea of abstracting out a common programming
pattern as a generic mechanism.  Before considering monads, let us review this
idea, by thinking about error recovery.

How could we define zipTree so that we can recover from failure?

> zipTree1 :: Tree a -> Tree b -> Maybe (Tree (a,b))
> zipTree1 (Leaf a)     (Leaf b)       = undefined
> zipTree1 (Branch l r) (Branch l' r') = undefined
> zipTree1 _ _ = Nothing









Yuck! Can we do better? You bet!

Can you identify any patterns in the code?  

  * How do we *return* a value? Is there a common pattern?  

  * How do we *use* a value? Is there a common pattern? A helper 
    function that we can define to factor out the pattern 
    matching?







                 
Refactor!
---------

Use the new general definitions to refactor the code.

> zipTree2 :: Tree a -> Tree b -> Maybe (Tree (a,b))
> zipTree2 (Leaf a) (Leaf b) = undefined
> zipTree2 (Branch l r) (Branch l' r') = undefined
> zipTree2 _ _ = Nothing









Do notation
-----------

Haskell provides a special notation for expressions of the above
structure, allowing them to be written in a more appealing form:

~~~~~{.haskell}
do x1 <- m1
   x2 <- m2
   ...
   xn <- mn
   f x1 x2 ... xn
~~~~~

Hence, for example, our function can be redefined as...

> zipTree3 :: Tree a -> Tree b -> Maybe (Tree (a,b))
> zipTree3 (Leaf a)     (Leaf b)       = undefined
> zipTree3 (Branch l r) (Branch l' r') = undefined
> zipTree3 _ _ = Nothing






Monads in Haskell
=================

The `do` notation for sequencing is not specific to the `Maybe` type,
but can be used with any type that forms a *monad* -- in other words,
any type that is an instance of the `Monad` type class. The general
concept comes from a branch of mathematics called category theory.  In
Haskell, however, a monad is simply a parameterised type `m`, together
with two functions of the following types:

~~~~~{.haskell}
return :: a -> m a
(>>=)  :: m a -> (a -> m b) -> m b
~~~~~

The notion of a monad can now be captured as follows:

~~~~~{.haskell}
class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
~~~~~

That is, a monad is a parameterised type `m` that supports `return`
and `>>=` functions of the specified types.  The fact that `m` must
be a parameterised type, rather than just a type, is inferred from its
use in the types for the two functions.   

(To form a well-defined monad, the two functions are also required to
satisfy some simple properties; we will return to these later.)


It is now straightforward to make `Maybe` into a monadic type:

~~~~~{.haskell}
instance Monad Maybe where
   -- return      :: a -> Maybe a
   return x       =  Just x

   -- (>>=)       :: Maybe a -> (a -> Maybe b) -> Maybe b
   Nothing  >>= _ =  Nothing
   (Just x) >>= f =  f x
~~~~~

(*Aside*: types are not permitted in instance declarations, but we
include them as comments for reference.)  

It is because of this declaration that the `do` notation can be used
to sequence `Maybe` values.  More generally, Haskell supports the use
of this notation with any monadic type.  In the next few sections we
give some further examples of types that are monadic, and the benefits
that result from recognising and exploiting this fact.





Unpacking the do notation
=========================

So what does the Monad type class have to do with the `do` notation? 
Well, remember code that we so nicely wrote in this form:

~~~~~{.haskell}
do x1 <- m1
   x2 <- m2
   ...
   xn <- mn
   f x1 x2 ... xn
~~~~~

Haskell automagically translated that code into the following
sequence of nested binds.

~~~~~{.haskell}
m1 >>= (\x1 ->
  m2 >>= (\x2 ->
     ...
      mn >>= (\xn ->
        f x1 x2 ... xn)...))
~~~~~

So much easier to read with the arrows flipped around, no? 





But wait, there's more.  Sometimes with do notation, we see a line in
the middle that *doesn't* bind a variable.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
        main = do 
          x <- doSomething
          doSomethingElse     -- what is going on here?
          y <- andSoOn
          f x y
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In fact, we saw this sort of thing when using the `IO` monad. (Yep,
the `IO` type constructor is an instance of the `Monad` type class.
Under the covers, it is all binds and returns.)

> main :: IO ()
> main = do
>    putStrLn "This is the Classes lecture. What is your name?"
>    inpStr <- getLine
>    putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
>    return ()

Sometimes we don't need to use the result of a computation -- in
particular, if the computation has type `IO ()` then the result is
trivial. However, we still want to use bind for it's sequencing
capabilities... 

That brings us to a derived monad operator, called "sequence":

> (>>)  :: Monad m => m a -> m b -> m b
> m1 >> m2 = undefined


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
       doSomething >>= ( \x -> 
         doSomethingElse >>         -- it was just a sequence
           (andSoOn >>= ( \y -> 
             f x y)))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



> whatDoesThisDo :: IO ()
> whatDoesThisDo = foldr (>>) (putStrLn " Batman!")
>            (replicate 10 (putStr (show (0.0/0.0))))


Related Class: Applicative Functors
===================================

Let's take a look at the documentation for the [Monad
Class](https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Monad.html).

A recent change to Haskell's core library design is that the class
`Applicative` is a superclass of `Monad`. Any type constructor, like 
`Maybe` that is an instance of `Monad` must also be an instance of 
`Applicative`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
      class Applicative m => Monad m where
          ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

What is an `Applicative`? It is a functor with 
extra features. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
      class Functor f => Applicative f where
          pure   :: a -> f a
          (<*>)  :: f (a -> b) -> f a -> f b 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In otherwords, it is a functor with application, providing operations to embed
pure expressions (`pure`), and sequence computations and combine their results
(`<*>`).

Like `Functor`, this class captures useful functions for working with data
structures. Let's look at the `Applicative` instance for `Maybe`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
   instance Applicative Maybe where
      pure = Just

      Just f  <*> m      = fmap f m
      Nothing <*> _      = Nothing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that this superclass constraint means that any monads that you define
must also have `Applicative` and `Functor` instances.  However, this is not
much of a burden: there is a straightforward definition of `Functor` and
`Applicative` functions in terms of `return` and `(>>=)`. 

> monad_fmap :: (Monad m) => (a -> b) -> m a -> m b
> monad_fmap = undefined

> monad_pure :: (Monad m) => a -> m a 
> monad_pure = undefined

> monad_zap  :: (Monad m) => m (a -> b) -> m a -> m b
> monad_zap  = undefined

Note that `monad_fmap` is called `liftM` and `monad_zap` is called `ap` in the
Control.Monad library.

With these functions it is trivial to construct instances of Functor and
Applicative given an instance of Monad.  Of course, you may not want to always
do that: sometimes a specialized definition will be more efficient.









The List Monad
==============


The `Maybe` monad provides a simple model of computations that can
fail, in the sense that a value of type `Maybe a` is either `Nothing`,
which we can think of as representing failure, or has the form
`Just x` for some `x` of type `a`, which we can think of as success.

The list monad generalises this notion by permitting _multiple_
results in the case of success.  More precisely, a value of `[a]` is
either the empty list `[]`, which we can think of as failure, or a
non-empty list `[x1,x2,...,xn]`, which we can think of as a list of
successes.



A challenge
-----------

Write the function that takes each possible value `x` from the list
`xs`, and each possible value `y` from the list `ys` and returns a
list of the `(x,y)` pairs.

> pairs :: [Int] -> [Int] -> [(Int,Int)]
> pairs xs ys = undefined

                               
> testPairs = pairs1 [1,2,3,4] [5,6,7,8]








Can we divide this up? What are the patterns here?

We have

     concatMap (\x ->  do something with x) xs
               
     concatMap (\y ->  do something with y) ys

Generalize:

     concatMap f xs
              
Rejigger a bit to match the pattern above:

     (>>=) :: [a] -> (a -> [b]) -> [b]
     xs >>= f = concatMap f xs

What could `return` be?


















     return :: a -> [a]
     return x = [x]




Rewrite `pairs` using >>= and return

> pairs1 :: [Int] -> [Int] -> [(Int,Int)]
> pairs1 xs ys = undefined




Rewrite again using do notation

> pairs2 :: [Int] -> [Int] -> [(Int,Int)]
> pairs2 xs ys = undefined


Make sure that it still works.

> testPairs2 = pairs1 [1,2,3,4] [5,6,7,8]



Making lists into a `Monad` instance:

~~~~~{.haskell}
    instance Monad [] where
       -- return :: a -> [a]
       return x  =  [x]

       -- (>>=)  :: [a] -> (a -> [b]) -> [b]
       xs >>= f  =  concatMap f xs
~~~~~

(*Aside*: in this context, `[]` denotes the list type `[a]` without
its parameter.  That is, )  

So `return` simply converts a value into a (singleton) successful
result, while `>>=` provides a means of sequencing computations that
may produce multiple results: `xs >>= f` applies the function `f` to
each of the values in `xs` to give a list of lists of results, which
is then concatenated to give a single list of results.





List comprehensions
-------------------

It is interesting to note the similarity to how this function would be
defined using the list comprehension notation:

> pairs3 xs ys = [ (x,y) | x <- xs, y <- ys ]


What are some other examples that can be written using list comprehension?


* Rewrite the `map` function using a list comprehension.

> map' f xs = undefined


* Create a list of all pairs where the first component is less than the second.

> pairs4 xs ys = undefined





alternatively, using do notation


> pairs4' xs ys = undefined


alternatively, using `guard` (which requires a bit more from the monad)

> pairs4'' xs ys = undefined



* Rewrite `filter`, using a guarded list comprehension.

> filter :: (a -> Bool) -> [a] -> [a]
> filter f xs = undefined




* Remember quicksort?

> quicksort [] = []
> quicksort (x:xs) = undefined



In fact, there is a formal connection between the `do` notation and
the comprehension notation.  Both are simply different shorthands for
repeated use of the `>>=` operator for lists.  Indeed, the language
*Gofer*, one of the precursors to Haskell, permitted the comprehension
notation to be used with *any* monad.  For simplicity, Haskell only
allows comprehension to be used with lists.








Programming Over an Arbitrary Monad
===================================

An important benefit of abstracting out the notion of a monad into a
single typeclass is that it then becomes possible to define a number
of useful functions that work in an arbitrary monad.

We've already seen this in the `pairs` function

> 
> genpairs xs ys = do
>   x <- xs
>   y <- ys
>   return (x, y)
> 

What do you think the type of the above is? 

~~~~~{.haskell}
ghci> :type genpairs
~~~~~






It takes two monadic values and returns a single *paired* monadic
value.  Be careful though! The function above will behave differently
depending on what specific monad instance it is used with! If we use
the `Maybe` monad...

~~~~~{.haskell}
ghci> pairs (Nothing) (Just 'a')
ghci> pairs (Just 42) (Nothing)
ghci> pairs (Just 2) (Just 'a')
~~~~~

This generalizes to the list monad (the one for which we designed
`pairs`)...

~~~~~{.haskell}
ghci> pairs [] ['a'] 
ghci> pairs [42] []
ghci> pairs [2] ['a'] 
ghci> pairs [1,2] "ab" 
~~~~~

However, the behavior is quite different with the `IO` monad:

~~~~~{.haskell}
ghci> pairs getChar getChar
~~~~~

> myval = genpairs getChar getChar
             

Other common generic operations can be adapted for monadic programming.  For
example, we have seen above that the `map` function on lists can be
generalized as follows:

> liftM :: Monad m => (a -> b) -> m a -> m b
> liftM = undefined





























Similarly, `concat` on lists generalizes to:

> join    :: Monad m => m (m a) -> m a
> join mmx = undefined


















As a final example, we can define a function that transforms
a list of monadic expressions into a single such expression that
returns a list of results, by performing each of the argument
expressions in sequence and collecting their results:

> sequence  :: Monad m => [m a] -> m [a]
> sequence  = undefined










See the library
[Control.Monad](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Monad.html)
for *many* more general purpose monad operations.


The Monad Laws
==============

Earlier we mentioned that the notion of a monad requires that the
`return` and `>>=` operators satisfy some simple properties.  

The first two properties concern the link between `return` and `>>=`:

~~~~~{.haskell}

   (return x) >>= f  =  f x    --   (1)

   mx >>= return     =  mx     --   (2)

~~~~~

Intuitively, equation (1) states that if we return a value `x` and
then feed this value into a function `f`, this should give the same
result as simply applying `f` to `x`.  Conversely, equation (2) states
that if we feed the results of a computation `mx` into the function
return, this should give the same result as simply performing `mx`.
Together, these equations express -- modulo the fact that the second
argument to `>>=` involves a binding operation -- that `return` is a
left and right identity for `>>=`.

The third and final property expresses (again modulo binding) that
`>>=` is associative:

~~~~~{.haskell} 

   (mx >>= f) >>= g  =  mx >>= (\x -> (f x >>= g)) 	-- (3)

~~~~~

Note that we cannot simply write `mx >>= (f >>= g)` on the right-hand
side of this equation, as this would not be type correct.


It is instructive to rewrite these laws in terms of the `do` notation... 



~~~~~{.haskell} 

   (return x) >>= (\y -> f y) =  f x    --   (1)

      do y <- return x     =    f x
         f y

   mx >>= (\x -> return x)     =  mx     --   (2)

      do x <- mx         =    mx
         return x

   (mx >>= f) >>= (\y -> g y)  =  mx >>= (\x -> (f x >>= (\y -> g y))) 	-- (3)

      do y <- do z <- mx       =  do x<- mx
                 f z                 y <- fx
         g y                         g y

~~~~~



As an example of the utility of the monad laws, let us see how they
can be used to prove a useful property of the `liftM` function above,
namely that it distributes over the composition operator for
functions, in the sense that:

~~~~~{.haskell}

   liftM (f . g)  =  liftM f . liftM g

~~~~~

This equation generalizes the familiar distribution property of
map from lists to an arbitrary monad.  In order to verify this
equation, we first rewrite the definition of `liftM` using `>>=`:

~~~~~{.haskell}

   liftM f mx  =  mx >>= \x -> return (f x)

~~~~~

Now the distribution property can be verified as follows:

~~~~~{.haskell}

   (liftM f . liftM g) mx 

        = ...

        = liftM (f . g) mx

~~~~~


Exercise
--------

Show that the `Maybe` monad satisfies equations (1), (2) and (3).





Credit
------

This lecture draws on lecture notes by [Graham Hutton][0] and [John
Hughes][1].


[0]: http://www.cs.nott.ac.uk/~gmh/monads
[1]: http://www.cse.chalmers.se/~rjmh/OPLSS/

