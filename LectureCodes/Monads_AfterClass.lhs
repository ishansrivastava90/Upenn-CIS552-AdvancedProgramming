Introduction to Monads
======================

> {-# LANGUAGE NoImplicitPrelude, KindSignatures #-}
> module Monads where
> import Prelude hiding (filter,(>>),(<$>))
> import Data.Char (toUpper)
> import Control.Applicative ( Alternative(..), Applicative(..) )
> import Test.QuickCheck
> import Control.Monad ( MonadPlus )

Quiz
----

Consider the definition of trees with values at their *leaves*.

> data Tree a = Leaf a | Branch (Tree a) (Tree a)
>    deriving (Eq, Show)

Define the following function that combines together the data stored
in the tree.


> -- | zip two trees together
> zipTree :: Tree a -> Tree b -> Tree (a,b)
> zipTree (Leaf a) (Leaf b) = Leaf (a,b)
> zipTree (Branch a1 a2) (Branch b1 b2) = Branch (zipTree a1 b1) (zipTree a2 b2)
> zipTree (Leaf a) (Branch b1 b2) = error "zip mismatch"
> zipTree (Branch a1 a2) (Leaf b) = error "zip mismatch"

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
> zipTree1 (Leaf a)     (Leaf b)       = Just (Leaf (a,b))
> zipTree1 (Branch l r) (Branch l' r') =
>   case zipTree1 l l' of
>      Just l'' -> case zipTree1 r r' of
>                     Just r'' -> Just (Branch l'' r'')
>                     Nothing  -> Nothing
>      Nothing -> Nothing
> zipTree1 _ _ = Nothing


Yuck! Can we do better? You bet!

Can you identify any patterns in the code?

  * How do we *return* a value? Is there a common pattern?

  * How do we *use* a value? Is there a common pattern? A helper
    function that we can define to factor out the pattern
    matching?
 

Suggested helper functions from class:

> combineJust :: Maybe a -> Maybe b -> (a -> b -> c) -> Maybe c
> combineJust (Just x) (Just y) f = Just (f x y)
> combineJust _ _ _ = Nothing

> {-
> funct :: Maybe a -> (a -> Maybe b) -> Maybe b
> funct Nothing _ = Nothing
> funct (Just x) f = (f x)

> return :: a -> Maybe a
> return = Just
> -}


 


Look closely at the program
---------------------------

    zipTree :: Tree a -> Tree b -> Maybe (Tree (a,b))
    zipTree (Leaf a) (Leaf b) =
        Just (Leaf (a,b))   <-------------------- this is how we
    zipTree (Branch l r) (Branch l' r') =          return a value
       ~~~~~~~~~~~~~~~~~~~~~~                              |
       case zipTree l l' of                                |
         Nothing -> Nothing                                |
         Just l'' ->           <------- This is how we     |
       ~~~~~~~~~~~~~~~~~~~~             use a value        |
           case zipTree r r' of         |                  |
             Nothing -> Nothing   <-----|                  |
             Just r'' ->                                   |
            ~~~~~~~~~~~~~~~~~~~~~~~                        |
                Just (Branch l'' r'')   <------------------|
    zipTree _ _ = Nothing


 
Common parts of the definition
------------------------------

For *returning* a value we have:

      Just (Leaf(a,b))
      Just (Branch l'' r'')
 
Abstract to general pattern:
 
      Just x
 
Give the pattern a good name:
 
      return :: a -> Maybe a
      return x = Just x
 
For *using* a value we have:
 
      case zipTree l l' of
        Nothing -> Nothing
        Just l'' -> ...
            do something with l''
 
      case zipTree r r' of
        Nothing -> Nothing
        Just r'' -> ...
            do something with r''

General pattern:

      case x of
         Nothing -> Nothing
         Just y -> f y

Name that pattern ("bind") or ("use x in f"):

     (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
     x >>= f =
       case x of
         Nothing -> Nothing
         Just y  -> f y
 
 
That is, if the first argument is `Nothing` then the second argument
is ignored and `Nothing` is returned as the result.  Otherwise, if
the first argument is of the form `Just x`, then the second argument
is applied to `x` to give a result of type `Maybe b`.





 
Refactor!
---------

Use the new general definitions to refactor the code.

> zipTree2 :: Tree a -> Tree b -> Maybe (Tree (a,b))
> zipTree2 (Leaf a) (Leaf b) = return (Leaf (a,b))
> zipTree2 (Branch l r) (Branch l' r') =
>  ( zipTree2 l l') >>= (\ l'' ->
>    ( zipTree2 r r' ) >>= (\ r'' ->
>      return (Branch l'' r'')))
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
> zipTree3 (Leaf a)     (Leaf b)       = return (Leaf (a,b))
> zipTree3 (Branch l r) (Branch l' r') = do
>   l'' <- zipTree3 l l'
>   r'' <- zipTree3 r r'
>   return (Branch l'' r'')

> {-
>  ( zipTree3 l l') >>= (\ l'' ->
>    ( zipTree3 r r' ) >>= (\ r'' ->
>      return (Branch l'' r'')))
> -}

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


> seqMaybe :: Maybe a -> Maybe b -> Maybe b
> seqMaybe (Just _) x = x
> seqMaybe Nothing  x = Nothing

That brings us to a derived monad operator, called "sequence":

> (>>)  :: Monad m => m a -> m b -> m b
> m1 >> m2 = m1 >>= (\ _ -> m2)


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


> -- instance Functor Maybe where
>  -- fmap :: (a -> b) -> Maybe a -> Maybe b
> -- fmap _ Nothing  = Nothing
> -- fmap f (Just x) = Just (f x)

Note that this superclass constraint means that any monads that you define
must also have `Applicative` and `Functor` instances.  However, this is not
much of a burden: there is a straightforward definition of `Functor` and
`Applicative` functions in terms of `return` and `(>>=)`.

> monad_fmap :: (Monad m) => (a -> b) -> m a -> m b
> monad_fmap f mx = do     --- liftM ---
>                   x <- mx
>                   return (f x)

> monad_pure :: (Monad m) => a -> m a
> monad_pure x = return x

> monad_zap  :: (Monad m) => m (a -> b) -> m a -> m b
> monad_zap  mf mx = do         --- ap ---
>                    f <- mf
>                    x <- mx
>                    return (f x)

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
> pairs xs ys = concatMap (\x -> concatMap (\y -> [(x,y)]) ys) xs


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
> pairs1 xs ys = xs >>= \x -> 
>                ys >>= \y ->
>                return (x,y)




Rewrite again using do notation

> pairs2 :: [Int] -> [Int] -> [(Int,Int)]
> pairs2 xs ys = do x <- xs
>                   y <- ys
>                   return (x,y)
            


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
its parameter.  That is, the notation `[a]` in a type is just syntactic
sugar for `[] a`. Try the latter in your code. It works!)

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

> map' f xs = [f x | x <- xs]


* Create a list of all pairs where the first component is less than the second.

> pairs4 xs ys = [(x,y) | x <- xs, y <- ys, x < y]





alternatively, using do notation


> pairs4' xs ys = do
>                 x <- xs
>                 y <- ys
>                 if (x < y) 
>                  then return (x,y)
>                  else []




* Rewrite `filter`, using a guarded list comprehension.

> filter :: (a -> Bool) -> [a] -> [a]
> filter f xs = [x | x <- xs, f x]




* Remember quicksort?

> quicksort [] = []
> quicksort (x:xs) = quicksort [x' | x' <- xs, x' < x] ++ [x]
>                 ++ quicksort [x' | x' <- xs, x' >= x]



In fact, there is a formal connection between the `do` notation and
the comprehension notation.  Both are simply different shorthands for
repeated use of the `>>=` operator for lists.  Indeed, the language
*Gofer*, one of the precursors to Haskell, permitted the comprehension
notation to be used with *any* monad.  For simplicity, Haskell only
allows comprehension to be used with lists.



The Monad Menagerie
===================

We have now seen several type classes related to `Monad`. Let's summarize
all of this and finish filling out the monad menagerie, where arrows denote
superclass relationships:

          Functor
             ^
             |
          Applicative  <-- Alternative
             ^               ^
             |               |
          Monad  <-------- MonadPlus


And here are the Haskell definitions, all in one place. (The library versions
of these include extra methods, but they all have sensible defaults and you
don't have to worry about them.)

~~~~~{.haskell}
class Functor m where
  fmap :: (a -> b) -> m a -> m b

class Functor m => Applicative m where
  pure  :: a -> m a
  (<*>) :: m (a -> b) -> m a -> m b

class Applicative m => Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

class Applicative m => Alternative m where           -- in Control.Applicative
  empty :: m a
  (<|>) :: m a -> m a -> m a

class (Alternative m, Monad m) => MonadPlus m where  -- in Control.Monad
  mzero :: m a
  mplus :: m a -> m a -> m a
~~~~~

Let's examine the two new classes here, `Alternative` and `MonadPlus`. They
look very, very similar, don't they? And indeed they are, essentially, the
same. We'll focus on `Alternative`, but everything we learn about `Alternative`
is true for `MonadPlus`, too. (See the "dirty truth" section below for why
both of these exist.)

The key to `Alternative` is the `(<|>)` operator, which combines two
results. The `empty` function is just the identity for `(<|>)`. That is,
`empty <|> x` and `x <|> empty` are both always `x`. As an example, here
is the `Alternative` instance for `Maybe`:

~~~~~{.haskell}
instance Alternative Maybe where
  empty = Nothing

  Nothing <|> b = b
  a       <|> _ = a
~~~~~

From the viewpoint of monadic computations, `empty` represents failure and
`(<|>)` represents choosing the successful result(s) of two computations.
This viewpoint leads us to the following very useful library function:

> guard :: Alternative m => Bool -> m ()   -- also in Control.Monad
> guard True  = pure ()
> guard False = empty

This function checks a Boolean condition, either allowing the computation
to continue (`pure ()` is the same as `return ()`, which is an identity for
the `(>>=)` operation) or fail. `guard` is the monadic equivalent of a guard
in a list comprehension. We can now thus write `pairs4` monadically, using
`guard`. (Recall that `pairs4` produces a list of all pairs of elements of
`xs` and `ys` such that the first element is less than the second.)

> pairs4'' xs ys = do
>                 x <- xs
>                 y <- ys
>                 guard (x < y)
>                 return (x,y)



The dirty truth about these classes
-----------------------------------

The designers of Haskell have made a terrible mistake.

Happily, the mistake is now fixed. The mistake was that they never thought
of `Functor` or `Applicative` when designing the language. Because `Monad`
historically came first, it didn't have `Functor` or `Applicative` as
superclasses. This was very annoying, because every type that has a
`Monad` instance also has a sensible `Functor` and `Applicative` instance.
Yet lots of library functions had to be written twice, once for monads and
once for applicative functors. For GHC 7.10, this was fixed, by making
`Applicative` a superclass of `Monad` (and breaking a lot of code in the
process). Despite having fixed this, there are still lots of vestiges
of this historical accident littered throughout the standard library.

The existence of both `MonadPlus` and `Alternative` is one of these vestiges.
They are, essentially, the same class. Every `MonadPlus` instance should just
do the same thing as the `Alternative` instance. So don't worry too much about
any choice between these. Use `MonadPlus` if the thing is a monad, and
use `Alternative` if the thing is just an applicative functor but not a monad.
(There are surprisingly few structures that are applicative functors but not
monads.)

And, if you're perusing the standard library and see silly duplication between
`Applicative` stuff and `Monad` stuff, now you know why.


The non-determinism monad
-------------------------

Equipped with `MonadPlus`, we are now ready to understand the list monad
as a *non-determinism monad*, where you can view the list of results as
a set of possibilities, of which some are chosen non-deterministically.
You can see how the definition of `pairs4''` above makes sense under this view.

For example, suppose we are given a list of integers and we wish to extract
pairs from this list where the first number divides evenly into the second.
Here is the algorithm: non-deterministically choose two numbers from the input
and check that the required property holds. Simple!

> dividingPairs :: [Integer] -> [(Integer, Integer)]
> dividingPairs xs = do 
>                    x1 <- xs
>                    x2 <- xs   
>                    guard (x1 /= 0 && x2 `mod` x1 == 0)
>                    return (x1,x2)

> dividingPairs' :: [Integer] -> [(Integer, Integer)]
> dividingPairs' xs = [(x1,x2) | x1 <- xs,
>                                x2 <- xs,
>                                x1 /= 0,
>                                x2 `mod` x1 == 0]


> prop_dividingPairs :: [Integer] -> Bool
> prop_dividingPairs xs = all (\(x,y) -> y `mod` x == 0) (dividingPairs xs)
>
> checkDividingPairs :: IO ()
> checkDividingPairs = quickCheck prop_dividingPairs

This `dividingPairs` is very similar in structure to `pairs4''`. Write a more
general version of these functions, working over an arbitrary `MonadPlus` and
condition function:

> -- choosePairs :: {- write a type here -}
> --choosePairs :: (Alternative m , Monad m) => (a -> b -> Bool) -> m a -> m b -> m (a,b)
> choosePairs :: (Alternative m, MonadPlus m) => (a -> b -> Bool) -> m a -> m b -> m (a,b)
> choosePairs f xs ys = do 
>                       x <- xs
>                       y <- ys
>                       guard (f x y)
>                       return (x,y)
                


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
> liftM f mx = do 
>               x <- mx
>               return (f x)


Similarly, `concat` on lists generalizes to:

Return is just a function, not a key word as in Java. 
It just wraps in a monad in Haskell

> join    :: Monad m => m (m a) -> m a
> join mmx = do mx <- mmx
>               mx



As a final example, we can define a function that transforms
a list of monadic expressions into a single such expression that
returns a list of results, by performing each of the argument
expressions in sequence and collecting their results:

> 
> --sequence  :: Monad m => [m a] -> m [a]
> --sequence (mx:mxs) = do
> --                    x  <- mx
> --                    xs <- sequence mxs
> --                    return ([x] ++ xs) -}











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





`Applicative` widgets
=====================

Here is a useful combinator, an infix version of `fmap`:

> (<$>) :: Functor f => (a -> b) -> f a -> f b
> (<$>) = fmap

Using `(<$>)` and `(<*>)` allows you to write certain monadic code in
a different idiomatic style (called `Applicative` style).

Recall the `zipTree` example from the top of this file. Using the
so-called `Applicative` widgets, this function can be written like this

> zipTree4 :: Tree a -> Tree b -> Maybe (Tree (a,b))
> zipTree4 (Leaf a)       (Leaf b)       = return (Leaf (a,b))
> zipTree4 (Branch a1 a2) (Branch b1 b2) = Branch <$> zipTree4 a1 b1
>                                                 <*> zipTree4 a2 b2
> zipTree4 _              _              = Nothing

Take a close look at the types to figure out how this all works out.
Happily, it's mechanical to figure out which widget you need: start
with a pure (non-monadic) function (like `Branch`), and then use
`(<$>)` for the first monadic argument. After that, separate all
arguments with `(<*>)`. Easy, and more concise than `do` in many
circumstances.


Credit
------

This lecture draws on lecture notes by [Graham Hutton][0] and [John
Hughes][1].


[0]: http://www.cs.nott.ac.uk/~gmh/monads
[1]: http://www.cse.chalmers.se/~rjmh/OPLSS/
