Higher-Order Programming Patterns
=================================

> {-# OPTIONS_GHC -fno-warn-type-defaults #-}
> module Lec3 where

> import Prelude hiding (map, foldr, filter, pred, sum, product)
> import Data.Char
> import Test.HUnit



Polymorphic Data Structures
---------------------------

Polymorphic functions that can *operate* on different kinds of values are
often associated with polymorphic data structures that can *contain*
different kinds of values.  The types of such functions and data structures
are written with one or more type variables.

For example, the list length function:

> len :: [a] -> Int
> len []     = 0
> len (_:xs) = 1 + len xs

The function's type states that we can invoke `len` on any kind of list.
The type variable `a` is a placeholder that is replaced with the actual type
of the list elements at different application sites.  Thus, in the following
applications of `len`, `a` is replaced with `Double`, `Char` and `[Int]`
respectively.

     len [1.1, 2.2, 3.3, 4.4] :: Int
        
     len "mmm donuts!"  :: Int

     len [[], [1], [1,2], [1,2,3]] :: Int

Most of the standard list manipulating functions, for example those in the
standard library [Data.List][1], have generic types.  With a little
practice, you'll find that the type signature contains a surprising amount
of information about how the function behaves.

    (++) :: [a] -> [a] -> [a]
    
    head :: [a] -> a
    
    tail :: [a] -> [a]





"Bottling" Computation Patterns With Polymorphic Higher-Order Functions
=======================================================================

The combination of polymorphism and higher-order functions is the secret
sauce that makes FP so tasty.  It allows us to take *patterns of
computation* that reappear in different guises in different places, and
crisply specify them as reusable strategies.  Let's look at some concrete
examples...

Computation Pattern: Iteration
------------------------------

Let's write a function that converts a string to uppercase.  Recall that, in
Haskell, a `String` is nothing but a list of `Char`s.  So we must start with
a function that will convert an individual `Char` to its uppercase
version. Once we find this function, we will simply *walk over the list*,
and apply the function to each `Char`.

How might we find such a transformer?  Lets query [Hoogle][2] for a function
of the appropriate type!  Ah, we see that the module `Data.Char` contains
such a function:

     toUpper :: Char -> Char

Using this, we can write a simple recursive function that does what we need:

> toUpperString :: String -> String
> toUpperString = undefined




This pattern of of recursion appears all over the place.  For example,
suppose we represent a location on the plane using a pair of `Double`s (for
the x- and y- coordinates) and we have a list of points that represent a
polygon.

> type XY      = (Double, Double)
> type Polygon = [XY]

It's easy to write a function that *shifts* a point by a specific amount:

> shiftXY :: XY -> XY -> XY
> shiftXY (dx, dy) (x, y) = undefined

How would we translate a polygon?  Just walk over all the points in
the polygon and translate them individually.

> shiftPoly :: XY -> Polygon -> Polygon
> shiftPoly _ []       = []
> shiftPoly d (xy:xys) = undefined

Now, some people (using some languages) might be quite happy with the above
code. But what separates a good programmer from a great one is the ability
to *abstract*.

The functions `toUpperString` and `shiftPoly` share the same computational
structure: they walk over a list and apply a function to each element.  We
can abstract this common pattern out as a higher-order function, `map`.
Since the two functions we're abstracting differ only in what they do to
each list element, so we'll just take that as an input!

> map = undefined





The type of `map` tells us exactly what it does: it takes an `a -> b`
transformer and list of `a` values, and transforms each `a` value to return
a list of `b` values.  We can now safely reuse the pattern, by
*instantiating* the transformer with different specific operations.

> toUpperString' :: String -> String
> toUpperString' = undefined

> shiftPoly' :: XY -> Polygon -> Polygon
> shiftPoly' d = undefined



Much better.  But let's make sure our refactoring didn't break anything!

> testMap = runTestTT $ TestList $ 
>   [     toUpperString' "abc" 
>     ~?= toUpperString "abc"
>   , 
>         shiftPoly' (0.5,0.5) [(1,1),(2,2),(3,3)] 
>     ~?= shiftPoly (0.5,0.5) [(1,1),(2,2),(3,3)] ]  


By the way, what happened to the list parameters of `toUpperString`
and `shiftPoly`?  Two words: *partial application*.  In general, in
Haskell, a function definition equation

    f x = e x

is identical to 

    f = e

as long as `x` isn't used in `e`.  Thus, to save ourselves the
trouble of typing, and the blight of seeing the vestigial `x`, we
often prefer to just leave it out altogether.

(As an exercise, you may like to prove to yourself using just equational
reasoning, using the equality laws we have seen, that the above versions of
`toUpperString` and `shiftPoly` are equivalent.)



We've already seen a few other examples of the map pattern.  Recall the
`listIncr` function, which added 1 to each element of a list:

> listIncr :: [Int] -> [Int]
> listIncr []     = []
> listIncr (x:xs) = (x+1) : listIncr xs

We can write this more cleanly with map, of course:

> listIncr' :: [Int] -> [Int]
> listIncr' = undefined



Computation Pattern: Folding 
----------------------------

Once you've put on the FP goggles, you start seeing a handful of computation
patterns popping up everywhere.  Here's another...


Lets write a function that *adds* all the elements of a list.

> sum :: [Int] -> Int
> sum = undefined

Next, a function that *multiplies* the elements of a list.

> product :: [Int] -> Int
> product = undefined 

Can you see the pattern?  Again, the only bits that are different are
the `base` case value, and the function being used to combine the list
element with the recursive result at each step.  We'll just turn those
into parameters, and lo!

> foldr = undefined

Now, each of the individual functions are just specific instances of the 
general `foldr` pattern.

> sum', product' :: [Int] -> Int
> sum'     = undefined
> product' = undefined

> foldrTest = runTestTT $ TestList [
>               sum' [1,2,3] ~?= sum [1,2,3],
>               product' [1,2,3] ~?= product [1,2,3] 
>             ]
                           

To develop some intuition about `foldr` let's unfold an example a few
times by hand.

~~~~~
foldr f base [x1,x2,...,xn] 

  == f x1 (foldr f base [x2,...,xn])           {- unfold -} 

  == f x1 (f x2 (foldr f base [...,xn]))       {- unfold -} 

  == f x1 (f x2 (... (f xn base)))             {- unfold -} 

~~~~~

Aha!  It has a rather pleasing structure that mirrors that of lists;
the `:` is replaced by the `f` and the `[]` is replaced by `base`.  So
can you see how to use it to eliminate recursion from the recursion
from our list-length function?

    len :: [a] -> Int
    len []     = 0
    len (x:xs) = 1 + len xs

> len' :: [a] -> Int
> len' = undefined


Or, how would you use foldr to eliminate the recursion from this?

> factorial :: Int -> Int
> factorial 0 = 1
> factorial n = n * factorial (n-1)

> factorial' :: Int -> Int
> factorial' n = undefined




OK, one more.  The standard list library function `filter` has this
type:

> filter :: (a -> Bool) -> [a] -> [a]

The idea is that it the output list should contain only the elements
of the first list for which the input function returns `True`.

So:

> filterTests :: Test
> filterTests = TestList 
>      [ filter (>10) [1..20] ~?= [11..20],
>        filter (\l -> sum l <= 42) [ [10,20], [50,50], [1..5] ]
>          ~?= [[10,20],[1..5]] ]

Can we implement filter using foldr?  Sure!

> filter pred = undefined



Which is more readable? HOFs or Recursion
-----------------------------------------

As a beginner, you might find the explicitly recursive versions of
some of these functions easier to follow than the `map` and `foldr`
versions.  However, as you write more Haskell, you will probably start
to find the latter are far easier, because `map` and `foldr`
encapsulate such common patterns that you'll become completely
accustomed to thinking in terms of them and other similar
abstractions.

In contrast, explicitly writing out the recursive pattern matching
should start to feel needlessly low-level. Every time you see a
recursive function, you have to understand how the knots are tied --
and worse, there is potential for making silly off-by-one type errors
if you re-jigger the basic strategy every time.

As an added bonus, it can be quite useful and profitable to
*parallelize* and *distribute* the computation patterns (like `map`
and `foldr`) in just one place, thereby allowing arbitrary hundreds or
thousands of instances to [benefit in a single shot!][3].

We'll see some other similar patterns later on.



Spotting Patterns In The "Real" World
=====================================

It was all well and good to see the patterns in tiny toy functions.
Needless to say, these patterns appear regularly in real code, if only
you know to look for them.  We didn't get to this in class, but for
additional practice you can look at the [extended
example](SecretCode.html) on your own.

Acknowledgements: This lecture is based on notes by Ranjit Jhala, Winter 2011

[1]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html "Data.List"
[2]: http://haskell.org/hoogle "Hoogle Query: Char -> Char"
[3]: http://en.wikipedia.org/wiki/MapReduce "MapReduce"


> main :: IO ()
> main = putStrLn "This is Lec3"
