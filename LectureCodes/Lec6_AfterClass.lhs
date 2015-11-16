QuickCheck: Type-directed Property Testing
==========================================

Announcements:
* Peer reviews due tonight by midnight. Submit online.

* HW 3/4 available very soon.
   - Due two weeks from tomorrow.
   - Submit both together.
   - New random partners.

* Today's lecture is about bugfinding. There will be bugs in the examples,
  please do not blurt them out if you see them.


> module QuickCheck where

In this lecture, we will look at [QuickCheck][1], a technique that

cleverly exploits typeclasses and monads to deliver a powerful 
automatic testing methodology. 

Quickcheck was developed by [Koen Claessen][0] and [John Hughes][11]
more than ten years ago, and has since been ported to other languages
and is currently used, among other things to find subtle [concurrency
bugs][3] in [telecommunications code][4]. In 2010, it received the
[most influential paper award](http://www.sigplan.org/award-icfp.htm)
for the ICFP 2000 conference.

The key idea on which QuickCheck is founded is *property-based
testing*.  That is, instead of writing individual test cases (eg unit
tests corresponding to input-output pairs for particular functions)
one should write *properties* that are desired of the functions, and
then *automatically* generate *random* tests which can be run to
verify (or rather, falsify) the property.

By emphasizing the importance of specifications, QuickCheck yields 
several benefits:

1. The developer is forced to think about what the code *should do*,

2. The tool finds corner-cases where the specification is violated, 
   which leads to either the code or the specification getting fixed,

3. The specifications live on as rich, machine-checkable documentation
   about how the code should behave.

To use the QuickCheck library, you need to first install it with cabal.

      cabal install quickcheck

> import Test.QuickCheck (Arbitrary(..),Gen(..),Property(..),OrderedList(..),
>                         forAll,frequency,elements,sized,oneof,(==>),collect,
>                         quickCheck,sample,choose,quickCheckWith,
>                         classify,stdArgs,maxSuccess)
> import Control.Monad (liftM,liftM2,liftM3)
> import Data.List (sort,insert,nub)
> import Data.Maybe (fromMaybe)

> import Data.Map (Map)
> import qualified Data.Map as Map


Properties
==========

A QuickCheck property is essentially a function whose output is a
boolean.  A standard "hello-world" QC property might be something
about common functions on lists.

> prop_revapp :: [Int] -> [Int] -> Bool
> prop_revapp xs ys = reverse (xs ++ ys) == reverse xs ++ reverse ys


That is, a property looks a bit like a mathematical theorem that the
programmer believes is true. A QC convention is to use the prefix `"prop_"`
for QC properties. Note that the type signature for the property is not the 
usual polymorphic signature; we have given the concrete type `Int` for the
elements of the list. This is because QC uses the types to generate random
inputs, and hence is restricted to monomorphic properties (those that don't
contain type variables.)

To *check* a property, we simply invoke the `quickCheck` action with the 
property. Note that only certain types of properties can be tested, these 
properties are all in the 'Testable' type class. 

~~~~~{.haskell}
quickCheck :: (Testable prop) => prop -> IO ()
  	-- Defined in Test.QuickCheck.Test
~~~~~

`[Int] -> [Int] -> Bool` is a Testable property, so 
let's try quickCheck on our example property above

~~~~~{.haskell}
*Main> quickCheck prop_revapp 
~~~~~









What's that ?! Well, let's run the *property* function on the two inputs

~~~~~{.haskell}
*Main> prop_revapp [0] [1] 
~~~~~





QC has found a sample input for which the property function *fails* ie,
returns `False`. Of course, those of you who are paying attention will
realize there was a bug in our property, namely it should be

> prop_revapp_ok :: [Int] -> [Int] -> Bool
> prop_revapp_ok xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

because `reverse` will flip the order of the two parts `xs` and `ys` of 
`xs ++ ys`. Now, when we run 

~~~~~{.haskell}
*Main> quickCheck prop_revapp_ok
~~~~~






That is, Haskell generated 100 test inputs and for all of those, the
property held. You can up the stakes a bit by changing the number of tests
you want to run

> quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

and then do

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_revapp_ok
~~~~~







QuickCheck QuickSort
--------------------

Let's look at a slightly more interesting example. Here is the canonical 
implementation of *quicksort* in Haskell.

> qsort []     = []
> qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
> 
>   where lhs  = [y | y <- xs, y < x]   -- this is a "list comprehension"
>         
>         rhs  = [z | z <- xs, z > x]







Really doesn't need much explanation! Let's run it "by hand" on a few inputs

~~~~~{.haskell}
*Main> [10,9..1]

*Main> qsort [10,9..1]


*Main> [2,4..20] ++ [1,3..11]

*Main> qsort $ [2,4..20] ++ [1,3..11]

~~~~~

Looks good -- let's try to test that the output is in 
fact sorted. We need a function that checks that a 
list is ordered

> isOrdered :: Ord a => [a] -> Bool
> isOrdered [] = True
> isOrdered [x] = True
> isOrdered (x:y:xs) = x <= y && isOrdered (y : xs)
> 

and then we can use the above to write a property saying that the
result of qsort is an ordered list.

> prop_qsort_isOrdered :: [Int] -> Bool
> prop_qsort_isOrdered xs = isOrdered (qsort xs)

Let's test it!

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_qsort_isOrdered 
~~~~~




Conditional Properties
----------------------

Here are several other properties that we 
might want. First, repeated `qsorting` should not
change the list. That is, 

> prop_qsort_idemp ::  [Int] -> Bool 
> prop_qsort_idemp xs = qsort (qsort xs) == qsort xs





Second, the head of the result is the minimum element
of the input

> prop_qsort_min :: [Int] -> Bool
> prop_qsort_min xs = head (qsort xs) == minimum xs

~~~~~{.haskell}
*Main> quickCheck prop_qsort_min 
~~~~~








However, when we run this, we run into a glitch.

But of course! The earlier properties held *for all inputs*
while this property makes no sense if the input list is empty! 
This is why thinking about specifications and properties has the 
benefit of clarifying the *preconditions* under which a given 
piece of code is supposed to work. 

In this case we want a *conditional properties* where we only want 
the output to satisfy to satisfy the spec *if* the input meets the
precondition that it is non-empty.

> prop_qsort_nn_min    :: [Int] -> Property
> prop_qsort_nn_min xs = 
>   not (null xs) ==> head (qsort xs) == minimum xs
>
> prop_qsort_nn_max    :: [Int] -> Property
> prop_qsort_nn_max xs = 
>   not (null xs) ==> last (qsort xs) == maximum xs

We can write a similar property for the maximum element too. 

~~~~~{.haskell}
*Main> quickCheckN 100 prop_qsort_nn_min

*Main> quickCheckN 100 prop_qsort_nn_max
~~~~~


This time around, both the properties hold.

Note that now, instead of just being a `Bool` the output
of the function is a `Property` a special type built into 
the QC library. Similarly the *implies* combinator `==>` 
is one of many QC combinators that allow the construction 
of rich properties.


Testing Against a Model Implementation
--------------------------------------

We could keep writing different properties that capture 
various aspects of the desired functionality of `qsort`. 
Another approach for validation is to test that our `qsort` 
is *behaviorally* identical to a trusted *reference 
implementation* which itself may be too inefficient or 
otherwise unsuitable for deployment. In this case, let's 
use the standard library's `sort` function

> prop_qsort_sort    :: [Int] -> Bool
> prop_qsort_sort xs =  qsort xs == sort xs

which we can put to the test

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_qsort_sort
~~~~~






Say, what?!



~~~~~{.haskell}
*Main> qsort [-1,-1]
~~~~~

Ugh! So close, and yet ... Can you spot the bug in our code?

~~~~~{.haskell}
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where lhs  = [y | y <- xs, y < x]
        rhs  = [z | z <- xs, z > x]
~~~~~








We're assuming that the *only* occurrence of (the value) `x` 
is itself! That is, if there are any *copies* of `x` in the 
tail, they will not appear in either `lhs` or `rhs` and hence
they get thrown out of the output. 


Is this a bug in the code? What *is* a bug anyway? Perhaps the
fact that all duplicates are eliminated is a *feature*! At any 
rate there is an inconsistency between our mental model of how 
the code *should* behave as articulated in `prop_qsort_sort` 
and the actual behavior of the code itself.

We can rectify matters by stipulating that the `qsort` produces
lists of distinct elements

> isDistinct :: Eq a => [a] -> Bool
> isDistinct xs = nub xs == xs
>
> prop_qsort_distinct :: [Int] -> Bool 
> prop_qsort_distinct = isDistinct . qsort  

and then, weakening the equivalence to only hold on inputs that 
are duplicate-free 

> prop_qsort_distinct_sort :: [Int] -> Property
> prop_qsort_distinct_sort xs = 
>   isDistinct xs ==> qsort xs == sort xs

QuickCheck happily checks the modified properties

~~~~~{.haskell}
*Main> quickCheck prop_qsort_distinct

*Main> quickCheck prop_qsort_distinct_sort 

~~~~~


The Perils of Conditional Testing
---------------------------------

Well, we managed to *fix* the `qsort` property, but beware! Adding
preconditions leads one down a slippery slope. In fact, if we paid
closer attention to the above runs, we would notice something

~~~~~{.haskell}
*Main> quickCheckN 10000 prop_qsort_distinct_sort 
...
(5012 tests; 248 discarded)
...
+++ OK, passed 10000 tests.
~~~~~

The bit about some tests being *discarded* is ominous. In effect, 
when the property is constructed with the `==>` combinator, QC 
discards the randomly generated tests on which the precondition 
is false. In the above case QC grinds away on the remainder until 
it can meet its target of `10000` valid tests. This is because 
the probability of a randomly generated list meeting the precondition 
(having distinct elements) is high enough. This may not always be the case.

The following code is (a simplified version of) the `insert` function 
from the standard library 

~~~~~{.haskell}
insert x []                 = [x]
insert x (y:ys) | x > y     = x : y : ys
                | otherwise = y : insert x ys
~~~~~

Given an element `x` and a list `xs`, the function walks along `xs` 
till it finds the first element greater than `x` and it places `x` 
to the left of that element. Thus

~~~~~{.haskell}
*Main> insert 8 ([1..3] ++ [10..13])
~~~~~

Indeed, the following is the well known [insertion-sort][5] algorithm

> isort :: Ord a => [a] -> [a]
> isort = foldr insert []

We could write our own tests, but why do something a machine can do better?!

> prop_isort_sort    :: [Int] -> Bool
> prop_isort_sort xs = isort xs == sort xs

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_isort_sort 
~~~~~

Now, the reason that the above works is that the `insert` 
routine *preserves* sorted-ness. That is, while of course 
the property 

> prop_insert_ordered'      :: Int -> [Int] -> Bool
> prop_insert_ordered' x xs = isOrdered (insert x xs)

is bogus,

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_insert_ordered' 

*Main> insert 0 [0, -1]
~~~~~

the output *is* ordered if the input was ordered to begin with

> prop_insert_ordered      :: Int -> [Int] -> Property 
> prop_insert_ordered x xs = 
>   isOrdered xs ==> isOrdered (insert x xs)

Notice that now, the precondition is more *complex* -- the property 
requires that the input list be ordered. If we QC the property

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_insert_ordered

~~~~~





Ugh! The ordered lists are so *sparsely* distributed 
among random lists, that QC timed out well before it 
found 1000 valid inputs!

*Aside* the above example also illustrates the benefit of 
writing the property as `p ==> q` instead of using the boolean
operator `||` to write `not p || q`. In the latter case, there is 
a flat predicate, and QC doesn't know what the precondition is,
so a property may hold *vacuously*. For example consider the 
variant

> prop_insert_ordered_vacuous :: Int -> [Int] -> Bool
> prop_insert_ordered_vacuous x xs = 
>   not (isOrdered xs) || isOrdered (insert x xs)

QC will happily check it for us

~~~~~{.haskell}
*Main> quickCheckN 1000 prop_insert_ordered_vacuous
~~~~~

Unfortunately, in the above, the tests passed *vacuously* 
only because their inputs were *not* ordered, and one 
should use `==>` to avoid the false sense of security 
delivered by vacuity.

QC provides us with some combinators for guarding against 
vacuity by allowing us to investigate the *distribution* 
of test cases

~~~~~{.haskell}
collect  :: Show a => a -> Property -> Property
classify :: Bool -> String -> Property -> Property
~~~~~

We may use these to write a property that looks like

> prop_insert_ordered_vacuous' :: Int -> [Int] -> Property 
> prop_insert_ordered_vacuous' x xs = 
>   collect (length xs) $
>   classify (isOrdered xs) "ord" $
>   classify (not (isOrdered xs)) "not-ord" $
>   not (isOrdered xs) || isOrdered (insert x xs)

When we run this, as before we get a detailed breakdown
of the 100 passing tests

~~~~~{.haskell}
*Main> quickCheck prop_insert_ordered_vacuous'
~~~~~

where a line `P% N, COND` means that `p` percent of the inputs had length
`N` and satisfied the predicate denoted by the string `COND`. Thus, as we
see from the above, a paltry 13% of the tests were ordered and that was
because they were either empty (`2% 0, ord`) or had one (`9% 1, ord`).
or two elements (`2% 2, ord`). The odds of randomly stumbling upon a 
beefy list that is ordered are rather small indeed!




Generating Data
===============

Before we start discussing how QC generates data (and how we can help it
generate data meeting some pre-conditions), we must ask ourselves a basic
question: how does QC behave *randomly* in the first place?!

~~~~~{.haskell}
*Main> quickCheck prop_insert_ordered'

*Main> quickCheck prop_insert_ordered'

~~~~~

Eh? This seems most *impure* -- same inputs yielding two totally different
outputs! Well, this should give you a clue as to one of the key techniques
underlying QC -- **monads!** 

The QC library defines a type

   Gen a 

of "generators for values of type a".


Generator Combinators
---------------------

QC comes loaded with a set of combinators that allow us to create 
generators for various data structures.

The first of these combinators is `choose`

~~~~~{.haskell}
choose :: (System.Random.Random a) => (a, a) -> Gen a
~~~~~
 
which takes an *interval* and returns an random element from that interval.
(The typeclass `System.Random.Random` describes types which can be
*sampled*. For example, the following is a randomly chosen set of numbers
between `0` and `3`.

~~~~~{.haskell}
*Main> sample $ choose (0, 3)
~~~~~

A second useful combinator is `elements` 

~~~~~{.haskell}
elements :: [a] -> Gen a
~~~~~

which returns a generator that produces values drawn from the input list

~~~~~{.haskell}
*Main> sample $ elements [10, 20..100]
~~~~~

A third combinator is `oneof` 

~~~~~{.haskell}
oneof :: [Gen a] -> Gen a
~~~~~

which allows us to randomly choose between multiple generators

~~~~~{.haskell}
*Main> sample $ oneof [elements [10,20,30], choose (0,3)]
~~~~~

and finally, the above is generalized into the `frequency` combinator 

~~~~~{.haskell}
frequency :: [(Int, Gen a)] -> Gen a
~~~~~

which allows us to build weighted combinations of individual generators.

~~~~~{.haskell}
*Main> sample $ frequency [(1, elements [10,20]), (5, elements [11,21])]
~~~~~

The Generator Monad
-------------------

The parameterized type 'Gen' is an instance of the monad type class. What this
means (for today) is that there are a number of monadic operations available
for it.

~~~~~~~{.haskell}
-- from the class Monad
--
return :: a -> Gen a 
(>>=)  :: Gen a -> (a -> Gen b) -> Gen b

-- from the library Control.Monad
--
liftM  :: (a -> b) -> Gen a -> Gen b  
liftM2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
liftM3 :: (a -> b -> c -> d) -> Gen a -> Gen b -> Gen c -> Gen d

~~~~~~~

Note, `liftM` above has another name---`fmap`.  That's right, every monad 
is also a functor.

We will cover what it exactly means for Gen to be a monad in a future
lecture. However, as we will see, these operations will let us put generators
together compositionally.

> genThree :: Gen Int    -- a generator that always generates the value '3'
> genThree = return 3 

> genPair :: Gen a -> Gen b -> Gen (a,b)
> genPair ga gb = do
>   a <- ga
>   b <- gb
>   return (a,b)

> genPair' :: Gen a -> Gen b -> Gen (a,b)
> genPair' ga gb = ga >>= (\a -> gb >>= (\b -> return (a,b)))

> genPair'' ga gb = liftM2 (,) ga gb


The Arbitrary Typeclass
-----------------------

To keep track of all these generators, QC defines a typeclass containing types
for which random values can be generated!

~~~~~{.haskell}
class Arbitrary a where
  arbitrary :: Gen a
~~~~~

Thus, to have QC work with (ie generate random tests for) values of type
`a` we need only make `a` an instance of `Arbitrary` by defining an
appropriate `arbitrary` function for it. QC defines instances for base
types like `Int` , `Float`, etc

~~~~~{.haskell}
*Main> sample (arbitrary :: Gen Int)
~~~~~

and lifts them to compound types.

~~~~~{.haskell}
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a,b,c) where
  arbitrary = liftM3 (,,) arbitrary arbitrary arbitrary
~~~~~

~~~~~{.haskell}
*Main> sample (arbitrary :: Gen (Int,Float,Bool))
~~~~~

~~~~~{.haskell}
*Main> sample (arbitrary :: Gen [Int])
~~~~~



Generating Ordered Lists
------------------------

We can use the above combinators to write generators for lists 

> genList1 ::  (Arbitrary a) => Gen [a]
> genList1 = liftM2 (:) arbitrary genList1

~~~~~{.haskell}
*Main> sample (genList1 :: Gen [Int])
~~~~~

Can you spot a problem in the above?












It only generates infinite lists! Hmm. Let's try again,

> genList2 ::  (Arbitrary a) => Gen [a]
> genList2 = oneof [ return []
>                  , liftM2 (:) arbitrary genList2]

~~~~~{.haskell}
*Main> sample (genList2 :: Gen [Int])
~~~~~







This is not bad, but we may want to give the generator a higher 
chance of not finishing off with the empty list, so let's use

> genList3 ::  (Arbitrary a) => Gen [a]
> genList3 = frequency [ (1, return [])
>                      , (7, liftM2 (:) arbitrary genList3) ]

~~~~~{.haskell}
*Main> sample (genList3 :: Gen [Int])
~~~~~





We can use the above to build a custom generator that always returns
*ordered lists* by piping the generated list into the `sort` function

> genOrdList :: (Arbitrary a, Ord a) => Gen [a]
> genOrdList = genList3 >>= return . sort

~~~~~{.haskell}
*Main> sample (genOrdList :: Gen [Int])
~~~~~





To *check* the output of a custom generator we can use the `forAll` combinator

~~~~~{.haskell}
forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
~~~~~

For example, we can check that in fact, the combinator only produces
ordered lists

~~~~~
*Main> quickCheck $ forAll genOrdList isOrdered 
~~~~~

and now, we can properly test the `insert` property

> prop_insert :: Int -> Property 
> prop_insert x = forAll genOrdList $ \xs ->
>   isOrdered xs && isOrdered (insert x xs)

~~~~~
*Main> quickCheck prop_insert 
~~~~~




Using `newtype` for smarter test-case generation
------------------------------------------------

This works very well, but we might not want to write `forAll genOrdList`
everywhere we want to test a property on ordered lists only.  In order to get
around that, we can define a new type that *wraps* lists, but has a different
`Arbitrary` instance:

> newtype OrdList a = OrdList [a] deriving (Eq, Ord, Show, Read)
> 
> instance (Ord a, Arbitrary a) => Arbitrary (OrdList a) where
>   arbitrary = liftM OrdList genOrdList

This says that to generate an arbitrary `OrdList`, we use the `genOrdList` 
generator we just defined, and package that up.

~~~~~{.haskell}
*Main> sample (arbitrary :: Gen (OrdList Int))
~~~~~

Now, we can rewrite our `prop_insert` function more simply:

> prop_insert' :: Int -> OrdList Int -> Bool
> prop_insert' x (OrdList xs) = isOrdered $ insert x xs

And in fact, QuickCheck already has this type built in:

> prop_insert'' :: Int -> OrderedList Int -> Bool
> prop_insert'' x (Ordered xs) = isOrdered $ insert x xs

This technique of using `newtype`s for special-purpose instances is very 
common, both in QuickCheck and in other Haskell libraries.


-------------------------------------------------------------------------

Credit: This lecture is adapted from one used at UCSD [12].

[0]: http://www.cse.chalmers.se/~koen/
[1]: http://www.cse.chalmers.se/~rjmh/QuickCheck/
[2]: http://www.cs.york.ac.uk/fp/smallcheck/
[3]: http://video.google.com/videoplay?docid=4655369445141008672#
[4]: http://www.erlang-factory.com/upload/presentations/55/TestingErlangProgrammesforMulticore.pdf
[5]: http://en.wikipedia.org/wiki/Insertion_sort
[6]: http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/src/Test-QuickCheck-Gen.html#Gen
[7]: http://book.realworldhaskell.org/read/monads.html
[8]: http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
[11]: http://www.cse.chalmers.se/~rjmh
[12]: http://cseweb.ucsd.edu/classes/wi11/cse230/lectures/quickcheck.lhs
