Lecture 4: User-defined datatypes
====================================================

> {-# OPTIONS -fno-warn-type-defaults -fwarn-incomplete-patterns #-}
> module Lec4 where

> import Prelude hiding (Maybe,Just,Nothing,Either,Left,Right)
> import Test.HUnit

REMINDER: HW #0 is due Today at midnight! 
REMINDER: HW #1 is due Friday at midnight! 

Secret Code
===========

Optional extra practice for working with lists: we can develop a [Haskell
program](SecretCode.html) to encode files from disk.

User-defined datatypes
======================

So far, we've mostly talked about how to use the types that appear in
the Haskell standard library.  We also discussed a few type synonyms,
like

     type XY = (Double,Double)

from the last lecture, but we haven't described any ways to define
really _new_ types.

As a motivating example, suppose you are writing an application that
deals with calendars and you need to represent the days of the week.
You might be tempted to use `String`s or `Int`s, but both of these
choices have issues.  If you use

  type Day = String

there will be lots of `Day`s that don't actually represent real days.
Also, you will need to devise and adhere to some sort of
standardization - is Monday represented by "Monday", "monday",
"MONDAY", or "Mon"?  Should you handle more than one?

The choice

  type Day = Int

has similar problems.  There are lots of Ints that won't represent
valid days.  Also you'll have to remember whether you pick Sunday or
Monday to be the first day of the week, and whether it is represented
by 0 or 1.

Haskell has a better solution: user-defined datatypes:

> data Day = Monday
>          | Tuesday
>          | Wednesday
>          | Thursday
>          | Friday
>          | Saturday
>          | Sunday
>   deriving (Show, Eq)

The new values (`Monday`, `Tuesday`, etc.) are called "constructors" or "data
constructors".  This is a very simple example of a datatype (basically just an
enumeration), but we'll see more examples in a minute.

The last line enables printing and equality for this datatype. We'll see more
about that in the next lecture.

We can define functions on datatypes by pattern matching!  For example:

> nextWeekday :: Day -> Day
> nextWeekday Monday    = Tuesday
> nextWeekday Tuesday   = Wednesday
> nextWeekday Wednesday = Thursday
> nextWeekday Thursday  = Friday
> --nextWeekday _         = Monday   -- not so good for refactoring
> nextWeekday Friday    = Monday
> nextWeekday Saturday  = Monday
> nextWeekday Sunday    = Monday
> 



This is great.  Now we don't have to worry about the difference
between "Monday" and "monday" or which Int corresponds to which day.
If we make a typo (for example, write Frday instead of Friday), the
compiler will warn us _at compile time_.  And if we forget to handle
one of the days in some function, the compiler will warn us about that
too (inexhaustive pattern match warning).

Let's write one more function on `Day`s - we can now easily compute
when a package will arrive by "two day" shipping:

> twoBusinessDays :: Day -> Day
> -- twoBusinessDays x = nextWeekday (nextWeekday x)

> twoBusinessDays = nextWeekday . nextWeekday

> dontimes :: (a -> a) -> Int -> (a -> a)
> dontimes f n | n <=0      = id
>              | otherwise  = f . dontimes f (n - 1)

> dontimes' f n = foldr (.) id (replicate n f)





Datatypes can hold data, too.  For example, here is a datatype for
representing shapes:

> data Shape =
>    Circle    Point Double
>  | Rectangle Point Point

> data Point = 
>    Point Double Double




Here, `Circle` and `Rectangle` are the constructors - every `Shape`
must be one or the other.  Each constructors has some arguments:

- A `Circle` is specified by three `Doubles`.   These represent the x
  and y coordinates of the center and the radius.

- A `Rectangle` is specifed by four `Doubles`.  The first two are the
  coordinates of the lower left corner, and the second two are the
  coordinates of the upper right corner.







We can pattern match on shapes.  For example, here is a function that
computes the area of any `Shape`:

> area :: Shape -> Double
> area (Circle (Point _ _) r) = pi * r * r
> area (Rectangle (Point x1 y1) (Point x2 y2)) = (x2 - x1) * (y2 - y1)

> -- f :: Int -> Int
> -- f (x y) = y




Note that constructors are first-class Haskell values just all the
other values we have seen.  Like any value, they have types. 







For example the types of `Monday` and `Tuesday` shouldn't surprise you:

        Monday  :: Day
        Tuesday :: Day




The constructors that take arguments have _function_ types.  For
example, you must apply `Circle` to three `Double`s to get a `Shape`:

         Circle    :: Double -> Double -> Double -> Shape

         Rectangle :: Double -> Double -> Double -> Double -> Shape






Recursive Types
===============

Datatypes can be defined recursively.  That is, their constructors can
take other elements of the same type as arguments.

For example, we could define a type representing *nonempty* lists of integers:

> data IntList = ISingle Int
>              | ICons Int IntList

So that the list 1,2,3 is represented as:

> oneTwoThree :: IntList
> oneTwoThree = ICons 1 (ICons 2 (ISingle 3))

For comparison with Haskell's built-in lists, it might help to think
of this as:

> oneTwoThree' :: IntList
> oneTwoThree' = 1 `ICons` (2 `ICons` (ISingle 3))

We can define functions by recursion on these too, of course:

> sumOfIntList :: IntList -> Int
> sumOfIntList (ISingle x) = x 
> sumOfIntList (ICons x il) = x + sumOfIntList il

> testSumIL = "sumOfIntList" ~: sumOfIntList oneTwoThree ~?= 6

> mapIntList :: (Int -> Int) -> IntList -> IntList
> mapIntList f (ISingle x) = ISingle (f x)
> mapIntList f (ICons x xs) = ICons (f x) (mapIntList f xs)

> foldr1IntList :: (Int -> Int -> Int) -> IntList -> Int
> foldr1IntList f (ISingle x) = x
> foldr1IntList f (ICons x xs) = f x (foldr1IntList f xs)

> foldrIntList :: (Int -> a -> a) -> a -> IntList -> a
> foldrIntList f b (ISingle x) = f x b
> foldrIntList f b (ICons x xs) = f x (foldrIntList f b xs)


> sum' = foldrIntList (+) 
> sum'' = foldrIntList (+) 0




Polymorphic Datatypes
=====================

It would sure be annoying to have a seperate kind of list for each
type of data!  Luckily, we know Haskell's list type is polymorphic -
you can have a list of type `[a]` for any `a`. 

Users can define polymorphic datatypes too. For example, we can make the
non-empty lists above polymorphic.


As another example, here is the definition of the `Maybe` type (from the
Prelude) that we've used in past lectures:

> data Maybe a = Nothing | Just a

Notice that the type `Maybe` itself takes an argument now - the type
variable `a`.  We're allowed to use that type variable in the
constructors.  So `Just` is a constructor that can be applied to
values of any type and will create a `Maybe` of the same type:

    Just :: a -> Maybe a

Thus, `Just` and `Nothing` work at any type:

> justThree :: Maybe Int
> justThree = Just 3

> noInt :: Maybe Int
> noInt = Nothing

> justTrue :: Maybe Bool
> justTrue = Just True










A number of other polymorphic datatypes appear in the standard
library.  For example, here's a datatype that's useful when you
want to carry around values that could have either of two types:

> data Either a b = Left a | Right b

`Either` is often useful for error handling.  Sometimes returning a
`Maybe a` isn't quite good enough because you'd like to give a helpful
error message.  `Either String a` works a little better - you can use
`Left msg` in the case of an error, and `Right v` in case things
are... all right.

For example, here's a safer division function:

> safeDiv :: Int -> Int -> Either String Int
> safeDiv _ 0 = Left "You can't divide by zero, silly."
> safeDiv x y = Right $ x `div` y

Of course, Either is more useful when things can go wrong in more
than one way.









Trees
=====

OK, now let's play a bit with a bigger example: trees.  Here's
one way to define binary trees that have data at the internal nodes
in Haskell:

> data Tree a = Leaf | Branch a (Tree a) (Tree a) deriving (Eq, Show)


For example, we can represent the following tree:


        5
      /   \
     2     9
    / \     \
   1   4     7


Like this:
(Branch 5 (Branch 2 (Branch 1 Leaf Leaf) (Branch 4 Leaf Leaf))
          (Branch 9 Leaf (Branch 7 Leaf Leaf)))

> exTree :: Tree Int
> exTree = Branch 5 (Branch 2 (Branch 1 Leaf Leaf) (Branch 4 Leaf Leaf))
>                   (Branch 9 Leaf (Branch 7 Leaf Leaf))


We can write simple functions by recursion:

> -- treePlus :: Tree Int -> Int -> Tree Int
> treePlus (Branch x l r) y = Branch (x + y) (treePlus l y) (treePlus r y)
> treePlus Leaf y = Leaf 

> testTreePlus = "treePlus" ~: treePlus (Branch 2 Leaf Leaf) 3 ~?= (Branch 5 Leaf Leaf)




> infixOrder :: Tree a -> [a]
> infixOrder Leaf = []
> infixOrder (Branch x l r) = infixOrder l ++ [x] ++ infixOrder r

> testInfixOrder = "infixOrder" ~: infixOrder exTree ~?= [1,2,4,5,9,7]






Of course, what we should really do is reimplement our higher-order
patterns for trees!

> treeMap :: (a -> b) -> Tree a -> Tree b
> treeMap f (Branch x l r) = Branch (f x) (treeMap f l) (treeMap f r)
> treeMap f Leaf = Leaf 

> treePlus' t v = treeMap (+v) t




So that, for example, to increment each node in a `Tree Int`, we could
write:

> treeIncr :: Tree Int -> Tree Int
> treeIncr = treeMap (+1)


> testTreeIncr = "treeIncr" ~: treeIncr (Branch 1 (Branch 2 Leaf Leaf) Leaf) ~?= 
>                                       (Branch 2 (Branch 3 Leaf Leaf) Leaf) 





The type of fold will have to change a little bit.  Now the function
that tells us how to combine the current element with the recursive
result will have to take two results - one for each branch!

> treeFold :: (a -> b -> b -> b) -> b -> Tree a -> b
> treeFold f b (Branch x l r) = f x  (treeFold f b l) (treeFold f b r)
> treeFold f b Leaf = b


Now, let's reimplement infixOrder:

> infixOrder' :: Tree a -> [a]
> infixOrder' = treeFold (\x ll lr -> ll ++ [x] ++ lr) []

> testInfixOrder' = "infixOrder'" ~: infixOrder' exTree ~?= [1,2,4,5,9,7]


And we can get the other orderings just as quickly!

> prefixOrder :: Tree a -> [a]
> prefixOrder = treeFold (\x ll lr -> [x] ++ ll ++ lr) []

> postfixOrder :: Tree a -> [a]
> postfixOrder = treeFold (\x ll lr -> ll ++ lr ++ [x]) []

> testPrefixOrder  = "prefixOrder" ~: prefixOrder exTree ~?= [5,2,1,4,9,7]
> testPostfixOrder = "postfixOrder" ~: postfixOrder exTree ~?= [1,4,2,7,9,5]




> main :: IO ()
> main = do 
>   runTestTT $ TestList [
>     testSumIL,
>     testTreePlus,
>     testInfixOrder,
>     testTreeIncr,
>     testInfixOrder',
>     testPrefixOrder,
>     testPostfixOrder ]
>   return ()





[1] Part of this lecture is taken from "Learn You a Haskell for Great Good".
    http://learnyouahaskell.com/
