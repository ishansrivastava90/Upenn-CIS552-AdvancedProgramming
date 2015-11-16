
Lecture 5: Type Classes
=======================

> {-# OPTIONS -fno-warn-type-defaults -XMultiParamTypeClasses #-}
> module Classes where

> import Test.HUnit
> import Data.Char
> import Prelude hiding (lookup)

Announcements
=============
* HW #2 is posted, due Sunday at midnight.

* Read Real World Haskell Ch. 6.

* For more details, "Classes, Jim, but not as we know them," lecture from [OPLSS13](http://www.cs.uoregon.edu/research/summerschool/summer13/lectures/ClassesJimOPLSS.pdf). Also see [resources](../resources.html) page for links to video.

Our first qualified type
========================

Question: What is the type of (+)?

We've most often used (+) to add Ints, as in:

> fancySeven :: Int
> fancySeven = 3 + 4

So you might guess that the type of (+) is:

    (+) :: Int -> Int -> Int

But if you think a little harder, you'll realize that we've also used
(+) to add `Float`s and `Double`s, as in:

> fancyEight :: Float
> fancyEight = 3.2 + 4.8

So it must also be the case that:

    (+) :: Float -> Float -> Float

At this point, you might guess that (+) has the type

    (+) :: a -> a -> a

since it seems to work at many different type.  But this type would be
too general - it doesn't really make sense to add a `Bool` to a `Bool`
or an `Char -> Char` to an `Char -> Char`.

We need a type in the middle - (+) should work on numbers, but not
other things.  If we look up the actual type, we find:

    (+) :: Num a => a -> a -> a

What's going on here?  What's that fancy => thing?

In this type, `Num a` is a "type class constraint".  The type says
that (+) should work at any type `a`, so long as `a` is a member
of the `Num` type class.

`Num` is one of many type classes in Haskell's standard library.
Types like `Int` and `Double` are members of this class because they
support operations like (+) and (-).








Eq
==

Let's consider another function we've been using quite a bit:

    (==) :: Eq a => a -> a -> a

Of course!  It wouldn't make sense to check for equality at any type -
how does one compare functions, for example?

Let's peek at the definition of the Eq type class:

    class Eq a where
      (==) :: a -> a -> Bool
      (/=) :: a -> a -> Bool

This declares `Eq` to be a type class with a single parameter, `a`.
To show that some type is a member of the class, we must provide
definitions of (==) and (/=) for the type.  We do this with an
"instance" declaration.

For example, consider the following type:

> data PrimaryColor = Red | Green | Blue

We can tell Haskell that `PrimaryColor`s can be compared for equality
like this:

> instance Eq PrimaryColor where
>     c1 == c2         = undefined
>     c1 /= c2         = undefined

Now we can use (==) and (/=) on `PrimaryColor`s!

> fancyTrue :: Bool
> fancyTrue = Red == Red

It might seem annoying, though, that we had to provide both (==) and
(/=)...












Actually, we don't!  Type classes are allowed to provide "default
instances" - the full definition of `Eq` from the Prelude is:

    class Eq a where
      (==),(/=) :: a -> a -> Bool
    
      x /= y               = not (x == y)
      x == y               = not (x /= y)

So to define `Eq` for a new type, we only actually have to provide one
of (==) and (/=).  Haskell can figure out the other for us.










Let's do another example.  We'd like to define `Eq` for the type
`Tree` that we saw last time.  But we have a bit of a problem - to
check if trees are equal, we'll need to know if the data in each pair
of `Branch`s is equal.  Put another way, we'll only be able to compare
two `Tree a`s if `a` is already an instance of `Eq`.

> data Tree a = Leaf | Branch a (Tree a) (Tree a)

No worries, Haskell lets us put type class constraints on our instance
declarations:


> instance Eq a => Eq (Tree a) where
>    t1 == t2 = undefined



This code tells Haskell how to compare `Tree a`s for equality as long
as it already knows how to compare `a`s.

Let's try it out:

> tree1, tree2 :: Tree Int
> tree1 = Branch 3 (Branch 2 Leaf Leaf) (Branch 1 Leaf Leaf)
> tree2 = Branch 3 Leaf Leaf

> testTreeEq :: Test
> testTreeEq = TestList [ "tree1 == tree1" ~: tree1 == tree1 ~?= True,
>                         "tree1 /= tree2" ~: tree1 == tree2 ~?= False,
>                         "tree1 /= Leaf"  ~: tree1 == Leaf  ~?= False ]


More qualified types
====================

We can now explain the types of a few functions that we glossed over
before.  Type class constraints don't just appear on the functions
defined as members of a type class - they can appear anywhere we want
to use such a function.  For example, last week, we used the standard
library function `lookup` to find a member of an association list.
Let's peek at its implementation:

> lookup :: Eq a => a -> [(a,b)] -> Maybe b
> lookup _ []          = Nothing
> lookup a ((a',b):ps) = if a == a' then Just b 
>                                   else lookup a ps

The idea is that lookup checks to see if the given value appears as
the first element of any pair in the list.  To implement lookup, we
need to use the (==) function to check if we've reached the right
pair.  So, the type of lookup records that there must be an `Eq`
instance for `a` - otherwise, the compiler wouldn't have an 
implementation of (==) for this type.




Deriving
========

We've now written a couple `Eq` instances ourselves, and you might
guess that most of our future `Eq` instances will have a very similar
structure.  They will recurse down datatypes, making the two terms
use the same constructors and that any subterms are equal.  You'd
be right!

To avoid this kind of boiler plate, Haskell provides a nifty mechanism
called `deriving`.  When you declare a new datatype, you may simply
ask Haskell to automatically derive an `Eq` instance of this form.
For example, if we wanted equality for the `Shape` type we saw last time,
we could have written:

> data Point = Point Double Double
>      deriving (Eq)

> data Shape = Circle Point Float
>            | Rectangle Point Point
>      deriving (Eq)

Haskell can derive an `Eq` instance as long as it already has one
available for any data that appears as arguments to constructors.
Since it already knows how to compare `Double`s, this one works.

It won't always work, though, consider this datatype, which can
contain functions on `Int`s.

> data IntFunctions = OneArg (Int -> Int)
>                   | TwoArg (Int -> Int -> Int)

There is no `Eq` instances for functions (how do you tell if two
functions are equal?).  So, if we added `deriving (Eq)` to this type,
we'd get an error.

Of course, not every type class supports this "deriving" mechanism.
GHC can derive instances of a handful of classes for us (we'll see a
few more today), but for most standard library type classes and any
classes you define, you must write the instances out yourself.







Show and Read
=============

Time for a couple more type classes from the standard library.

Though we haven't talked about it explicitly, we've been using
Haskell's printing functions quite a bit.  Every time we've run code
in ghci and looked at the output, Haskell had to figure out how
to convert the data to a String.  A few times we've even explicitly
used a function called `show`, which converts a value to a `String`/

Of course, not every value can be printed.  Let's take a look at
the type of `show`:

    show :: Show a => a -> String

Aha, another type class!  `show` converts an `a` to a `String`, as
long as `a` is a member of the `Show` class.  Let's look at
the definition of this class:

    class Show a where
      show      :: a   -> String
    
      showsPrec :: Int -> a -> ShowS
      showList  :: [a] -> ShowS

To define an instance of `Show`, you must implement either `show`
or `showsPrec`.  We've already discussed `show`, which is a bit
like Java's `toString`.

The second function, `showsPrec`, takes an extra `Int` argument which
can be used to indicate the "precedence" of the printing context -
this is useful in some situations to determine, for example, whether
parentheses are needed in the output.  Its return type, `ShowS` is
used for more efficient printing.  For now, you don't need to worry
about these details.

The third function, `showList`, exists so that users can specify
a special way of printing lists of values, if desired for a
given type.  Usually the default instance is used.

By convention, instances of `Show` should produce valid Haskell
expression.

There is an "inverse" type class, called `Read`, which provides
the following function:

    read :: Read a => String -> a

Notice that the type variable `a` doesn't appear in any arguments to
this function.  In general, to use `read`, you must make sure the type
of the output is clear from context or provide it specifically.
For example, if you try this in ghci:

    ghci> read "3"

You will get an error - ghc doesn't know whether to interpret the
string "3" as an Int or a Float or even a Bool.  You can help it,
though:

    ghci> read "3" :: Int
    3

You can see the details of the `Read` type class in the standard
library.  Predictably, parsing values is a little more complicated
than printing them.  One important thing to remember, though, is
that the `read` and `show` functions should be inverses.  So, 
for example

    read (show 3) :: Int

should return 3, and
    
    show (read "3" :: Int)

should return "3".

Both `Show` and `Read` are derivable:

> data SadColors = Black | Brown | Grey
>    deriving (Eq, Show, Read)

Notice that if we type a value into ghci and the corresponding type
doesn't have a `Show` instance, we get an error saying ghci doesn't
know how to display the value:

    ghci> Leaf
  
      <interactive>:1:1:
          No instance for (Show (Tree a0))
            arising from a use of `print'
          Possible fix: add an instance declaration for (Show (Tree a0))
          In a stmt of an interactive GHCi command: print it
  
  
    ghci> \x -> (x,x)
  
      <interactive>:1:1:
          No instance for (Show (t0 -> (t0, t0)))
            arising from a use of `print'
          Possible fix:
            add an instance declaration for (Show (t0 -> (t0, t0)))
          In a stmt of an interactive GHCi command: print it

Type classes and OOP
====================

At this point, many of you are probably thinking that type classes are
a lot like Java's classes or interfaces.  You're right!  Both provide
us with a way to describe functions that can be implemented for many
types.

There are some important differences, though:

*   In Java, when you define a new class, you must specify all the
    interfaces it implements right away.  Haskell lets you add a new
    instance declaration at any time.
  
    This is very convenient - we often define new type classes and want
    to be able to add instances for types that are already around.  We
    wouldn't want to have to change the standard library just to
    get an instance for Int!
  
*   Type classes are better integrated in the standard library than
    Java interfaces.  In particular, every object in Java has "equals"
    and "toString" methods.  But this is silly - not every type of data
    can be checked for equality or printed effectively.  The result is
    that calling equals on classes that don't actually implement it
    may result in a run-time error.
  
    By contrast, Haskell will warn us _at compile time_ if we try to use
    (==) on a tzerm that doesn't support it.  It's all tracked in the
    types!
  
*   Haskell supports multi-parameter type classes and multiple
    inheritence.
  
    In Haskell, classes may require that types be members of an
    arbitrary number of other classes.  For example, you might
    write a class for "Serializable" data that can be written to
    a file and demand that Serializable types implement both
    Read and Show:
  
        class (Read a, Show a) => Serializable a where
          toFile :: a -> ByteString
          ...
  
    Also, type classes in Haskell may have multiple type arguments.
    Often it's useful to think of such classes as describing a
    relationship between types.  For example, we can define a class:
  
> class Convertible a b where
>     convert :: a -> b
  
    Instances of `Convertible a b` show how to convert from one type
    to another.  For example, we can convert from `Char`s to `Int`s
    using Haskell's built in `ord` function, which gets the ascii
    code of a character:
  
> instance Convertible Char Int where
>     convert = ord
  
    Or we can convert from Trees to Lists using the inorder traversal:
  
> instance Convertible (Tree a) [a] where
>     convert = infixOrder where
>        infixOrder Leaf = []
>        infixOrder (Branch v l r) = infixOrder l ++ [v] ++ infixOrder r
  
    Java doesn't have analogues for these features.
  
    (Note that to use multi-parameter type classes, you must give ghc
    the -XMultiParamTypeClasses flag)


Ord
===

Let's talk about another typeclass from the standard library.  This
one is for comparisons.  Question: what's the type of (<)?

    (<) :: Ord a => a -> a -> Bool

`Ord` is a type class for things that can be ordered.  Here is its
definition:

    class Eq a => Ord a where
       compare              :: a -> a -> Ordering
       (<), (<=), (>), (>=) :: a -> a -> Bool
       max, min             :: a -> a -> a

Notice that to be a member of the `Ord` class, a type must also
have an implementation of `Eq`.

Most of these methods we've seen before, so let's talk about the
one we haven't:

    compare :: Ord a => a -> a -> Ordering

This uses a new type from the standard library:

    data Ordering = LT | EQ | GT

So `compare` takes two terms and tells us whether the first is Less
Than, Equal to, or Greater Than than the second.  Most built in types
already have `Ord` instances - try some examples in ghci.

`Ord` is derivable like `Eq`, `Show` and `Read`.  If you're writing
your own `Ord` instance, you only need to provide `compare` or `(<=)`;
Haskell can fill in the rest.

The `Ord` type class shows up all over the standard library.  For
example, Data.List has a function which sorts lists:

    sort :: Ord a => [a] -> [a]

As you'd expect, we need to know an ordering on `a`s in order to sort
lists of them!


Enum and Bounded
================

Last week, we observed that we could use the [a..b] list syntax on
both `Int`s and `Char`s.  For example:

> tenToThirty :: [Int]
> tenToThirty = [10..30]

> abcde :: [Char]
> abcde = ['a'..'e']

But obviously this syntax can't work on every type.  Indeed, it works
on only the ones that implement the `Enum` type class!  This describes
sequentially ordered types - i.e., those that can be enumerated.

    class Enum a  where
      succ, pred           :: a -> a

      toEnum               :: Int -> a
      fromEnum             :: a -> Int
  
      -- These are used in haskell's translation of [n..m]
      enumFrom            :: a -> [a]
      enumFromThen        :: a -> a -> [a]
      enumFromTo          :: a -> a -> [a]
      enumFromThenTo      :: a -> a -> a -> [a]

Watch out, though. This syntax can lead to some strange behaviors if you don't
know exactly what these functions do for your type. What do you think this
expression means?

> wat  = [0.8 .. 10]

or what about this one?

> wat' = [0.5 .. 10]


OK, one more basic type class.  Recall that `Int` isn't arbitrary
precision: it represents an actual machine-sized number in your
computer.  Of course, this varies from machine to machine (64-bit
computers have a lot more `Int`s than 32-bit ones).  And Haskell
supports a number of other bounded datatypes too - `Char`, `Word`,
etc.

It would sure be nice if there were a uniform way to find out how
big these things are on a given computer...

Enter `Bounded`!

    class Bounded a where
      minBound, maxBound     :: a

So to find the biggest `Int` on my machine, I can write:

> biggestInt :: Int
> biggestInt = maxBound

Of course, if I tried to write:

    biggestInteger :: Integer
    biggestInteger = maxBound

I would get a type error, since `Integer`s are unbounded.  Again
the compiler protects us from basic mistakes like this.

Many standard library types support `Enum` and `Bounded`.  They are
also both derivable - but only for datatypes whose constructors don't
take any arguments.








Functor
=======

Now it's time for my favorite type class!

Recall the `map` function on lists:

    map :: (a -> b) -> [a] -> [b]

`map` takes a function and applies it to every element of a list,
creating a new list with the results.  We saw last week that
the same pattern can be used for `Tree`s:

    treeMap :: (a -> b) -> Tree a -> Tree b

If you think a little, you'll realize that map makes sense for pretty
much any data structure that holds a single type of values.  It would
be nice if we could factor this out into a class to keep track of the
types that support `map`.

Behold, `Functor`:

    class Functor f where
      fmap :: (a -> b) -> f a -> f b

    instance Eq a => Eq (Tree a) where
      ...

    instance Functor Tree where 
      ...
`Functor` is a little different than the other classes we've seen
so far.  It's a "constructor" class, because the types it works on
are constructors like `Tree`, `Maybe` and `[]` - they take another
type as their argument.  Notice how the `f` in the class declaration
is applied to other types.

The standard library defines:

    instance Functor [] where
      -- fmap :: (a -> b) -> [a] -> [b]
      fmap = map

And we can define an instance for our trees:

> instance Functor Tree where
>   fmap = treeMap where
>     treeMap f Leaf = Leaf
>     treeMap f (Branch x l r) = Branch (f x) (treeMap f l) (treeMap f r)

The standard library also defines `Functor` instances for a number of
other types.  For example, `Maybe` is a `Functor`:

    instance Functor Maybe where
       fmap _ Nothing  = Nothing
       fmap f (Just a) = Just (f a)

`Functor` is very useful, and you'll see many more examples of it in
the weeks to come.

Monad
=====

Last, the most famous of all Haskell type classes: The warm fuzzy
thing called 'Monad'.

We saw an example of the IO monad with code like this:

> main :: IO ()
> main = do
>    putStrLn "This is the Classes lecture. What is your name?"
>    inpStr <- getLine
>    putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
>    putStrLn $ "Now running tests."
>    _ <- runTestTT testTreeEq
>    return ()

This code works because IO is an instance of the Monad type class. We'll see
more instances of this class in the next few lectures. Don't try to understand
it all at once, we'll start with just seeing what's going on at a syntactic
level.

      class  Monad m  where

          -- | Sequentially compose two actions, passing any value produced
          -- by the first as an argument to the second.
          (>>=)       :: m a -> (a -> m b) -> m b

          -- | Inject a value into the monadic type.
          return      :: a -> m a

          -- | Sequentially compose two actions, discarding any value produced
          -- by the first, like sequencing operators (such as the semicolon)
          -- in imperative languages.
          (>>)        :: m a -> m b -> m b
          m >> k      = m >>= \_ -> k         -- default definition
  
          -- | Fail with a message.  This operation is not part of the
          -- mathematical definition of a monad, but is invoked on pattern-match
          -- failure in a @do@ expression.
          fail        :: String -> m a
          fail s      = error s              -- default definition
      
  
You can see the use of `return` in the last line of the `main` function above. 
In fact, it must be the last line in any computation because it doesn't compose 
multiple actions together.

> nop :: IO ()
> nop = do 
>         return ()

We've also been using `(>>=)`, but only behind the scenes. You've missed it
because of another feature of Haskell---layout.

If you've seen some code like this:

        main :: IO ()
        main = do 
          x <- doSomething
          doSomethingElse
          y <- andSoOn
          return ()
        

it is really shorthand for code like this:

       doSomething >>= ( \x -> 
         doSomethingElse >>
           (andSoOn >>= ( \y -> 
             return () )))

So everytime that you see `do` there is some monad involved (not necessarily
IO).

