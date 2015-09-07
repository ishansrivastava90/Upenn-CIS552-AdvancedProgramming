CIS 552 Lecture 2 - Lists and First-Class Functions
===================================================

> {-# LANGUAGE NoImplicitPrelude #-}
> {-# OPTIONS -fwarn-tabs -fno-warn-type-defaults #-}

> module Lec2 where
> import Data.Char
> import Test.HUnit

> import Prelude hiding (($))

Lists
-----

       [A]

A list is a sequence of values of the same type. There is no limit to
the number of values that can be stored in a list.

> l1 :: [Char]
> l1 = undefined

> l2 :: [Int]
> l2 = undefined

Lists can contain structured data...

> l3 :: [(Int,Bool)]
> l3 = [ (1,True), (2, False) ]

...and can be nested:

> l4 :: [[Int]]
> l4 = undefined

List elements *must* have the same type.

> -- l5 :: [Int]
> -- l5 = [ 1 , True ]  -- doesn't type check

The empty list is written `[]` and pronounced "nil".

> l6 :: [a]
> l6 = []

*Note*: `String` is just another name for `[Char]`.

> l7 :: String
> l7 = ['h','e','l','l','o',' ','5','5','2','!']




"Cons"tructing Lists
---------------------

> cons :: a -> [a] -> [a]
> cons = (:)

Input: element ("head") and list ("tail")
Output: new list with head followed by tail

> c1 :: [Char]
> c1 = 'a' : ['b', 'c']

> c2 :: [Int]
> c2 = 1 : []

> -- what is the type of c3?
> c3 = [] : [] 




Syntactic Sugar
---------------

       [x1,x2, .. , xn]   

 is short for   

       x1 : x2 : .. : xn : []




Function practice: List Generation
----------------------------------

Problem: Write a function that, given an argument `x` and a number `n`, returns
a list containing `n` copies of `x`.

Step 1: Define test cases for the function. 

We're using HUnit, a library for defining unit tests in Haskell.  The (~?=)
operator constructs a unit `Test` by comparing the actual result (first
argument) with an expected result (second argument). Haskell is lazy, so
these definitions *create* tests, but don't actually run them yet.

> testClone1, testClone2, testClone3 :: Test
> testClone1 = clone 'a' 4 ~?= ['a','a','a','a']
> testClone2 = clone 'a' 0 ~?= []
> testClone3 = clone 1.1 3 ~?= [1.1, 1.1, 1.1]


Step 2: Define the type of the function

> clone :: a -> Int -> [a]


Step 3: Implement the function

> -- clone x n = undefined
> clone x 0 = []
> clone x n = x : clone x (n-1)
>


Step 4: Run the tests

The HUnit function `runTestTT` actually runs a given unit test and prints
its result to the standard output stream. (That is why its result type is `IO
Counts`. The IO means that this computation does IO.)

> cl1, cl2, cl3 :: IO Counts
> cl1 = runTestTT testClone1
> cl2 = runTestTT testClone2
> cl3 = runTestTT testClone3

or

> cl4 :: IO Counts
> cl4 = runTestTT (TestList [ testClone1, testClone2, testClone3 ])


Function practice: Refactoring
------------------------------

Step 5: Refine the definition to make it more readable

> clone' :: a -> Int -> [a]
> clone' a 0 = []
> clone' a n = a : clone' a (n-1) 


Make sure to rerun the test cases on the new version.

> cl' :: IO Counts
> cl' = runTestTT (TestList [ 
>           clone' 'a' 4 ~?= ['a','a','a','a'],
>           clone' 'a' 0 ~?= [],
>           clone' 1.1 3 ~?= [1.1, 1.1, 1.1]
>          ])



More function practice
-----------------------

Define a function that, given two integers `i` and `j`, returns a list
containing all of the numbers at least as big as `i` but no bigger than `j`, in
order.

Step 1: Define test cases

> testRange :: Test
> testRange = TestList [ range 3  6  ~?= [3,4,5,6],
>                        range 42 42 ~?= [42],
>                        range 10 5  ~?= [] ]


Step 2: Declare the type

> range :: Int -> Int -> [Int]


Step 3: Define the function

> range i j = if i > j then []
>     else i : range (i+1) j


Step 4: run tests

> runRTests :: IO Counts
> runRTests = runTestTT testRange


Step 5: refactor

> range' :: Int -> Int -> [Int]
> range' i j 
>    | i > j     = []
>    | otherwise = i : range' (i+1) j


List recursion
--------------

The examples so far have *constructed* various lists. Of course, sometimes
we would like to write functions that *use* lists. We can use a list by
pattern matching...

> isHi :: String -> Bool
> isHi ['H','i'] = True
> isHi _ = False

> isGreeting :: String -> Bool
> isGreeting "Hi" = True
> isGreeting "Hello" = True
> isGreeting "Bonjour" = True
> isGreeting "Guten Tag" = True
> isGreeting _ = False

We can also work with lists more abstractly, for example determining if we have
a list of length one...

> isSingleton :: [a] -> Bool
> isSingleton (x : []) = True
> isSingleton _        = False

...or of length greater than two.

> isLong :: [a] -> Bool
> isLong l = if length l > 2 then True
>     else False


Function practice: List Access
------------------------------

Define a function, called listAdd, that, given a list of Ints returns
their sum.

Step 1: define test cases

> listAddTests :: Test
> listAddTests = TestList [ listAdd [] ~?= 0,
>                           listAdd [1, 2, 3] ~?= 6 ]


Step 2: define function signature

> listAdd :: [Int] -> Int


Step 3: implementation  

(using pattern matching to define the function by 
case analysis.)

> listAdd []       = 0
> listAdd (x : xs) = x + listAdd xs


Step 4: run the tests

> runLATests :: IO Counts
> runLATests = runTestTT listAddTests


Step 5: refactor as appropriate

> listAdd' :: [Int] -> Int
> listAdd' = undefined


Note that listAdd follows a general pattern of working with lists called *list
recursion*.  We can define lists as follows:

  A list is either 
     []      -- the empty list
     x : xs  -- or, an element x cons'ed onto another list xs

This is a recursive definition, as we are defining lists in terms of
themselves.  Recursive functions that work with lists will follow the
pattern of this definition:

     f :: [a] -> ...
     f [] =  ...       -- case for the empty list
     f (x : xs) = ...  -- case for a nonempty list, will use `f xs` 
                       -- recursively somehow. 




Function practice: List modification
------------------------------------

Define a function, called `listIncr`, that, given a list of ints,
returns a new list where each number has been incremented.

Step 1: write test case(s)

> listIncrTests :: Test
> listIncrTests = 
>  TestList [ listIncr [1,2,3] ~?= [2,3,4],
>             listIncr [42]    ~?= [43],
>             listIncr []      ~?= ([] :: [Int]) ]

Step 2: write function type

> listIncr :: [Int] -> [Int]

Step 3: define the function

> listIncr [] = []
> listIncr (x : xs) = x + 1 : listIncr xs

Step 4: run the tests

> runLITests :: IO Counts 
> runLITests = runTestTT listIncrTests

Step 5: refactor, if necessary



Making Haskell DO something
===========================

Programs interact with the world:
* Read files
* Display graphics
* Broadcast packets

They don't just compute values.

How does this fit with values & equalities above?


I/O via an "Action" Value
-------------------------

"IO actions" are a new sort of sort of value that describe an effect
on the world.

     IO a 

     Type of an action that returns an a



Example: Output action
----------------------

Actions that do something but return nothing have the type "IO ()".
() is pronounced "unit".

    putStr :: String -> IO ()

So `putStr` takes in a string and returns action that writes string to
stdout.

The only way to "execute" action (without using ghci), is to make it
the value of name "main".

> main :: IO ()
> main = putStr "Hello World! \n"

Compile and run:  
   ghc -o hello -main-is Lec2 Lec2.lhs 




Actions Just DESCRIBE Effects
-----------------------------

We can pass around IO values just like any other type.  For example, we can 
stick them in a pair:

> act2 :: (IO (), IO ())
> act2 = (putStr "Hello", putStr "Hello")


This doesn't actually run both actions, it just creates a pair holding 
two IO computations.

How can we do many actions?  By composing small actions.

Just 'do' it
-------------

> many :: IO ()
> many = do putStr "Hello"
>           putStr " World!"
>           putStr "\n"

Note: white-space is significant here. (See ch 3, "Offside rule" in
RWH).



Example: Input Action
---------------------

Actions can also return a value.

    getLine :: IO String

This reads and returns a line from stdin.  We can name the result:

    x <- action

Here `x` is a variable that can be used to refer to the result of the
action in later code.

> query :: IO ()
> query = do putStr "What is your name? "
>            n <- getLine
>            putStrLn ("Welcome to CIS 552 " ++ n)




Example: Testing Actions
------------------------

runTestTT :: Test -> IO Counts

> numTest :: IO Counts
> numTest = runTestTT (3 ~?= 4) 

This is an action that runs the test case(s) and returns a
data structure recording which ones pass and fail.

> dotest :: IO ()
> dotest = do c <- runTestTT (3 ~?= 3)
>             putStrLn (show c)



Functions Are Data 
==================

As in all functional languages, Haskell functions are *first-class*
values, meaning that they can be treated just as you would any other
data.

You can pass functions around in *any* manner that you can pass any
other data around. For example, suppose you have a simple functions
`plus1` and `minus1` defined via the equations

> plus1 :: Int -> Int
> plus1 x = x + 1

> minus1 :: Int -> Int
> minus1 x = x - 1

Now, you can make a pair containing two instances of the function.

> funp :: (Int -> Int, Int -> Int)
> funp = undefined


Or you can make a list containing some copies of the functions:

> funs :: [Int -> Int]
> funs = undefined




Taking Functions as Input
-------------------------

This innocent looking feature makes a langage surprisingly brawny and
flexible, because now, we can write *higher-order* functions that take
functions as input and return functions as output!  Consider:

> doTwice :: (a -> a) -> a -> a
> doTwice f x = f (f x)

> dtTests :: Test
> dtTests = TestList [ doTwice plus1  4 ~?= 6,
>                      doTwice minus1 5 ~?= 3 ]

Here, `doTwice` takes two inputs: a function `f` and value `x`, and
returns the the result of applying `f` to `x`, and feeding that result
back into `f` to get the final output.  Note how the raw code is
clearer to understand than my long-winded English description!

Last time we talked about how programs execute in Haskell: we just
substitute equals for equals.  Let's think about an example with
`doTwice`: 

~~~~~
doTwice plus1 10 == plus1 (plus1 10)        {- unfold doTwice -} 

                 == plus1 (10 + 1)          {- unfold plus1 -}

                 == (10 + 1) + 1            {- unfold plus1 -}

                 == 12                      {- old-school arithmetic -}
~~~~~




Returning Functions as Output
-----------------------------

Similarly, it can be useful to write functions that return new 
functions as output. For example, rather than writing different 
versions `plus1`, `plus2`, `plus3` *etc.* we can just write a 
single function `plusn` as

> plusn :: Int -> (Int -> Int)
> plusn n = f
>    where f x = x + n

That is, `plusn` returns as output a function `f` which itself 
takes as input an integer `x` and adds `n` to it. Lets use it

> plus10  :: Int -> Int
> plus10  = undefined

> minus20 :: Int -> Int
> minus20 = undefined

Note the types of the above are `Int -> Int`.  That is, `plus10` and
`minus20` are functions that take in an integer and return an integer
(even though we didn't explicitly give them an argument).




Partial Application
-------------------

In regular arithmetic, the `-` operator is *left-associative*. Hence,
        
        2 - 1 - 1 == (2 - 1) - 1 == 0

(and not `2 - (1 - 1) == 2` !). Just like `-` is an arithmetic
operator that takes two numbers and returns an number, in Haskell,
`->` is a *type operator* that takes two types, the input and output,
and returns a new function type.  However, `->` is
*right-associative*: the type

    Int -> Int -> Int

is equivalent to

    Int -> (Int -> Int)

That is, the first type of function, which takes two Ints, is in
reality a function that takes a single Int as input, and returns as
*output* a function from Ints to Ints! Equipped with this knowledge,
consider the function

> plus :: Int -> Int -> Int
> plus m n = m + n

Thus, whenever we use `plus` we can either pass in both the inputs 
at once, as in

   plus 10 20

or instead, we can *partially* apply the function, by just passing in
only one input

> plusfive :: Int -> Int
> plusfive = plus 5

thereby getting as output a function that is *waiting* for the second
input (at which point it will produce the final result).

> pfivetest :: Test
> pfivetest = plusfive 1000 ~?= 1005

So how does this execute?  Again *substitute equals for equals*

~~~~~{.haskell}
plusfive 1000 == plus 5 1000       {- definition of plusfive -}
              == 5 + 1000          {- unfold plus -}
              == 1005              {- arithmetic -}
~~~~~

Finally, by now it should be pretty clear that `plusn n` is equivalent 
to the partially applied `plus n`.

If you have been following so far, you should know how this behaves.

> doTwicePlus20 :: Int -> Int
> doTwicePlus20 = doTwice (plus 20)

First, see if you can figure out the type.

Next, see if you can figure out how this evaluates.

      doTwicePlus20 0 == doTwice ((plus 20) 0)
                      == plus20 (20 + 0)
                      == 20 + 20 + 0
                      == 40



Anonymous Functions
-------------------

As we have seen, with Haskell, it is quite easy to create function values
that are not bound to any name. For example the expression `plus 1000`
yields a function value that is not bound to any name. 

We will see many situations where a particular function is only used once,
and hence, there is no need to explicitly name it. Haskell provides a 
mechanism to create such *anonymous* functions. For example, 
        
    \x -> x + 1

is an expression that corresponds to a function that takes an argument `x`
and returns as output the value `x + 2`. The function has no name, but we
can use it in the same place where we would write a function. 

> anonTests :: Test
> anonTests = TestList [ (\x -> x + 1) 100 ~?= 101,
>                        doTwice (\x -> x + 1) 100 ~?= 102 ]

Of course, we could name the function if we wanted to

> plus1' :: Int -> Int
> plus1' = \x -> x + 1

Indeed, in general, a function defining equation
        
   f x1 x2 ... xn = e 

is equivalent to

   f = \x1 x2 ... xn -> e 



Infix Operations and Sections 
-----------------------------

In order to improve readability, Haskell allows you to use certain
functions as *infix* operations: a function whose name appears in 
parentheses can be used as an infix operation. My personal favorite 
infix operator is the application function, defined like this:

> ($) :: (a -> b) -> a -> b
> f $ x = f x

Huh? Doesn't seem so compelling does it?  It's just application.

Actually, its very handy because it has different precedence than
normal application.  For example, I can write:

   minus20 $ plus 30 32

Which means the same as:
   
   minus20 (plus 30 32)

That is, Haskell interprets everything after the `$` as one argument to
`minus20`.  I couldn't do this by writing:

   minus20 plus 30 32

Because Haskell would think this was the application of `minus20` to the
three separate arguments `plus`, `30` and `32`.
   


We will see many other interesting infix operators in the course of
the class; indeed many standard operators including that we have used
already are defined in this manner in the standard library.  For
example:

  (:) :: a -> [a] -> [a]

Indeed, Haskell allows you to use *any* function as an infix operator,
simply by wrapping it inside backticks.

> anotherFive :: Int
> anotherFive = 2 `plus` 3

Recall the clone function from earlier.

  clone x n | n == 0    = []
            | otherwise = x : clone x (n-1)

We invoke it in an infix-style, like so:

> threeThirties :: [Int]
> threeThirties = 30 `clone` 3

To further improve readability, Haskell allows you to use *partially
applied* infix operators, ie infix operators with only a single
argument. These are called *sections*. Thus, the section `(+1)` is
simply a function that takes as input a number, the argument missing
on the left of the `+` and returns that number plus `1`.

> anotherFour :: Int
> anotherFour = doTwice (+2) 0

Similarly, the section `(1:)` takes a list of numbers and returns a
new list with `1` followed by the input list.   So

    doTwice (1:) [2..5]

Evaluates to [1,1,2,3,4,5]



Polymorphism
============

We used to `doTwice` to repeat an arithmetic operation, but the actual body
of the function is oblivious to how `f` behaves.

We say that `doTwice` is *polymorphic* in that it works with different
types of values, eg functions that increment integers and concatenate
strings.  This is vital for *abstraction*.  The general notion of
repeating, ie *doing twice* is entirely independent from the types of
the operation that is being repeated, and so we shouldn't have to
write separate repeaters for integers and strings.  Polymorphism
allows us to *reuse* the same abstraction `doTwice` in different
settings.

Of course, with great power, comes great responsibility.

The section `(10 <)` takes an integer and returns `True` 
iff the integer is greater than `10`

> greaterThan10 :: Int -> Bool
> greaterThan10 = (10 <)

However, because the input and output types are different, it doesn't
make sense to try `doTwice greaterThan10`.  A quick glance at the type
of doTwice would tell us this:

   doTwice :: (a -> a) -> a -> a

The `a` above is a *type variable*. The signature above states that
the first argument to `doTwice` must be a function that maps values of
type `a` to `a`, i.e., must produce an output that has the same type
as its input (so that that output can be fed into the function again!).
The second argument must also be an `a` at which point we may are
guaranteed that the result from `doTwice` will also be an `a`. The
above holds for *any* `a` which allows us to safely re-use `doTwice`
in different settings.

Of course, if the input and output type of the input function are
different, as in `greaterThan10`, then the function is incompatible
with `doTwice`.

Ok, to make sure you're following, can you figure out what this does?

> ex1 :: (a -> a) -> a -> a
> ex1 = doTwice doTwice




Acknowledgements: this lecture from cse 230 [1], which itself was 
inspired by a previous version of CIS 552.

[1]: http://cseweb.ucsd.edu/classes/wi11/cse230/lectures.html
