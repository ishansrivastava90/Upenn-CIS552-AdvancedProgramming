Using the State monad for Random Generation
===========================================

> module RandomGen where

> import System.Random (StdGen, mkStdGen, next)
> import State
> import Control.Monad

Random Generation
-----------------

Recall that quickCheck needs to randomly generate values of any type. It turns
out that we can use the state monad to define the `Gen` monad used in the
QuickCheck libary.

First, a brief discussion of pseudo-random number generators. [Pseudo-random
number generators](http://en.wikipedia.org/wiki/Pseudorandom_number_generator)
aren't really random, they just look like it. They are more like functions
that are so complicated that they might as well be random. The nice property
about them is that they are repeatable, if you give them the same *seed* they
will produce the same sequence of "random" numbers.

Haskell has a library for Pseudo-Random numbers called
[System.Random](http://hackage.haskell.org/packages/archive/random/latest/doc/html/System-Random.html).

~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
type StdGen  -- a type for a "standard" random number generator.

-- | Construct a generator from a given seed. Distinct arguments 
-- are likely to produce distinct generators.
mkStdGen :: Int -> StdGen

-- | Returns an Int that is uniformly distributed in a range of at least 30 bits.
next     :: StdGen -> (Int, StdGen)
~~~~~~~~~~~~~~~~~~~~~~~~~~

For example, we can generate a random integer by constructing a random
number generator, calling `next` and then projecting the result.

> testRandom :: Int -> Int
> testRandom i = fst (next (mkStdGen i))

If we'd like to constrain that integer to a specific range (0,n) we
can use `nextBounded`.

> nextBounded :: Int -> StdGen -> (Int, StdGen)
> nextBounded bound s = undefined

> testBounded :: Int -> Int -> Int
> testBounded x = fst . nextBounded x . mkStdGen


QC is defined by class types that can construct random
values. Let's do it first the hard way...


> -- | Extract random values of any type
> class Arb1 a where
>    arb1 :: StdGen -> (a, StdGen)

> instance Arb1 Int where
>    arb1 = next

> instance Arb1 Bool where
>    arb1 = undefined

> testArb1 :: Arb1 a => Int -> a
> testArb1 = fst . arb1 . mkStdGen

What about for pairs?

> instance (Arb1 a, Arb1 b) => Arb1 (a,b) where
>    arb1 = undefined

And for lists?

> instance (Arb1 a) => Arb1 [a] where
>    arb1 s = undefined




Ouch, there's a lot of state passing going on here.

State Monad to the Rescue
-------------------------

Last time, we developed a reusable library for the State monad. Let's
use it to *define* a generator monad for QC.

Our reusable library defines an abstract type for the State monad, and
the following operations for working with these sorts of computations.

~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}

type State s a = ...

instance Monad (State s) where ...

get      :: State s s
put      :: s -> State s ()

state    :: (s -> (a,s)) -> State s a
runState :: State s a -> s -> (a,s)

~~~~~~~~~~~~~~~~~~~~~~~~~~

Now let's define a type for generators, using the State monad.

> type Gen a = State StdGen a 

With this type, we can create a type class similar to the one in the
QuickCheck library.

> class Arb a where 
>   arb :: Gen a

For example, we can use the `state` operation to inject the `next`
function into the `State StdGen a` type.

> instance Arb Int where
>   arb = state next

What if we want a bounded generator?

> bounded :: Int -> Gen Int
> bounded b = undefined

What about random generation for other types?  How does the state
monad help that definition? How does it compare to the version above?

> instance Arb Bool where
>   arb = undefined

> instance (Arb a, Arb b) => Arb (a,b) where
>   arb = undefined


> instance (Arb a) => Arb [a] where
>   arb = undefined

We can also define the `sample` function, which generates 10 random
values. Our version uses a fixed seed, so it will always generate the same
random values. However, the QC version uses the IO monad to get different
seeds.

> sample :: Show a => Gen a -> [a]
> sample gen = evalState (sequence (replicate 10 gen)) (mkStdGen 932497234)


> -- sample :: Gen a => IO ()