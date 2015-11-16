Monad Transformers
==================

> {-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
>              FlexibleInstances, KindSignatures #-}
>
> module Transformers where
>
> import Control.Monad.Except
> import Control.Monad.State
> import Control.Monad.Writer


What monads have we seen so far?
================================

* []
* State
* Maybe
* IO
* Parsing
* AList


How do we use *multiple* monads at once?
========================================

Today, we will see how monads can be used to write (and compose)
*evaluators* for programming languages.

Let's look at a simple language of division expressions.

> data Expr = Val Int
>           | Div Expr Expr
>           deriving (Show)

Our first evaluator is *unsafe*.

> eval            ::  Expr -> Int
> eval (Val n)   =  n
> eval (Div x y) =  eval x `div` eval y

Here are two terms that we will use as running examples.

> ok  = (Val 1972 `Div` Val 2)
>       `Div` Val 23
> err = Val 2 `Div`
>       (Val 1 `Div`
>        (Val 2 `Div` Val 3))

The first evaluates properly and returns a valid answer, while the
second fails with a divide-by-zero exception.

~~~~~{.haskell}
    *Main> eval ok
    *Main> eval err
~~~~~

We don't like this `eval` because it can blow up with a divide-by-zero
error and stop the whole evaluator.

We can use the `Maybe` type to treat the failure case more gently: a
`Nothing` result means that an error happened somewhere, while a `Just
n` result meant that evaluation succeeded yielding `n`.

> evalMaybe ::  Expr -> Maybe Int
> evalMaybe (Val n)   = return n
> evalMaybe (Div x y) = do
>   n <- evalMaybe x
>   m <- evalMaybe y
>   if m /= 0 then return (n `div` m)
>             else Nothing
 
~~~~~{.haskell}
    *Main> evalMaybe ok
    *Main> evalMaybe err
~~~~~


Error Handling Via Exception Monads
-----------------------------------

The trouble with the above is that it doesn't let us know *where* the
divide by zero occurred. It would be nice to have a real exception
mechanism where we could say `Throw x` (for some descriptive value
`x`, such as a string describing the error), which would percolate up
to the top and tell us what the problem was.

If you think for a moment, you'll realize this is but a small tweak on
the `Maybe` type; all we need is to jazz up the `Nothing` constructor
so that it carries the exception value.

> data Exc a = Raise String
>            | Result a
>            deriving (Show)

Here `Raise` is like `Nothing` but it carries a string denoting what
the exception was. We can make the above a `Monad` much like the
`Maybe` monad.

> instance Monad Exc where
>   -- (>>=)  :: Exc a -> (a -> Exc b) -> Exc b
>   Result x >>= f = f x
>   Raise s  >>= _ = Raise s
>
>   -- return :: a -> Exc a
>   return   = Result

> instance Applicative Exc where
>    pure = return
>    (<*>) = ap

> instance Functor Exc where
>    fmap = liftM

Now we can use our newly minted monad to write a better exception-
throwing evaluator,

> -- evalExc ::  Expr -> Exc Int
> evalExc (Val n)   = return n
> evalExc (Div x y) = do
>  n <- evalExc x
>  m <- evalExc y
>  if (m /= 0)
>    then  return (n `div` m)
>    else  Raise $ errorS x y

where the helper function `errorS` generates the error string.

> errorS y m = "Error dividing " ++ show y ++ " by " ++ show m

~~~~~{.haskell}
    *Main> evalExc ok
    *Main> evalExc err
~~~~~


Counting Operations Using the State Monad
-----------------------------------------

Next, let's stop being so paranoid about errors and instead try to do
some *profiling*. Lets imagine that the `div` operator is very
expensive, and that we would like to *count* the number of divisions
that are performed while evaluating a particular expression.

As you might imagine, our old friend the state monad is going to be
just what we need here! The type of store that we'd like to use is
just the count of number of division operations, and we can store that
in an `Int`.

The `Prof` type here is just a specialized state monad.

> data Prof a = Prof { runProf :: Int -> (a, Int) }
>
> instance Monad Prof where
>    return x = Prof $ \s -> (x, s)
>    pa >>= f = Prof $ \s -> let (x, s') = runProf pa s
>                                pb      = f x
>                            in runProf pb s'
>
> instance Applicative Prof where
>   pure  = return
>   (<*>) = ap
>
> instance Functor Prof where
>   fmap = liftM

We'll need a way of incrementing the counter:

> tickProf :: Prof ()
> tickProf = Prof $ \s -> ((), s+1)

Now we can write a *profiling* evaluator,

> evalST           :: Expr -> Prof Int
> evalST (Val n)   = return n
> evalST (Div x y) = do
>   m <- evalST x
>   n <- evalST y
>   tickProf
>   return (m `div` n)

and observe it at work:

> goProf :: Expr -> IO ()
> goProf e = putStrLn $ "value: " ++ show x ++ ", count: " ++ show s
>            where (x,s) = runProf (evalST e) 0 :: (Int, Int)

~~~~~{.haskell}
    *Main> goProf ok
~~~~~

But... alas!  To get the profiling, we threw out the nifty error
handling that we had put in earlier!!

~~~~~{.haskell}
    *Main> goProf err
~~~~~

Transformers: Making Monads Multitask
=====================================

So, at the moment, it seems that Monads can do many things, but only
*one thing at a time* -- you can either use a monad to do the error-
management plumbing *or* to do the state-manipulation plumbing, but
not at the same time.  Is it too much ask for both? I guess we could
write a *mega-state-and-exception* monad that supports the operations
of both, but that doesn't sound like any fun at all!  Especially
since, if we later decide to add yet another feature, then we would
have to make up yet another mega-monad.

So we will take a different approach, where we will keep *wrapping* --
or "decorating" -- monads with extra features, so that we can take a
simple monad, and then add the Exception monad's features to it, and
then add the State monad's features and so on.

The key to doing this is to define exception handling, state passing,
etc., not as monads, but rather as *type-level functions from monads
to monads.*

This will require a little more work up-front (most of which is done
already in well-designed libraries), but after that we can add new
features in a modular manner.  For example, to get a mega
state-and-exception monad, we will start with a dummy `Identity`
monad, supply it to the `StateT` monad transformer (which yields
state-passing monad) and pass the result to the `ExcT` monad
transformer, which yields the desired mega monad.

(Incidentally, if you are a Python programmer, the above may remind
some of you of the [Decorator Design Pattern][2] and other [Python
Decorators][3].)



Step 1: Describing Monads With Special Features
-----------------------------------------------

The first step to being able to compose monads is to define
typeclasses that describe monads with particular features. For
example, the notion of an *exception monad* is captured by the
typeclass

> class Monad m => MonadErrorString m where
>   throwErrorString :: String -> m a

which describes monads that are also equipped with an appropriate
`throwErrorString` function.  We can make `Exc` an instance of the above
like this:

> instance MonadErrorString Exc where
>   throwErrorString = Raise

(See what happens if you change `Raise` to `throwErrorString` in the
evaluator `evalExc` above. What is the new type of the evaluator?)

Similarly, we can bottle the notion of a *state monad* in a
typeclass...

> class Monad m => MonadStateInt m where
>   runStateInt :: m a -> Int -> m (a, Int)
>   getInt      :: m Int
>   putInt      :: Int -> m ()

which describes monads equipped with execution, extraction, and
modification functions of appropriate types.  We can redefine the
ticking operation to work for any state monad:

> tickStateInt :: MonadStateInt m => m ()
> tickStateInt = do
>   x <- getInt
>   putInt (x + 1)

Naturally, we can make `Prof` an instance of the above:

> instance MonadStateInt Prof where
>   -- runStateInt :: Prof a -> Int -> Prof (a, Int)
>   runStateInt m a = return (runProf m a)
>   getInt          = Prof $ \s -> (s, s)
>   putInt x        = Prof $ \_ -> ((), x)

(Now go back and see what happens when you replace `tickProf` with
`tickStateInt` in `evalProf` above.)



Step 2: Using Monads With Special Features
------------------------------------------

Armed with these two typeclasses, we can write our exception-throwing,
step-counting evaluator quite easily:

> evalMega :: (MonadErrorString m, MonadStateInt m) => Expr -> m Int
> evalMega (Val n)   = return n
> evalMega (Div x y) = do
>   n <- evalMega x
>   m <- evalMega y
>   if (m == 0)
>     then throwErrorString $ errorS n m
>     else do
>       tickStateInt
>       return (n `div` m)
 
Note that it is simply the combination of the two evaluators from
before -- we use the error handling from `evalExc` and the profiling
from `evalProf`.

Meditate for a moment on the type of above evaluator; note that it
works with *any monad* that is both a exception- and a state- monad!

But where do we *get* monads that are both state-manipulating and
exception-handling?

One answer is that we can just define one!

Exercise: define one! Write a datatype with instances for
`Monad` (and `Functor` and `Applicative`), `MonadErrorString`,
and `MonadStateInt`. Make sure that `evalMega` works with
your monad. Hint: Start with either `Prof` or `Exc` and build
out the extra pieces.




















In the end, making your own mega-monad is a bit
disappointing, since we've already defined the state- and
exception-handling functionality separately.

A better answer is to build them piece by piece.  We'll do this by
defining some type level functions that will *add* state manipulation
or exception handling to *any* pre-existing monad.



Step 3: Adding Features to Existing Monads
------------------------------------------

To add new features to existing monads, we use *monad transformers* --
type operators `t` that map each monad `m` to a monad `t m`.


**A Transformer For Exceptions**

Consider the following datatype declaration (using the record syntax
we've seen before):

> data ExcT m a = MkExc { runExcT :: m (Exc a) }

> -- MkExc   :: m (Exc a) -> ExcT m a
> -- runExcT :: ExcT m a -> m (Exc a)

It is simply a type with two parameters -- the first is a monad `m`,
which we will wrap around the exception monad `Exc a`. In other words,
the `ExcT m a` simply *injects* the `Exc a` monad into the value slot
of the `m` monad. By convention, the names of monad transformers end
with `T`.

Now, the real trick is twofold, we ensure that if `m` is a monad, then
transformed `ExcT m` is an *exception monad* -- that is, a `MonadErrorString`.

First, we show the transformer output is a monad:

> instance Monad m => Monad (ExcT m) where
>   -- return :: a -> ExcT m a
>   return x = MkExc $ return $ Result x

>   -- (>>=) :: ExcT m a -> (a -> ExcT m b) -> ExcT m b
>   -- p :: m (Exc a)
>   MkExc p >>= f  =  MkExc $ do ea <- p    -- ea :: Exc a
>                                case ea of
>                                  Raise s  -> return $ Raise s
>                                  Result a -> runExcT $ f a

> instance Monad m => Applicative (ExcT m) where
>    pure = return
>    (<*>) = ap

> instance Monad m => Functor (ExcT m) where
>    fmap = liftM

And next we ensure that the transformer is an exception monad by
equipping it with `throwErrorExc`

> instance Monad m => MonadErrorString (ExcT m) where
>   -- throwErrorExc :: String -> ExcT m a
>   throwErrorString = MkExc . return . Raise



**A Transformer For State**

Next, we will build a transformer for the state monad, following more
or less the same recipe as for exceptions. Here is the type for the
transformer:

> newtype StateIntT m a =  MkStateIntT { runStateIntT :: Int -> m (a, Int) }

> -- MkStateIntT  :: (Int -> m (a,Int)) -> StateIntT m a
> -- runStateIntT :: StateIntT m a -> (Int -> m (a,Int))

Thus, in effect, the enhanced monad is a variant of the ordinary state
monad where a starting store is mapped to an _action_ in the monad `m`
that returns both a result of type `a` and a new store.

(Note that the monad transformer is *not* this:

    newtype StateIntT m a = Mk (m (Int -> (a, Int)))

That is, it is not an `m` action yielding a store transformation.
Why is this not what we want?)

Next, we declare that the transformer's output is a monad:

> -- newtype StateIntT m a =  MkStateIntT { runStateIntT :: Int -> m (a, Int) }

> instance Monad m => Monad (StateIntT m) where
>   -- return :: a -> StateIntT m a
>   return x = MkStateIntT $ \s -> return (x,s)
>
>   -- (>>=) :: StateIntT m a -> (a -> StateIntT m b) -> StateIntT m b
>   -- p :: StateIntT m a
>   -- f :: a -> StateIntT m b
>   p >>= f = MkStateIntT $ \s -> do (r,s') <- runStateIntT p s
>                                    runStateIntT (f r) s'

> instance Monad m => Applicative (StateIntT m) where
>   pure  = return
>   (<*>) = ap

> instance Monad m => Functor (StateIntT m) where
>   fmap  = liftM

And finally we declare that the transformer is a state monad by
equipping it with the operations from `MonadStateInt`:

> instance Monad m => MonadStateInt (StateIntT m) where
> --runStateIntT :: StateIntT m a -> Int -> StateIntT m (a, Int)
>   runStateInt f s = MkStateIntT $ \s' -> do -- need: m ((a,Int),Int)
>                                             (r,s'') <- runStateIntT f s
>                                             return ((r,s''),s')

> --getInt :: StateIntT m Int
>   getInt = MkStateIntT getIt
>     where --putIt :: Int -> m (Int, Int)
>           getIt s = return (s, s)

> --putInt :: Int -> StateIntT m ()
>   putInt s = MkStateIntT putIt
>     where --putIt :: Int -> m ((), Int)
>           putIt _ = return ((), s)



Step 4: The `lift` operation
----------------------------

Of course, we must make sure that the original features of the monads
are not lost in the transformed monads.  The key ingredient of a
transformer is that it must have a function `lift` that takes an `m`
action and turns it into a `t m` action.  This will allow us to
transfer operations from the old monad into the transformed monad: any
operation on the input monad `m` can be directly lifted into an action
on the transformed monad, and so the transformation *preserves* all
the operations on the original monad.

~~~{.haskell}
    class MonadTrans t where   -- from Control.Monad.Trans (among other places)
      lift :: Monad m => m a -> t m a
~~~

It is easy to formally state that `ExcT` is a bona-fide transformer by
making it an instance of the `MonadTrans` class:

> -- > data ExcT m a = MkExc { unMkExc :: m (Exc a) }

> instance MonadTrans ExcT where
>   -- lift :: Monad m => m a -> ExcT m a
>   lift = MkExc . lift_ where
>     -- lift_  :: (Monad m) => m a -> m (Exc a)
>     lift_ mt = do x <- mt
>                   return (Result x)

Similarly, for the state monad transformer:

> instance MonadTrans StateIntT where
>   -- Monad m => m a -> StateIntT m a
>   -- ma :: m a
>   -- MkStateIntT :: (Int -> m (a,Int)) -> StateIntT m a
>   lift ma = MkStateIntT $ \s -> do r <- ma
>                                    return (r,s)


We will see how `lift` is used in a moment.  But first, a quick
digression...


** What is the Type of a Type? **

The expression `t m a` in the definition of the `MonadTrans` class
above deserves a little unpacking: `t`, `m`, and `a` are all variables
standing for types, but each of them stands for a different *sort* of
type:

- `a` stands for an ordinary type, like `Int` or `Char`

- `m` stands for a *type operator* like `Maybe` or `[]` that takes an
  ordinary type as its argument

- `t` stands for a *higher-order type operator* like `ExcT` that takes
  two arguments: a type operator and an ordinary type!

To keep all this straight, Haskell uses a system of *kinds* to
classify different sorts of type expressions, just as types themselves
are used to classify different sorts of values.

- Ordinary types like `Int` and `Char` have kind `*`.  These are the
only types that are *inhabited* by Haskell values.

- Simple type operators like `Maybe` or `[]` have kind `* -> *` -- that
is, when applied to an ordinary type, they yield an ordinary type.
(For example, `Maybe` itself doesn't describe any values, but when
`Maybe` is applied to `Int` we get the ordinary type `Maybe Int`,
which describes values like `Just 4`.)

- Higher-order type operators like `ExcT` have types like
`(* -> *) -> * -> *`.  This says that, when `ExcT` is applied to a type
operator like `[]`, it yields a type operator that, when applied to an
ordinary type (like `Char`) yields an ordinary type (`ExcT [] Char`)
describing values (such as `MkExc [Result 'x', Result 'y']`).

You can annotate type variables with their kinds, if you like, using
the same decoration syntax as we have been using to annotate value
bindings with their types:

~~~~~{.haskell}
     class MonadTrans ( t :: (* -> *) -> * -> * ) where
       lift :: Monad m => m a -> t m a
~~~~~


Step 5: Preserving Old Features of Monads
-----------------------------------------

Using `lift`, we can ensure that, if a monad was already an
"exceptional" monad, then the result of the state transformer is too:

> instance MonadErrorString m => MonadErrorString (StateIntT m) where
>   -- throwErrorString :: String -> StateIntT m a
>   -- throwErrorString :: String -> m a
>   -- lift :: m a -> StateIntT m a
>   throwErrorString = lift . throwErrorString

Similarly, if a monad was already a state-manipulating monad, then the
result of the exception-transformer is *also* a state-manipulating
monad:

> instance MonadStateInt m => MonadStateInt (ExcT m) where
>   getInt = lift getInt
>
>   putInt = lift . putInt
>
>   -- runStateInt :: (MonadStateInt m) => m a -> Int -> m (a,Int)
>   -- here, runStateInt :: ExcT m a -> Int -> ExcT m (a, Int)
>   -- m :: m (Exc a)
>   -- s :: Int
>   -- data ExcT m a = MkExc { runExcT :: m (Exc a) }
>   runStateInt m s = MkExc $ do (ea,s')  <- runStateInt (runExcT m) s
>                                case ea of
>                                  Result a -> return $ Result (a,s')
>                                  Raise e  -> return $ Raise e


Step 6: Whew! Put It Together and Run
-------------------------------------

Finally, we can put all the pieces together and run the transformers.
We can also *order* the transformations differently (which can have
different consequences on the output, as we will see).

> evalExSt :: Expr -> StateIntT Exc Int
> evalExSt = evalMega
>
> evalStEx :: Expr -> ExcT Prof Int
> evalStEx = evalMega

We can run this as follows:

> goExSt :: Expr -> IO ()
> goExSt e = putStr $ pr (evalExSt e) where
>     pr :: StateIntT Exc Int -> String
>     pr f = case runStateIntT f 0 of
>                      Raise s         -> "Raise: " ++ s ++ "\n"
>                      Result (v, cnt) -> "Count: " ++ show cnt ++ "\n" ++
>                                         "Result: " ++ show v ++ "\n"

> goStEx :: Expr -> IO ()
> goStEx e = putStr $ pr (evalStEx e) where
>    pr :: ExcT Prof Int -> String
>    pr f = "Count: " ++ show cnt ++ "\n" ++ show r ++ "\n"
>      where (r, cnt) = runProf (runExcT f) 0
 
~~~~~{.haskell}
    *Main> goExSt ok
    *Main> goExSt err
    *Main> goStEx ok
    *Main> goStEx err
~~~~~


The Monad Transformer Library
=============================

While it is instructive to roll our own versions of things as we've
done here, in practice you should reuse as much as you can from
standard libraries.


Error Monads and Transformers
-----------------------------

The above sauced-up exception-tracking version of `Maybe` already
exists as the standard type [Either][1].

~~~~~{.haskell}
    *Main> :info Either
    data Either a b = Left a | Right b     -- Defined in Data.Either
~~~~~

The `Either` type is a generalization of our `Exc` type, where the
exception is polymorphic, rather than just being a `String`. In other
words the hand-rolled `Exc a` corresponds to the standard `Either
String a` type.

The standard [MonadError][6] typeclass corresponds to the `MonadErrorString`
developed above.  (Note that it also provides a `catch` operation.)

~~~~~{.haskell}
    *Main> :info MonadError
    class (Monad m) => MonadError e m | m -> e where
      throwError :: e -> m a
      catchError :: m a -> (e -> m a) -> m a
            -- Defined in Control.Monad.Except.Class

    instance Monad m => MonadError e (ExceptT e m)
      -- Defined in Control.Monad.Except

    instance MonadError e (Either e)
      -- Defined in Control.Monad.Except

    instance MonadError IOException IO -- Defined in Control.Monad.Except
~~~~~

Note that `Either String` is an instance of `MonadError String` just as
`Exc` is an instance of `MonadErrorString`. Finally, the `ExceptT e` transformer
corresponds to the `ExcT` transformer developed above, and its output
is guaranteed to be an instance of `MonadError e`.


State Monads and Transformers
-----------------------------

Similarly, the `StateInt` monad that we wrote above is a simplified form of
the more general [State][4] monad.

~~~~~{.haskell}
    *Main> :info State
    type State s = StateT s Data.Functor.Identity.Identity
            -- Defined in Control.Monad.Trans.State.Lazy
~~~~~

The standard [MonadState][5] typeclass is the full version of our
`MonadStateInt` rendered above.

~~~~~{.haskell}
    *Main> :info MonadState
    class (Monad m) => MonadState s m | m -> s where
      get :: m s
      put :: s -> m ()
            -- Defined in Control.Monad.State.Class

    instance (Monad m) => MonadState s (StateT s m)
      -- Defined in Control.Monad.State.Lazy

    instance MonadState s (State s)
      -- Defined in Control.Monad.State.Lazy
~~~~~

Note that `State s` is already an instance of `MonadState`, just as
`Prof` is an instance of `MonadStateInt`. Finally, the `StateT s` transformer
corresponds to the `StateIntT` transformer developed above and its output is
guaranteed to be an instance of `MonadState s`.

Thus, if we stick with the standard libraries, all we have to write is
this:

> tick :: (MonadState Int m) => m ()
> tick = do { n <- get; put (n+1) }

> eval1 :: (MonadError String m, MonadState Int m) =>
>           Expr -> m Int
> eval1 (Val n)   = return n
> eval1 (Div x y) = do n   <- eval1 x
>                      m   <- eval1 y
>                      if m == 0
>                        then throwError $ errorS y m
>                        else do tick
>                                return  $ n `div` m

> evalSE :: Expr -> StateT Int (Either String) Int
> evalSE = eval1

~~~~~{.haskell}
    *Main> runStateT (evalSE ok) 0
    *Main> runStateT (evalSE err) 0
~~~~~

As above, we can also stack them in the other order,

> evalES :: Expr -> ExceptT String (State Int) Int
> evalES = eval1

which will yield a different result:

~~~~~{.haskell}
    *Main> runState (runExceptT (evalES ok)) 0
    *Main> runState (runExceptT (evalES err)) 0
~~~~~



Tracing Operations Via Logger Monads
====================================

As a last example of monad transformers, we will spice up our
computations to also *log* messages about what is happening (a pure
variant of the usual method where we just print the messages to the
screen).  This can be done with the standard [Writer][7] monad, which
supports a `tell` action that logs the string you want (and allows you
to later view the entire log of the computation).

To accomodate logging, we juice up our evaluator directly as follows:

> eval2 :: (MonadError String m, MonadState Int m,
>           MonadWriter String m) =>
>           Expr -> m Int
> eval2 v =
>   case v of
>     Val n   -> do tell $ msg v n
>                   return n
>     Div x y -> do n <- eval2 x
>                   m <- eval2 y
>                   if m == 0
>                     then throwError $ errorS y m
>                     else do tick
>                             tell $ msg v (n `div` m)   -- new
>                             return  $ n `div` m

The `msg` function is simply:

> msg :: (Show a, Show b) => a -> b -> String
> msg t r = "term: " ++ show t ++ ", yields " ++ show r ++ "\n"

Note that the only addition to the previous evaluator is the `tell`
operations! We can run the above using

> evalWSE :: Expr -> WSE Int
> evalWSE = eval2

where `WSE` is a type abbreviation:

> type WSE a = WriterT String (StateT Int (Either String)) a

That is, we simply use the `WriterT` transformer to decorate the underlying
monad carrying the state and exception information.

~~~~~{.haskell}
    *Main> runStateT (runWriterT (evalWSE ok)) 0
    *Main> runStateT (runWriterT (evalWSE err)) 0
~~~~~

The results are a bit unreadable, but we can write our own pretty-printer...

> ppr :: Show a => WSE a -> IO ()
> ppr m = putStrLn $
>         case runStateT (runWriterT m) 0 of
>            Left s            -> "Error: " ++ s
>            Right ((v, w), s) -> "Log:\n"  ++ w       ++ "\n" ++
>                                 "Count: " ++ show s  ++ "\n" ++
>                                 "Value: " ++ show v  ++ "\n"

after which we get:

~~~~~{.haskell}
    *Main> ppr $ evalWSE ok
    *Main> ppr $ evalWSE err
~~~~~



















*How come we didn't get any log in the error case?*

Once again, the answer lies in the *order* in which we compose the
transformers; since the error wraps the log, if the computation fails,
the log gets thrown away. Instead, we can just wrap the other way
around...

> type ESW a = ExceptT String (WriterT String (State Int)) a
>
> evalESW :: Expr -> ESW Int
> evalESW = eval2

after which, everything works just fine!

> instance Show a => Show (ESW a) where
>   show m = "Log:\n"  ++ log ++ "\n" ++
>            "Count: " ++ show cnt ++ "\n" ++
>            result
>     where ((res, log), cnt) = runState (runWriterT (runExceptT m)) 0
>           result   = case res of
>                        Left s -> "Error: " ++ s
>                        Right v -> "Value: " ++ show v

~~~~~{.haskell}
    *Main> evalESW err
~~~~~

[1]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t:Either
[2]: http://oreilly.com/catalog/hfdesignpat/chapter/ch03.pdf
[3]: http://en.wikipedia.org/wiki/Python_syntax_and_semantics#Decorators
[4]: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#v:state
[5]: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Class.html#t:MonadState
[6]: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Error-Class.html#t:MonadError
[7]: http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-Writer-Lazy.html#t:Writer
