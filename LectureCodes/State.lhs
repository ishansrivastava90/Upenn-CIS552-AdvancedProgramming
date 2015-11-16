A Generic State Transformer
===========================

Since state is a handy thing to have, the standard library includes a
[module][1] `Control.Monad.State` that defines a parameterized version
of the state-transformer monad.  This file is a simplified version of
that library.

We will only allow clients to use the functions declared below.

> {-# LANGUAGE InstanceSigs #-}

This module includes an explicit export list.  Only the types and functions
listed below will be visible to clients of the module.  Furthermore, the type
`State` is exported abstractly. Clients of this module will not have access to
the constructor for this type, nor be able to pattern match it.

> module State (State,  get, put, state, runState, evalState, execState, modify) where
> import Control.Monad (ap,liftM)

The type definition for a generic state transformer is very simple:

> newtype State s a = S { runState :: s -> (a, s) }

We'll export the S constructor as the function `state`:

> state :: (s -> (a,s)) -> State s a
> state = S

It is a parameterized state-transformer monad where the state is
denoted by type `s` and the return value of the transformer is the
type `a`. We make the above a monad by declaring it to be an instance
of the `Monad` typeclass

> instance Monad (State s) where
>   -- return :: a -> State s a
>   return x   =  S $ \s -> (x,s)
>   -- (>>=) :: State s a -> (a -> State s b) -> State s b
>   st >>= f   =  S (\s -> let (x,s') = runState st s in runState (f x) s')

Starting with GHC 7.10, all monads must also be a member of `Functor` and
`Applicative`. However, we can use functions from Control.Monad to define
these instances in a generic way.  (You might try to redefine them yourself
for fun!)

> instance Functor (State s) where
>   fmap = liftM

> instance Applicative (State s) where
>   pure  = return
>   (<*>) = ap


There are two other ways of evaluating the state monad. The first only
returns the final result,

> evalState :: State s a -> s -> a
> evalState st s = fst (runState st s)

and the second only returns the final state.

> execState :: State s a -> s -> s
> execState st = snd . runState st


Accessing and Modifying State
-----------------------------

Since our notion of state is generic, it is useful to write `get` and
`put` functions with which one can *access* and *modify* the state. We
can easily `get` the *current* state via


> get :: State s s
> get = S (\ s -> (s, s) )



That is, `get` denotes an action that leaves the state unchanged but
returns the state itself as a value. Note that although `get` *does
not* have a function type (unless you peek under the covers of
`State`), we consider it a monadic "action".

Dually, to *update* the state to some new value `s'` we can write
the function

> put :: s -> State s ()
> put s' = S (\ _ -> ( ()  , s'))

which denotes an action that ignores (i.e., blows away) the old state
and replaces it with `s'`. Note that the `put s'` is an action that
itself yields nothing (that is, merely the unit value).


For convenience, there is also the `modify` function that maps an old state to
a new state *inside* a state monad. The old state is thrown away.

> modify :: (s -> s) -> State s ()
> modify f = state (\ s -> ( (), f s ))

> modify' f = do
>     s <- get
>     put (f s)



[1]: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-State-Lazy.html#g:2
