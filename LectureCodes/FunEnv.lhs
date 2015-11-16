Interpreters for functional programming languages
=========================================================

Interpreters
------------

> {-# LANGUAGE FlexibleContexts, FlexibleInstances, OverlappingInstances #-}
> module FunEnv where

> import Data.Map (Map)
> import qualified Data.Map as Map
> import Control.Monad.State

So far, we have seen how to write an interpreter for a simple
imperative language with While loops. But what about something closer
to home?  What if you wanted to write an interpreter for Haskell?  Or
for Ocaml?  Or Scheme? Or Clojure?

Today we'll implement a few different interpreters for small purely functional
language that captures the essence of these languages. This language is called
FUN because that is what we are going to have.

The abstract syntax for this language is in the module
[FunSyntax](FunSyntax.lhs), together with a parser and pretty-printer
for its concrete syntax.

> import FunSyntax  


An Interpreter for FUN
----------------------

In some sense, writing an interpreter for FUN is *easier* than writing
one for WHILE. Recall that in our interpreter for the WHILE language,
we leaned on Haskell structures to implement similar functionality for
the object language.  For example, we used Haskell integer and boolean
values to represent WHILE's integer and boolean values; and we used
Haskell's plus operator to implement WHILE's Plus.

We'll use this trick again to implement higher-order functions.  To
extend the datatype of values with function values, we'll just use
Haskell functions.  Just like we mapped WHILE integers to Haskell
integers, we'll map FUN functions to Haskell functions!

> data Value =
>    IntVal  Int
>  | BoolVal Bool
>  -- new! function values
>  | FunVal (Value -> Value)

> instance Show Value where
>     show (IntVal i)  = show i
>     show (BoolVal b) = show b
>     show (FunVal _)  = "<function>"   -- can't show functions

Now, on to the interpreter itself.  We can reuse some parts of the WHILE
interpreter, such as the evaluator for boolean operators...

> evalB :: Bop -> Value -> Value -> Value
> evalB Plus   (IntVal i1) (IntVal i2) = IntVal  (i1 + i2)
> evalB Minus  (IntVal i1) (IntVal i2) = IntVal  (i1 - i2)
> evalB Times  (IntVal i1) (IntVal i2) = IntVal  (i1 * i2)
> evalB Gt     (IntVal i1) (IntVal i2) = BoolVal (i1 > i2)
> evalB Ge     (IntVal i1) (IntVal i2) = BoolVal (i1 >= i2)
> evalB Lt     (IntVal i1) (IntVal i2) = BoolVal (i1 < i2)
> evalB Le     (IntVal i1) (IntVal i2) = BoolVal (i1 <= i2)
> evalB _ _ _ = IntVal 0

... and the use of a finite map to keep track of the values of
variables:

> type Environment = Map Variable Value
 
> slookup :: Variable -> Environment -> Value
> slookup x m = case (Map.lookup x m) of
>     Just v ->  v
>     Nothing -> (IntVal 0)

Note however, that unlike WHILE, the state monad is not exactly right.
If we used the state monad, our evaluator would have type

    eval :: Expression -> State Environment Value

which is essentially equivalent to:

    eval :: Expression -> Environment -> (Value, Environment)

That is, every call to `eval` returns a new Environment.  However, the
language we are interpreting is _pure_ -- there is no way to change
the value of a FUN variable -- so the fact that we get access to the
resulting `Environment` is of no use to us.

So, instead, we'll write our interpreter using the following simpler type:

     eval :: Expression -> Environment -> Value

We'll pass the current value of the store as an additional argument to
the evaluator. When we extend the store, the extension is only
temporary -- and it follows the recursive structure of the evaluator
itself... which just so happens to match the lexical structure of the
term. Nifty!

> eval :: Expression -> Environment -> Value
> eval (Var x)       s = slookup x s
> eval (IntExp i)    s = IntVal i
> eval (BoolExp i)   s = BoolVal i
> eval (Op o e1 e2)  s = (evalB o) (eval e1 s) (eval e2 s) 
> eval (If e1 e2 e3) s = 
>    let v1 = eval e1 s in
>    case v1 of
>      BoolVal b -> if b then eval e2 s else eval e3 s
>      _ ->  (IntVal 0)

We need to update the variable mapping when we evaluate functions.
The value of a function expression is a Haskell function (whose
argument will be used to provide the value of the bound variable when
evaluating the body of the function).  

> eval (Fun x e)     s    = undefined


(Make sure you understand this bit -- it's the crux of the matter!)

We can then evaluate a function application by applying the function
value to its argument.

> eval (App fun arg) s    = undefined


Finally, in the case of a recursive definition for x, we need to
evaluate the right-hand-side using a store that maps x to the value
that we are currently computing?!  Whoa!  We're using a Haskell
recursive definition to define recursive definitions in FUN.

> eval (Let x e1 e2) s = undefined

Let's give it a try!

> t0 = (eval factExp) Map.empty

> parseAndEval s = liftM (\x -> eval x Map.empty) (parse s)

> p1 = parseAndEval "(fun X -> fun Y -> X) 1 2"
> p2 = parseAndEval "(fun X -> fun Y -> Y) 1 2"

Here's another way to write factorial of 5, based on the Y combinator:

> yExp = "(fun F -> (fun X -> F (X X)) (fun X -> F (X X)))"
> fExp = "(fun FACT -> fun X -> if X <= 0 then 1 else X * FACT (X - 1)))"
> p3 = parseAndEval ("(" ++ yExp ++ " " ++ fExp ++ " 5)")

In fact, we can even create a simple REPL (read-eval-print loop) for
evaluating expressions and showing their results.

> replE :: IO ()
> replE = do
>    putStr "%> "
>    line <- getLine
>    case parse line of 
>      Just exp -> do
>          putStrLn $ show (eval exp Map.empty)
>          replE
>      Nothing -> putStrLn "what?!?" >> replE


The Reader Monad
================

One small criticism of what we've got now is that all the "environment
plumbing" we have to do is a bit boring.  Fortunately, we can still
write our interpreter in monadic style; we just need to use a
different monad! The `Reader` Monad is a type `r -> a`, which captures
the idea of reading shared values from a common environment.  (There
is a standard implementation of this monad the library
`Control.Monad.Reader`, including a monad transformer version of
it. However, we'll build it up ourselves here.)

> type Reader a = (->) Environment a

> instance Monad ((->) Environment) where
>   -- return :: a -> Environment -> a
>   return x = undefined
>   -- (>>=) :: (Environment -> a) -> (a -> (Environment -> b)) 
>   --                             -> (Environment -> b)
>   m >>=  k = undefined

With this monad, we can pass the environment around implicitly where it
doesn't matter, yet still refer to it explicitly when necessary.

Now let's rewrite the evaluator above using this monad.

> evalM :: Expression -> (Environment -> Value)
> evalM (Var x)       = slookup x 

> evalM (IntExp i)    = return (IntVal i)

> evalM (BoolExp i)   = return (BoolVal i)

> evalM (Op o e1 e2)  = undefined

> evalM (If e1 e2 e3) = undefined

> evalM (Fun x e)     = undefined

> evalM (App fun arg) = undefined

> evalM (Let x e1 e2) = undefined



