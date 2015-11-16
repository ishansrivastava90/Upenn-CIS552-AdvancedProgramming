> module FunEnv2 where

> import Data.Map (Map)
> import qualified Data.Map as Map
> import Control.Monad

> import FunSyntax  


Doing more of the work ourselves
--------------------------------

In this version of the interpreter we will try to avoid using Haskell
functions to represent FUN functions.

Instead, we will just use their definitions. I.e. the name of the parameter
and the body of the functions.

> data Value =
>    IntVal  Int
>  | BoolVal Bool
>  -- new! function values
>  | FunVal Variable Expression Environment

This version has the advantage that we can show a function value!

> instance Show Value where
>     show (IntVal i)    = show i
>     show (BoolVal b)   = show b
>     show (FunVal x e _)  = indented (Fun x e)

The evaluator for binary expressions remains the same.

> evalB :: Bop -> Value -> Value -> Value
> evalB Plus   (IntVal i1) (IntVal i2) = IntVal  (i1 + i2)
> evalB Minus  (IntVal i1) (IntVal i2) = IntVal  (i1 - i2)
> evalB Times  (IntVal i1) (IntVal i2) = IntVal  (i1 * i2)
> evalB Gt     (IntVal i1) (IntVal i2) = BoolVal (i1 > i2)
> evalB Ge     (IntVal i1) (IntVal i2) = BoolVal (i1 >= i2)
> evalB Lt     (IntVal i1) (IntVal i2) = BoolVal (i1 < i2)
> evalB Le     (IntVal i1) (IntVal i2) = BoolVal (i1 <= i2)
> evalB _ _ _ = IntVal 0

As does most of our environment-based evaluator from before.

> type Environment = Map Variable Value
 
> slookup :: Variable -> Environment -> Value
> slookup x m = case (Map.lookup x m) of
>     Just v ->  v
>     Nothing -> (IntVal 0)

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
> eval (Let x e1 e2) s = 
>    let v  = eval e1 s'
>        s' = Map.insert x v s in   
>    eval e2 s'

The difference is in the cases for functions and applications. For functions,
we'll just create a function value straightaway.

> --eval (Fun x e)     s    = eval e s
> eval (Fun x e)    s    = FunVal x e s

For applications, once we've retrieved the function value, and the value of
the argument we can evaluate the function body. But which environment should
we use?

> --eval' (App fun arg) s    = let v = eval arg s 
> --                              p = getParam fun in
> --                               case p of 
> --                                 Just x -> eval fun (Map.insert x v s)
> --                                 _      -> IntVal 0

> eval (App fun arg) s   = let (FunVal x e s')  = (eval fun s) 
>                              v                = (eval arg s)
>                          in eval e (Map.insert x v s')

> getParam :: Expression -> Maybe Variable
> getParam (Fun x e) = Just x
> getParam _         = Nothing


We have the same REPL as before. 

> replE :: IO ()
> replE = do
>    putStr "%> "
>    line <- getLine
>    case parse line of 
>      Just exp -> do
>          putStrLn $ show (eval exp Map.empty)
>          replE
>      Nothing -> putStrLn "what?!?" >> replE

Give this version a try!


> t0 = eval factExp Map.empty

However, not everything from before will work.

> parseAndEval s = liftM (\x -> eval x Map.empty) (parse s)

> p1 = parseAndEval "(fun X -> fun Y -> X) 1 2"
> p2 = parseAndEval "(fun X -> fun Y -> Y) 1 2"

Furthermore, we may notice something strange with this evaluator...

>-- parseAndEval s = liftM (\x -> eval x Map.empty) (parse s)

What is going on in this example?

> --p1 = parseAndEval "let X = 3 in let F = fun Y -> X + Y in let X = 5 in F 4"
