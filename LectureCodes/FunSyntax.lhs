> {-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

The abstract and concrete syntax of FUN, a small functional
programming language.

> module FunSyntax where

> import Text.PrettyPrint (Doc, (<+>),($$),(<>))
> import qualified Text.PrettyPrint as PP

> import qualified Data.Char as Char

> import Parsers (Parser)
> import qualified Parsers as Parser
> import Control.Applicative(Alternative(..))
> import Control.Monad

> import Test.QuickCheck

The syntax of the language is a little like that of the WHILE programming
language.

> type Variable = String

> data Bop = 
>    Plus     -- +  :: Int  -> Int  -> Int
>  | Minus    -- -  :: Int  -> Int  -> Int
>  | Times    -- *  :: Int  -> Int  -> Int
>  | Gt       -- >  :: Int -> Int -> Bool 
>  | Ge       -- >= :: Int -> Int -> Bool
>  | Lt       -- <  :: Int -> Int -> Bool
>  | Le       -- <= :: Int -> Int -> Bool
>     deriving (Eq, Show, Enum)

Like Haskell (and OCaml), and unlike WHILE, this language does not
distinguish between expressions and statements: everything is an
expression. For that reason, we add 'If' as a new expression form to
the expressions that we already had in WHILE (variables, constants and
binary operators).

> data Expression =
>  -- stuff shared with WHILE...
>    Var Variable                        -- uppercase strings 'X'     
>  | IntExp  Int                         -- natural numbers   '0' ..
>  | BoolExp Bool                        -- 'true' or 'false'
>  | Op  Bop Expression Expression       -- infix binary operators 'e1 + e2'
>  -- new stuff...
>  | If Expression Expression Expression -- if expressions, 'if X then Y else Z'
>  -- interesting new stuff...
>  | Fun Variable Expression             -- anonymous function,   'fun X -> e'
>  | App Expression Expression           -- function application, 'e1 e2'
>  | Let Variable Expression Expression  -- (recursive) binding,  'let F = e in e'
>     deriving (Show, Eq)

The "fun" stuff is in the last three lines. We want to be able to
create (anonymous) first-class functions, apply them to arguments, and
use them in recursive definitions. For example, our good friend the
factorial function might be written in the concrete syntax of FUN as:

      let FACT = fun X ->   
                  if X <= 1 then 1 else X * FACT (X - 1)
      in FACT 5

and represented in the abstract syntax as:

> factExp :: Expression
> factExp = Let "FACT" (Fun "X" (If 
>                            (Op Le (Var "X") (IntExp 1)) (IntExp 1)
>                              (Op Times (Var "X") (App (Var "FACT") (Op Minus (Var "X") (IntExp 1))))))
>          (App (Var "FACT") (IntExp 5))


FUN Parser
----------

> varP :: Parser Variable
> varP  = wsP (some (Parser.satP Char.isUpper))

> boolP :: Parser Bool
> boolP =  (wsP $ Parser.string "true" >> return True) 
>      <|> (wsP $ Parser.string "false" >> return False)


> -- only natural numbers for simplicity (no negative numbers)
> intP :: Parser Int
> intP = Parser.oneNat 

> opP :: Parser Bop 
> opP =  (wsP $ Parser.string "+"  >> return Plus) 
>    <|> (wsP $ Parser.string "-"  >> return Minus)
>    <|> (wsP $ Parser.string "*"  >> return Times)
>    <|> (wsP $ Parser.string ">=" >> return Ge)
>    <|> (wsP $ Parser.string "<=" >> return Le)
>    <|> (wsP $ Parser.string ">"  >> return Gt)
>    <|> (wsP $ Parser.string "<"  >> return Lt)

> varExprP  = Var     `liftM` wsP varP
> boolExprP = BoolExp `liftM` wsP boolP
> intExprP  = IntExp  `liftM` wsP intP

> ifP = do 
>     wsP $ Parser.string "if"
>     e1 <- exprP
>     wsP $ Parser.string "then"
>     e2 <- exprP
>     wsP $ Parser.string "else"
>     e3 <- exprP
>     return (If e1 e2 e3)

> funP = do
>     wsP $ Parser.string "fun"
>     x <- varP
>     wsP $ Parser.string "->"
>     e <- exprP
>     return (Fun x e)

> letrecP = do
>     wsP $ Parser.string "let"
>     x <- varP
>     wsP $ Parser.string "="
>     e1 <- exprP
>     wsP $ Parser.string "in"
>     e2 <- exprP
>     return (Let x e1 e2)

> -- we use chainl for associativity and precedence
> exprP :: Parser Expression
> exprP = sumP where
>   sumP    = prodP `Parser.chainl` wsP (liftM Op addOp)
>   prodP   = compP `Parser.chainl` wsP (liftM Op mulOp)
>   compP   = appP  `Parser.chainl` wsP (liftM Op cmpOp)
>   appP    = factorP >>= \x -> 
>                (some factorP >>= \vs -> return (foldl App x vs))
>                <|> return x
>   factorP = wsP (Parser.parenP '(' exprP ')') <|> baseP
>   baseP   = boolExprP <|> intExprP <|> ifP <|> funP <|> letrecP 
>          <|> varExprP 

> -- only succeeds for operators at a particular precedence level
> opLevel :: Int -> Parser (Expression -> Expression -> Expression)
> opLevel l = do x <- opP 
>                if level x == l then (return $ Op x) else fail ""

> -- parse something then consume all following whitespace
> wsP :: Parser a -> Parser a
> wsP p = do x <- p 
>            many (Parser.satP Char.isSpace)
>            return x

> parse :: String -> Maybe Expression
> parse s = case Parser.doParse exprP s of 
>             (exp,_):_ -> Just exp
>             _ -> Nothing

> addOp :: Parser Bop
> addOp = plus `Parser.chooseP` minus
>          where plus  = Parser.char '+' >> return Plus
>                minus = Parser.char '-' >> return Minus                
>
> mulOp :: Parser Bop
> mulOp = times 
>          where times = Parser.char '*' >> return Times                

> 
> cmpOp :: Parser Bop
> cmpOp = le `Parser.chooseP` lt `Parser.chooseP` ge `Parser.chooseP` gt
>          where lt  = Parser.char '<' >> return Lt
>                gt  = Parser.char '>' >> return Gt
>                le  = Parser.string "<=" >> return Le
>                ge  = Parser.string ">=" >> return Ge


FUN Printer 
------------

> instance PP Bop where
>   pp Plus   =  PP.text "+"
>   pp Minus  =  PP.text "-"
>   pp Times  =  PP.text "*"
>   pp Gt     =  PP.text ">"
>   pp Ge     =  PP.text ">="
>   pp Lt     =  PP.text "<"
>   pp Le     =  PP.text "<="

> class PP a where
>   pp :: a -> Doc

> display :: PP a => a -> String
> display = show . pp

> instance PP Variable where
>  pp s = PP.text s

> instance PP Expression where
>  pp (Var x)  = PP.text x
>  pp (IntExp x)   = PP.text (show x)
>  pp (BoolExp x)  = if x then PP.text "true" else PP.text "false"
>  pp e@(Op _ _ _) = ppPrec 0 e 
>  pp (If e s1 s2) = 
>    PP.vcat [PP.text "if" <+> pp e <+> PP.text "then",
>         PP.nest 2 (pp s1), 
>         PP.text "else",
>         PP.nest 2 (pp s2) ]
>  pp e@(App _ _) = ppPrec 0 e
>  pp (Fun x e)   = 
>   PP.hang (PP.text "fun" <+> pp x <+> PP.text "->") 2 (pp e)
>  pp (Let x e1 e2) = 
>   PP.vcat [PP.text "let" <+> pp x <+> PP.text "=",              
>         PP.nest 2 (pp e1),
>         PP.text "in",
>         PP.nest 2 (pp e2) ]

> ppPrec n (Op bop e1 e2) =
>     parens (level bop < n) $
>           ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2 
> ppPrec n (App e1 e2) = 
>     parens (levelApp < n) $
>           ppPrec levelApp e1 <+> ppPrec (levelApp + 1) e2
> ppPrec n e@(Fun _ _) = 
>     parens (levelFun < n) $ pp e
> ppPrec n e@(If _ _ _) = 
>     parens (levelIf < n) $ pp e
> ppPrec n e@(Let _ _ _) = 
>     parens (levelLet < n) $ pp e
> ppPrec _ e' = pp e'
> parens b = if b then PP.parens else id

> -- emulate the C++ precendence-level table
> level :: Bop -> Int
> level Plus   = 3
> level Minus  = 3 
> level Times  = 5
> level _      = 8

> levelApp     = 10
> levelIf      = 2
> levelLet     = 1
> levelFun     = 1  -- (= almost always needs parens)



Roundtrip Property
------------------


> indented :: PP a => a -> String
> indented = PP.render . pp

> prop_roundtrip :: Expression -> Bool
> prop_roundtrip s = parse (indented s) == Just s


> quickCheckN :: Test.QuickCheck.Testable prop => Int -> prop -> IO ()
> quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n , maxSize = 100 }


> instance Arbitrary Expression where
>   arbitrary = sized genExp

>   shrink (Op o e1 e2)  = [Op o e1' e2' | e1' <- shrink e1, e2' <- shrink e2 ]
>   shrink (If e1 e2 e3) = [If e1' e2' e3' | e1' <- shrink e1, e2' <- shrink e2, e3' <- shrink e3 ]
>   shrink (Fun v e1)    = [Fun v e1' | e1' <- shrink e1]
>   shrink (App e1 e2)   = [App e1' e2' | e1' <- shrink e1, e2' <- shrink e2 ]
>   shrink (Let v e1 e2) = [Let v e1' e2' | e1' <- shrink e1, e2' <- shrink e2 ]
>   shrink _             = [ ]

> genExp :: Int -> Gen Expression
> genExp 0 = oneof     [ liftM Var arbVar
>                      , liftM IntExp arbNat
>                      , liftM BoolExp arbitrary
>                      ]
> genExp n = frequency [ (1, liftM Var arbVar)
>                      , (1, liftM IntExp arbNat)
>                      , (1, liftM BoolExp arbitrary)
>                      , (7, liftM3 Op arbitrary (genExp n') (genExp n'))
>                      , (7, liftM3 If (genExp n') (genExp n') (genExp n'))
>                      , (7, liftM2 Fun arbVar (genExp n'))
>                      , (7, liftM2 App (genExp n') (genExp n'))
>                      , (7, liftM3 Let arbVar (genExp n') (genExp n')) 
>                      ]
>  where n' = n `div` 2
> instance Arbitrary Bop where
>   arbitrary = elements [ Plus .. ]

> arbNat :: Gen Int
> arbNat = liftM abs arbitrary

> arbVar :: Gen Variable
> arbVar = elements $ map return ['A'..'Z']