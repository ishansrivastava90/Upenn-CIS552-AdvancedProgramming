Monadic Parsing 
===============

Supplementary reading (not required, but useful): RWH chapters 10 and 16.

> module Parsers where
> import Data.Char
> import Control.Applicative
> import Control.Monad


What is a Parser?
-----------------

A parser is a piece of software that takes a raw `String` (or sequence
of bytes) and returns some structured object -- for example, a list of
options, an XML tree or JSON object, a program's Abstract Syntax Tree,
and so on.  Parsing is one of the most basic computational tasks.  For
example:

- Shell Scripts (command-line options)
- Web Browsers (duh!)
- Games (level descriptors)
- Routers (packets)
- etc.

(Indeed I defy you to find any serious system that does *not* do some
parsing somewhere!)

The simplest way to think of a parser is as a function -- i.e., its
type should be roughly this:

~~~~~{.haskell}
    type Parser = String -> StructuredObject
~~~~~


Composing Parsers
-----------------

The usual way to build a parser is by specifying a grammar and using a
parser generator (e.g., yacc, bison, antlr) to create the actual
parsing function. Despite its advantages, one major limitation of the
grammar-based approach is its lack of modularity. For example, suppose
we have two kinds of primitive values, `Thingy` and `Whatsit`.

       Thingy : ...rule...   { ...action... } ;

       Whatsit : ...rule...  { ...action... } ;

If we want a parser for *sequences of* `Thingy` and `Whatsit` we have
to painstakingly duplicate the rules:

      Thingies : Thingy Thingies  { ... } 
                 EmptyThingy      { ... } ;
      
      Whatsits : Whatsit Whatsits { ... }
                 EmptyWhatsit     { ... } ;

That is, the languages in which parsers are usually described are
lacking in features for modularity and reuse.  

In this lecture, we will see how to *compose* mini-parsers for
sub-values to get bigger parsers for complex values.

To do so, we will generalize the above parser type a little bit, by
noting that a (sub-)parser need not (indeed, in general will not)
consume *all* of its input, in which case we need to have the parser
return the unconsumed part of its input:

~~~~~{.haskell}
    type Parser = String -> (StructuredObject, String) 
~~~~~








Of course, it would be silly to have different types for parsers for
different kinds of structured objects, so we parameterize the `Parser`
type over the type of structured object that it returns:

~~~~~{.haskell}
    type Parser a = String -> (a, String) 
~~~~~





One last generalization is to allow a parser to return a *list* of
possible parse results, where the empty list corresponds to a failure
to parse:

~~~~~{.haskell}
    type Parser a = String -> [(a, String)]
~~~~~






As the last step, let's wrap this type definition up as a `newtype` and 
define a record component to let us conveniently extract the parser:

> newtype Parser a = P { doParse :: String -> [(a, String)] }

~~~~~~{.haskell}
      ghci> :t doParse 
      doParse :: Parser a -> String -> [(a,String)]
~~~~~~


This will make sure that we keep parsers distinct from other values of
this type and, more importantly, will allow us to make parsers an
instance of one or more typeclasses, if this turns out to be
convenient (see below!).

Below, we will define a number of operators on the `Parser` type,
which will allow us to build up descriptions of parsers
compositionally.  The actual parsing happens when we use a parser by
applying it to an input string, using `doParse`.


Now all we have to do is build some parsers!


Parsing a Single Character
--------------------------

Here's a *very* simple character parser that returns the first `Char`
from a (nonempty) string:

> oneChar :: Parser Char
> oneChar = P (\s -> case s of
>                 ""     -> []
>                 (c:cs) -> [(c,cs)] )

~~~~~{.haskell}
    ghci> doParse oneChar "hey!"
    ghci> doPa
    rse oneChar ""p
~~~~~


Parser Composition
------------------

Using `oneChar` we can write a composite parser that returns a pair of
the first two `Char` values from the front of the input string:


> twoChar0 :: Parser (Char, Char)
> twoChar0 = P (\s -> [((c1, c2), s2) | 
>                         (c1,s1) <- doParse oneChar s,
>                         (c2,s2) <- doParse oneChar s1 ])


More generally, we can write a *parser combinator* that takes two
parsers and returns a new parser that uses first one and then the
other and returns the pair of resulting values...

> pairP0 ::  Parser a -> Parser b -> Parser (a,b)
> pairP0 p1 p2 = P (\s -> [((c1, c2), s2) | 
>                         (c1,s1) <- doParse p1 s,
>                         (c2,s2) <- doParse p2 s1 ])

and use that to rewrite 'twoChar' more elegantly like this:

> twoChar = pairP0 oneChar oneChar 

~~~~~{.haskell}
    ghci> doParse twoChar "hey!"
    ghci> doParse twoChar ""
~~~~~

At this point, it will be helpful to step back and take a look at the
bigger picture.  Here's the *type* of a parser:

~~~~~{.haskell}
    newtype Parser a = P { doParse :: String -> [(a, String)] }
~~~~~

It might remind you of something else... Remember this?

~~~~~{.haskell}
    newtype State s a = S { runState :: s -> (a, s) 


Parser is A Monad
=================

Indeed, a parser, like a state transformer, [is a monad][2]!  We just
need to define the `return` and `>>=` functions.

The definition of `return` is very simple -- we can let the types
guide us.  We ignore the input string (keeping the whole thing as the
"remainder after parsing") and just return the given value.

> returnP :: a -> Parser a
> returnP x = P (\s -> [(x,s)]) 

The bind is a bit more tricky, but again, we can lean on the types:

> bindP :: Parser a -> (a -> Parser b) -> Parser b
> p1 `bindP` fp2 = P (\s -> let lt = doParse p1 s in
>                             concatMap (\(x,s') -> doParse (fp2 x) s') lt)

That is, we just need to suck the `a` values out of the first parser
and invoke the second parser with them, passing it the remaining part
of the string.

Armed with those, we can officially brand parsers as monads:

> instance Monad Parser where
>   (>>=)  = bindP
>   return = returnP

And, of course, the associated `Applicative` and `Functor` instances will also
give us useful functions to work with.

> instance Applicative Parser where
>   pure   = return
>   (<*>)  = ap

> instance Functor Parser where
>   fmap   = liftM



Parser Combinators
==================

Now we're picking up speed.  First, we can use our beloved `do`
notation to rewrite `pairP` like this:

> pairP :: Parser a -> Parser b -> Parser (a,b)
> pairP p1 p2 = do x <- p1
>                  y <- p2
>                  return (x, y)

(Note the similarity to the `pairs` function we have seen before!)

We can even dip into the Control.Monad library and write `pairP` even 
more succinctly:

> pairP' :: Parser a -> Parser b -> Parser (a,b)
> pairP' = liftM2 (,)  



Next, let's flex our monadic muscles and write some new parsers. It
will be helpful to have a *failure* parser that always goes down in
flames (returns `[]`):

> failP :: Parser a
> failP = P (\s -> [])






This may seem a little silly, but it's helpful for building up richer
parsers like the following, which parses a `Char` *if* it satisfies a
predicate `p`:

> satP ::  (Char -> Bool) -> Parser Char
> satP b = do 
>          c <- oneChar
>          if (b  c) then return c else failP


Note that we are working abstractly here: we can define `satP` without
using the `P` data constructor or `doParse`.

With this, we can write some simple parsers for particular characters.
The following definitions parse alphabetic and numeric characters
respectively (`isAlpha` and `isDigit` come from the standard Prelude).

> alphaChar, digitChar :: Parser Char
> alphaChar = satP isAlpha
> digitChar = satP isDigit

~~~~~~~~~~~~~~~~~~{.haskell}
    ghci> doParse alphaChar "123" 
    ghci> doParse digitChar "123"
~~~~~~~~~~~~~~~~~~~

And this little fellow returns the first digit in a string as an `Int`:

> digitInt :: Parser Int
> digitInt = do 
>   c <- digitChar
>   return $ ord c - ord '0'

~~~~~{.haskell}
    ghci> doParse digitInt "92"
    ghci> doParse digitInt "cat"
~~~~~

Finally, this parser will parse just one specific `Char`:

> char :: Char -> Parser Char
> char c = satP (==c)

~~~~~~~~~~~{.haskell}
    ghci> doParse (char 'a') "ab" 
    ghci> doParse (char 'a') "ba"            
~~~~~~~~~~~~~~~~~~~~~


Recursive Parsing
-----------------

To parse more interesting things, we need to add some kind of
recursion to our combinators. For example, it's all very well to parse
individual characters (as in `char` above), but it would a lot more
fun if we could recognize whole `String`s.


> string :: String -> Parser String
> string s = sequence  [char x | x <- s] 

Recursive with do notation

> string' :: String -> Parser String
> string' []    = return ""
> string (c:cs) = do
>         c'  <- char c
>         cs' <- string cs
>         return (c' : cs')


Much better!

~~~~~{.haskell}
    ghci> doParse (string "mic") "mickeyMouse"
    ghci> doParse (string "mic") "donald duck"
~~~~~



A Nondeterministic Choice Combinator
------------------------------------

Next, let's write a combinator that takes two sub-parsers and 
nondeterministically chooses between them. 

> chooseP :: Parser a -> Parser a -> Parser a

How to write it?  Well, we want to return a succesful parse if
*either* parser succeeds. Since our parsers return multiple values, we
can simply return the *union* of all the results!

> p1 `chooseP` p2 = undefined

We can use the above combinator to build a parser that 
returns either an alphabet or a numeric character

> alphaNumChar = alphaChar `chooseP` digitChar

~~~~~{.haskell}
    ghci> doParse alphaNumChar "cat"
    ghci> doParse alphaNumChar "2cat"
    ghci> doParse alphaNumChar "2at"
~~~~~

If *both* parsers succeed, we get back all the results. For example,
here's a parser that grabs `n` characters from the input:

> grabn :: Int -> Parser String 
> grabn n | n <= 0    = return ""
>         | otherwise = do c  <- oneChar  
>                          cs <- grabn (n-1)
>                          return (c:cs)

(Challenge: can you remove the explicit recursion from that?)

Now, we can use our choice combinator 

> grab2or4 = grabn 2 `chooseP` grabn 4

and now will get back both results if both parses are possible,

~~~~~{.haskell}
    ghci> doParse grab2or4 "mickeymouse"
~~~~~

and only one if the other is not possible:

~~~~~{.haskell}
    ghci> doParse grab2or4 "mic"
    ghci> doParse grab2or4 "m"
~~~~~


Even with just these rudimentary parsers we have at our disposal, we
can start doing some interesting things. For example, here is a little
calculator. First, we parse arithmetic operations as follows:

> intOp = plus `chooseP` minus `chooseP` times `chooseP` divide 
>   where plus   = char '+' >> return (+)
>         minus  = char '-' >> return (-)
>         times  = char '*' >> return (*)
>         divide = char '/' >> return div

(Can you guess the type of the above parser?)  Then we parse
simple expressions

> calc :: Parser Int
> calc = undefined

which, when run, will perform both parsing and calculation.

~~~~~{.haskell}
    ghci> doParse calc "8/2"
    ghci> doParse calc "8+2cat"
~~~~~





Parsing With a List of Sub-Parsers
----------------------------------

Let's write a combinator that takes a parser `p` that returns an `a`
and constructs a parser that recognizes a sequence of strings (each
recognized by `p`) and returns a *list* of `a` values. That is, it
keeps grabbing `a` values as long as it can and returns them as `[a]`.

We can do this by writing a parser that either succeeds without consuming any
input or parses one thing (if possible) and then calls itself recursively.

> manyP :: Parser a -> Parser [a]
> manyP p = undefined





Beware: This parser can yield a lot of results!

~~~~~{.haskell}
    ghci> doParse (manyP digitInt) "12345a" 
~~~~~

What happens if we swap the order of the `chooseP`?

Deterministic Choice
--------------------

Often, what we want is a single maximal result, not a long list of
partial results.  To get this, we need a *deterministic* choice
combinator. This combinator returns no more than one result -- i.e.,
it runs the choice parser but discards extra results.

> chooseFirstP :: Parser a -> Parser a -> Parser a
> chooseFirstP p1 p2 = undefined

We can use deterministic choice and failure together to make the `Parser` type an instance
of the `Alternative` type class from [Control.Applicative](https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Applicative.html).

> instance Alternative Parser where
>    empty = failP
>    (<|>) = chooseFirstP

This instance automatically gives us definitions of the functions `many` and
`some`.  Their default definitions look something like this:

~~~~~~~~~~{.haskell}
    many :: f a -> f [a]
    many v = some v <|> pure []

    some :: f a -> f [a]
    some v = liftM2 (:) v (many v)
~~~~~~~~~~

The `many` combinator returns a single, maximal sequence produced by iterating
the given parser.

~~~~~{.haskell}
    ghci> doParse (many digitInt) "12345a" 
~~~~~


Let's use the above to write a parser that will return an entire
natural number (not just a single digit.)

> oneNat0 :: Parser Int
> oneNat0 = undefined



*Aside*, can you spot the pattern above? We took the 
parser `many digitChar` and simply converted its output
using the `read` function. This is a recurring theme, and 
the type of what we did gives us a clue...

~~~~~{.haskell}
    (a -> b) -> Parser a -> Parser b
~~~~~

Aha! a lot like `map`. Indeed, we can use `fmap` from the `Functor` instance
to rewrite our number parser like this:

> oneNat1 :: Parser Int
> oneNat1 = undefined

~~~~~{.haskell}
    ghci> doParse oneNat1 "12345a"
    ghci> doParse oneNat1 ""
~~~~~

One more twist: let's make sure that there is at least one digit
before we do the read. For that we can use the `some` function, which 
succeeds only if the given parser succeeds at least once.

> oneNat :: Parser Int
> oneNat = undefined

~~~~~{.haskell}
    ghci> doParse oneNat "12345a"
    ghci> doParse oneNat ""
~~~~~


Parsing XML
===========

For a short digression, let's see a quick example that puts together
what we know about parsers. In 15 mins, we'll build a parser for
simple XML expressions.

See [Xml.lhs](Xml.lhs)



Parsing Arithmetic Expressions
==============================

Now let's use the above to build a small calculator, that parses and
evaluates arithmetic expressions. In essence, an expression is either
a binary operand applied to two sub-expressions or else an integer. We
can state this as:

> calc1 ::  Parser Int
> calc1 = binExp <|> oneNat 
>   where binExp = do x <- oneNat
>                     o <- intOp 
>                     y <- calc1
>                     return $ x `o` y

This works pretty well...

~~~~~{.haskell}
    ghci> doParse calc1 "1+2+33"
    ghci> doParse calc1 "11+22-33"
~~~~~

But things get a bit strange with minus:

~~~~~{.haskell}
    ghci> doParse calc1 "11+22-33+45"
~~~~~

Huh?  Well, if you look back at the code, you'll realize the 
above was parsed as

~~~~~{.haskell}
    11 + ( 22 - (33 + 45))
~~~~~

because in each `binExp` we require the left operand to be an
integer. In other words, we are assuming that each operator is *right
associative* hence the above result.  Making this parser left
associative is harder than it looks---we can't just swap 'oneNat' and
'calc1' above.  (Why not?)

Even worse, we have no precedence, and so

~~~~~{.haskell}
    ghci> doParse calc1 "10*2+100"
~~~~~

is parsed as:

~~~~~{.haskell}
    10 * (2 + 100)
~~~~~

We'll fix the precedence issue first, then tackle associativity.


Precedence
----------

We can add both left associativity and precedence by stratifying the
parser into different levels.  Here, let's split our binary operations
into addition-like and multiplication-like ones.

> addOp :: Parser (Int -> Int -> Int)
> addOp = undefined
>
> mulOp :: Parser (Int -> Int -> Int)
> mulOp = undefined

Now, we can stratify our language into mutually recursive
sub-languages, where each top-level expression is parsed as a
*sum-of-products*

> sumE :: Parser Int
> sumE = undefined

> prodE :: Parser Int
> prodE = undefined
>
> factorE :: Parser Int
> factorE = undefined

~~~~~{.haskell}
    ghci> doParse sumE "10*2+100"
    ghci> doParse sumE "10*(2+100)"
~~~~~

Do you understand why the first parse returned `120` ?  What would
happen if we *swapped* the order of `prodE` and `sumE` in the body of
`addE` (or `factorE` and `prodE` in the body of `prodE`)?




Parsing Pattern: Chaining
-------------------------

There is not much point gloating about combinators if we are going to
write code like the above -- the bodies of `sumE` and `prodE` are
almost identical!

Let's take a closer look at them. In essence, a `sumE` is 
of the form:

~~~~~{.haskell}
    prodE + ( prodE + ( prodE + ... prodE ))
~~~~~

That is, we keep chaining together `prodE` values and adding them for
as long as we can. Similarly a `prodE` is of the form

~~~~~{.haskell}
    factorE * ( factorE * ( factorE * ... factorE ))
~~~~~

where we keep chaining `factorE` values and multiplying them for as
long as we can. 

But we're still not done: we need to fix the associativity problem:

~~~~~{.haskell}
    ghci> doParse sumE "10-1-1"
~~~~~

Ugh! I hope you understand why: it's because the above was parsed as
`10 - (1 - 1)` (right associative) and not `(10 - 1) - 1` (left
associative). You might be tempted to fix that simply by flipping the order,

~~~~~{.haskell}
    sumE = addE <|> prodE 
      where addE = do x <- sumE 
                      o <- addOp
                      y <- prodE 
                      return $ x `o` y
~~~~~

but this would be disastrous. Can you see why?  The parser for `sumE`
directly (recursively) calls itself *without consuming any input!*
Thus, it goes off the deep end and never comes back. Instead, we want
to make sure we keep consuming `prodE` values and adding them up
(rather like fold) and so we could do

> sumE1 :: Parser Int
> sumE1 = prodE1 >>= rest 
>   where rest x = next x <|> return x
>         next x = do o <- addOp
>                     y <- prodE1 
>                     rest $ x `o` y

> prodE1 :: Parser Int
> prodE1 = factorE1 >>= rest
>   where rest x = next x <|> return x
>         next x = do o <- mulOp
>                     y <- factorE1 
>                     rest $ x `o` y

> factorE1 :: Parser Int
> factorE1 = parenE <|> oneNat
>   where parenE = do char '('
>                     n <- sumE1 
>                     char ')'
>                     return n

The above is indeed left associative:

~~~~~{.haskell}
    ghci> doParse sumE1 "10-1-1"
~~~~~

Also, it is very easy to spot and bottle the chaining computation
pattern: the only differences are the *base* parser (`prodE1` vs
`factorE1`) and the binary operation (`addOp` vs `mulOp`).  We simply
make those parameters to our *chain-left* combinator:

> chainl :: Parser b -> Parser (b -> b -> b) -> Parser b
> p `chainl` pop = undefined

Similarly, we often want to parse bracketed expressions, so we can
write a combinator

> parenP :: Char -> Parser b -> Char -> Parser b
> parenP l p r = undefined

after which we can rewrite the grammar in three lines:

> sumE2    = prodE2   `chainl` addOp
> prodE2   = factorE2 `chainl` mulOp
> factorE2 = parenP '(' sumE2 ')' <|> oneNat 

~~~~~{.haskell}
    ghci> doParse sumE2 "10-1-1"
    ghci> doParse sumE2 "10*2+1"
    ghci> doParse sumE2 "10+2*1"
~~~~~


This concludes our in-class exploration of monadic parsing, but what
we've covered is really just the tip of an iceberg. Though parsing is
a very old problem, studied since the dawn of computing, monads bring
a fresh perspective that has now been transferred from Haskell to
[many other languages][3]. There have been several exciting
[recent][4] [papers][5] on the subject, which you can explore on your
own.  Finally, Haskell comes with several parser combinator libraries,
one of which you will use in your next homework assignment.

[2]: http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
[4]: http://www.cse.chalmers.se/~nad/publications/danielsson-parser-combinators.html
[5]: http://portal.acm.org/citation.cfm?doid=1706299.1706347



