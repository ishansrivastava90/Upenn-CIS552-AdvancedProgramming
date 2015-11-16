> {-# OPTIONS -Wall #-}
> module SecretCode where

OK, we're going to write a Haskell program to encode and decode text
files using a secret code.  We'll call it the Brown Fox code.  Here's
how it works:

    - Replace each letter according to the following correspondence:

            "abcdefghijklmnopqrstuvwxyz"
        to  "thequickbrownfxjmpsvlazydg"

      But leave any non-letter characters alone.

    - Then reverse the order of the lines in the file.

> import Data.Char
> import Data.Maybe
> import Test.HUnit

First, we make a lookup list containing each pair of corresponding letters:

> code :: [(Char,Char)]
> code =    (zip ['a' .. 'z'] cypher)
>        ++ (zip ['A' .. 'Z'] (map toUpper cypher))
>   where 
>     cypher :: String
>     cypher = "thequickbrownfxjmpsvlazydg"

Now, how can we use this list?  

Association lists seem like they are probably a pretty common idiom, so let's
check Hoogle to see if there is a function to look things up in them
automatically...


What type would such a function have?  In our case, we want something
like:

        [(Char,Char)] -> Char -> Char

That is, the function should take a list that maps `Char`s to `Char`s
and a specific `Char`, and it should return the corresponding `Char`
from the list.  The first hit on Hoogle for this type is the standard
library function `lookup`:

       lookup :: Eq a => a -> [(a,b)] -> Maybe b

Ignoring the `Eq a` part for now (we'll talk about it next week), this
type makes a lot of sense.  It's more general (the list doesn't have
to contain `Char`s), and it returns a `Maybe`, because the thing you're
looking up might not be in the list.

(Recall that a `Maybe a` is either `Just v` for some v of type `a`, or
Nothing.)

So, we can use this to encode a particular character:

> encodeChar :: Char -> Char
> encodeChar c = fromMaybe c (lookup c code)
  
(Here, fromMaybe is another standard library functin. This function does the same thing as   
      swapChar :: Char -> Char
      swapChar c = 
         case lookup c code of 
            Just c' -> c'
            Nothing -> c
)

> testEncodeChar :: IO Counts
> testEncodeChar = runTestTT $ TestList [ encodeChar 'a' ~?= 't', encodeChar '.' ~?= '.']


We'll need a way to encode a whole line of text.  Of course, remembering
that `String`s are just lists of `Char`s, we know just the abstraction
for this:

> encodeLine :: String -> String
> encodeLine x = map encodeChar x

> testEncodeLine :: IO Counts
> testEncodeLine = runTestTT $ TestList [ encodeLine "abc defgh" ~?= "the quick"]

Finally, we'd need a function to encode a whole file.  Remember, we
want to reverse the order of the lines (so that the last line is
first, and so on), then swap the letters in each.  First, we
need a way to break a file into lines - we could do this manually
by folding over the `String` and checking for newlines, but since
it seems pretty common, let's check Hoogle instead.  Indeed,
we find a function

   lines :: String -> [String]

in the standard library to do just this.  And its counterpart:

   unlines :: [String] -> String

> encodeContent :: String -> String
> encodeContent = unlines . reverse . map encodeLine . lines

Here, the . operator is function composition.  That is:

      (f . g) x = f (g x)

OK, now let's construct an IO action that actually reads in a file
from disk, encodes it, and writes it back out.  We can look at the
Haskell Prelude to find functions for reading and writing files.

        readFile  :: FilePath -> IO String
        writeFile :: FilePath -> String -> IO ()

> encodeFile :: FilePath -> IO ()
> encodeFile f = do fcontents <- readFile f
>                   writeFile (f ++ ".code") (encodeContent fcontents)

OK, now lets put it all together into a "main" function that reads in
a file from input and swizzles it:

> main :: IO ()
> main = do putStrLn "What file shall I encode?"
>           fn <- getLine
>           encodeFile fn
>           putStrLn "All done!"

Now, for an additional challenge, refactor this program so that it can both
*encode* and *decode files*. Try to reuse as much of the existing
functionality as you can. One possible solution is [SecretCode2](SecretCode2.html).