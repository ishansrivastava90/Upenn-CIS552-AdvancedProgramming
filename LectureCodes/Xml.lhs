XML parsing
===========

A very simple example of parsing very simple XML data.

> module Xml where

> import Control.Applicative (Alternative(..))
> import Control.Monad (liftM)
> import System.IO

We could base this file on the definitions so far in lecture:

> -- import Parsers

But, to make this part self contained, we will import their 
analogues from Parsec. The `Parsec` library is more general 
than our version, so we will rebind the necessary functions with 
more familiar types.

> import qualified Text.Parsec as P
> type Parser a = P.Parsec String () a

> satP :: (Char -> Bool) -> Parser Char
> satP = P.satisfy

> char :: Char -> Parser Char
> char = P.char

> string :: String -> Parser String
> string = P.string

> parse :: Parser a -> String -> Either P.ParseError a
> parse p = P.parse p ""

Our goal: produce this structured data

> -- | A simplified datatype for storing XML
> data SimpleXML =
>           PCDATA  String
>         | Element ElementName [SimpleXML]
>       deriving Show
> 
> type ElementName = String
 
 

The characters `/`, `<`, and `>` are not allowed to appear in tags or
PCDATA.

> reserved :: Char -> Bool
> reserved c = c `elem` ['/', '<', '>']
 


Parse a maximal nonempty sequence of nonreserved characters:

> text :: Parser String
> text = undefined

~~~~{.haskell}
    Xml> parse text "skhdjf<ksjhdfhjksd"
~~~~
 
Parse an empty element, like `"<br/>"`

> emptyContainer :: Parser SimpleXML
> emptyContainer = undefined

~~~~~{.haskell}
    Xml> parse emptyContainer "<br/>"
~~~~~

 
Parse a container element consisting of an open tag, a sequence of XML
content, and matching a closing tag.  (For example, `<br></br>` or
`<title>A midsummer night's dream</title>`.)

> container :: Parser SimpleXML
> container = undefined

~~~~~{.haskell}
    Xml> parse container "<br></br>"
    Xml> parse container "<title>A midsummer night's dream</title>"
~~~~~



A parser for simple XML data:

> xml :: Parser SimpleXML
> xml = pcdata <|> element
 
> pcdata :: Parser SimpleXML
> pcdata = undefined

> element :: Parser SimpleXML
> element = undefined


Now let's try it on something a little bigger. How about [dream.html](dream.html)?

> -- | Run a parser on a particular input file
> parseFromFile :: Parser a -> String -> IO (Either P.ParseError a)
> parseFromFile parser filename = do 
>   handle <- openFile filename ReadMode 
>   str    <- hGetContents handle
>   return $ parse parser str 



~~~~~{.haskell}
    Xml> parseFromFile xml "dream.html"
~~~~~
