module Homunculus.Parser where

import Data.Char
import System.Random

import Prelude hiding (getChar)

{-
  Parsers have a second type (a) attached to them, that determines what they parse.
  All a parser contains is a function which will take a string and return a list of
  tuples: the first half of the tuple will be one possible "reading" of the string,
  and the second half will be leftovers that weren't parsed. The list means that there
  might be many different "readings" that the parser could come up with, and allows us
  to get away with failed reads by returning [] rather than using Maybe. In pretty much
  every instance here, we'll be ignoring all but the first entry in the list.
-}
newtype Parser a = Parser (String -> [(a,String)])

{-
  Functors can map over the values in some kind of "container" without modifying the
  container itself: ONLY the values within it change. This is pretty useful.

  For our particular uses, we only need to define fmap (because the function (<$) has
  a default binding that should work just fine).
-}
instance Functor Parser where
  fmap f p = p >>= \x -> return (f x)

{-
  Monads have to have bind (>>=), return, and fail functions defined.
  
  Bind lets me use functions of type (a -> Monad b) and apply them to values of type 
  Monad a. In this case, it'll probably be used mostly to apply (String -> Parser String) 
  functions to values that have already been run through a parser (therefore already of 
  type Parser String).

  Monads also have to be able to return their values, which basically converts anything
  of type a into type Monad a. Easy enough.

  Fail is just a general thing, a way to still return something.
-}
instance Monad Parser where
  p1 >>= f = Parser (\q -> do
                            (x,q') <- parse p1 q
                            parse (f x) q'
                    )
  return x = Parser (\q -> [(x,q)])
  fail _   = Parser (\_ -> [])

--Applies the parser function to the string
parse :: Parser a -> String -> [(a,String)]
parse (Parser p) str = p str

--Our most basic parser. It parses a single character at the head of the string.
getChar :: Parser Char
getChar = Parser $ \a -> case a of
  (x:xs)  -> [(x,xs)]
  []      -> []

--This is the same as the above, but gives us a string rather than a char.
getChar' :: Parser String
getChar' = Parser $ \a -> case a of
  (x:xs)  -> [([x],xs)]
  []      -> []

{-
  This is our combinator.

  If the first parser is given a string and reads it properly, then the result is returned
  as normal. If the first parser fails, the string will be run through the second parser
  instead of just failing. This can, of course, be chained beyond two parsers.
  
  Note that the first parser has priority. This is sequential, not parallel.
-}
(<>) :: Parser a -> Parser a -> Parser a
p <> p' = Parser $ \q -> case parse p q of
  []  -> parse p' q
  x   -> x

{-
  Checks to see if the head character in the string satisfies a condition. If it does, then
  that character is pulled off the string. If not, it fails.

  Good for checking to see if the next character is important.
-}
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- getChar
  if f c then return c else fail []

--Is the next char on the string [parameter]?
char :: Char -> Parser Char
char c = satisfy (==c)

--These kind of tricks are why I use Haskell.
string :: String -> Parser String
string = mapM char

--Convenience functions
alpha, digit, upper, lower, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpace

{-
  The many function has a built in "safety." If it can parse it, then it'll return a 
  parsed string (using rest to get a string rather than a char). If it can't parse, then
  it'll return a blank string.

  The "rest" function applies the same parser multiple times. Anytime something fails to
  parse, it will (thanks to many) just be erased, replaced with a blank string. Then the
  final result is concatenated, giving us [a].

  These two functions both call one another, and neither has a base case. That means that
  they can theoretically recurse infinitely. This is why lazy evaluation exists.
-}
many :: Parser a -> Parser [a]
many p = rest p <> (return [])

rest :: Parser a -> Parser [a]
rest p = do
  --Apply the parser to the first character in the string.
  x <- p
  --Now, with many as a safety function, apply it to the next part.
  xs <- many p
  --Finally, put the two results together and return them.
  return $ x:xs

--Goes through the string-to-parse until it sees the string-given-as-a-parameter.
until :: String -> Parser String
until s = many (satisfy (`notElem` s))

--Checks to see if the next character is one of a given list
oneOf :: [Char] -> Parser Char
oneOf xs = do
  x <- getChar
  if x `elem` xs then return x else fail []

--Take a string and, if possible, return an integer.
int :: Parser Int
int = do
  --Pull away a negative sign if it's there. Gives "" if there isn't one.
  sign <- string "-" <> return []
  --Uses rest/many to pull in all the digits it can reach. Fails if there are none.
  digit' <- rest digit
  --Now that we've isolated the sign (if any) and the numbers, we can use the built-in
  --read function to do the heavy-lifting.
  return (read (sign++digit') :: Int)

{-

  EVERYTHING BEFORE THIS POINT IS BASICALLY PREAMBLE.

  THIS IS WHERE THE USEFUL STUFF STARTS.

-}

--This is our primary parser. All of our useful stuff should get plugged into this.
script :: StdGen -> Parser String
script g = do
  --Runs through our various script-related parsers. If all fail, just returns the first
  --character as a string.
  x <- dice g <> getChar'
  --We have to use the many function here as a safety measure. Unfortunately, this will
  --convert all of our nice Parser String functions into Parser [String] functions, so
  --when we return everything we have to concat them all back together. We use a new
  --StdGen for this recursion so that we don't keep getting the same numbers over and over.
  xs <- many $ script g'
  --Finally, before returning, this runs the result through a series of secondary parsers
  --that make any finishing touches
  return $ ret $ concat $ x:xs
  where ret s = case parse script' s of
          [(x,[])]  -> x
          []        -> []
        (_,g') = random g :: (Int,StdGen)

--This is run over the output of script. The reason we don't combine the two parsers into
--one is that the parsers used here MUST be used on the output of the parsers found in
--script. For example, we have to use ops on the output of dice.
script' :: Parser String
script' = do
  x <- ops <> getChar'
  xs <- many script'
  return $ concat $ x:xs

--This is just a wrapper function to convert the rolls into a string
dice :: StdGen -> Parser String
dice g = do
  x <- dice' g
  return $ show $ foldl (+) 0 x

--Takes dice notation (XdY) and creates a [Int] to simulate rolling that many times
dice' :: StdGen -> Parser [Int]
dice' g = do
  --If there's an int, pull it. If not (written "dY"), it will use a 1.
  --If a negative number is given, Haskell will deal with it error-free.
  x <- int <> return 1
  --Pulls the 'd' character out. Since we don't need it for anything, it's just _
  _ <- char 'd'
  y <- int
  return $ take x $ randomRs (1,y) g

--Wrapper function to take mathematical operations and convert the result to a string
ops :: Parser String
ops = do
  x <- ops'
  return $ show x

--Take a mathematical operation (XfY) and evaluate it
ops' :: Parser Int
ops' = do
  x <- int
  f <- oneOf "+-*/%"
  y <- int
  ret x f y
  where ret x f y = case f of
          '+' -> return $ x+y
          '-' -> return $ x-y
          '*' -> return $ x*y
          '/' -> return $ x `div` y --Note that this is integer division!
          '%' -> return $ x `mod` y
          _   -> fail []



























