module Homunculus.Parser where

import Control.Monad (ap)
import Data.Char
import Data.List.Utils (split)
import System.Random hiding (split)

import Prelude hiding (getChar,until)

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

--With the new GHC release, all Monads have to be Applicative as well. Luckily, according
--to "Haskell/Applicative Functors" from WikiBooks, the two are so similar that we can
--make our Parser Applicative by simply making some synonyms.
instance Applicative Parser where
  pure  = return
  (<*>) = ap

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
  This is our compositor.

  If the first parser is given a string and reads it properly, then the newly parsed string
  will be sent through the second parser. If the first parser fails, however, then the
  second parser will never be used.
-}
(<.>) :: Parser String -> Parser String -> Parser String
p <.> p' = Parser $ \q -> case parse p q of
  [(x,[])]  -> parse p' x
  x         -> x

{-
  And this is a specialized combinator/compositor that allows for backtracking. 
  
  It takes a string and runs it through the first parser, then takes EVERYTHING (whether 
  the first parser got to it or not) and runs that, collectively, through the second. If 
  the first parser fails entirely, then it will just run the second parser over the 
  original string.
-}

(<?>) :: Parser String -> Parser String -> Parser String
p <?> p' = Parser $ \q -> case parse p q of
  [(x,xs)]  -> parse p' $ x++xs
  []        -> parse p' q

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
script :: [String] -> StdGen -> Parser String
script opts g = do
  --Runs through our various script-related parsers. If all fail, just returns the first
  --character as a string.
  --SPECIAL NOTE: The <?> function DOES create precedence, which is determined by the
  --order of the functions. So {foo=[bar|baz]} is legal, but [{foo=bar}|{foo=baz}] is not.
  x <- vars opts <?> list g <?> dice g <?> ops <?> getChar'
  --We have to use the many function here as a safety measure. Unfortunately, this will
  --convert all of our nice Parser String functions into Parser [String] functions, so
  --when we return everything we have to concat them all back together. We use a new
  --StdGen for this recursion so that we don't keep getting the same numbers over and over.
  xs <- many $ script opts g'
  --Then we run the generated string through a parser to do calculations, since those 
  --need to be done after everything else.
  return $ concat $ x:xs
  where (_,g') = random g :: (Int,StdGen)

--This is run over the output of script. The reason we don't combine the two parsers into
--one is that the parsers used here MUST be used on the output of the parsers found in
--script. For example, we have to use ops on the OUTPUT of dice.
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

--Takes dice notation (XdY) and creates a [Int] to simulate rolling that many times.
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

--Takes list-notation [X|Y|Z] and eliminates all but result from the list
list :: StdGen -> Parser String
list g = do
  _ <- char '['
  xs <- list' g'
  return $ xs !! (i `mod` length xs)
  where (i,g') = random g :: (Int,StdGen)

--Divides the list into [String] so that list can choose one to use
--If it sees another list, it will recurse before continuing
list' :: StdGen -> Parser [String]
list' g = do
  --Keep pulling stuff in until you hit something list-y
  val <- until "[|]" 
  --Deal with that list-y thing, and get back a [String] to return
  xs <- (newList val g') <> (f val)
  return xs
  where --If you hit a [, that means you've got a list within a list. So you have to
        --pull in the new list (and make a decision about which part to use), then 
        --combine that with everything else you'll find in this entry.
        newList val g = do
          l <- list g'
          xs <- list' (mkStdGen i)
          return $ (val++l++(head xs)) : (tail xs)
        --If you hit a | or ], you know that you're done with this entry. The difference
        --is in whether or not you then try to find more.
        f val = do
          x <- oneOf "|]"
          if x=='|' 
          then do
            val' <- list' g'
            return $ val:val'
          else return [val]
        (i,g') = random g :: (Int,StdGen)

--Takes a set of options {A=foo;B=bar;Else=Baz} and picks the first TRUE statement
vars :: [String] -> Parser String
vars opts = do
  _ <- char '{'
  xs <- vars'
  return $ head $ (filter (/="") (map test xs))++[""]
  where test ("Else",y) = y
        test (x,y) = if x `elem` opts then y else []

vars' :: Parser [(String,String)]
vars' = do
  val <- until ";}"
  x <- oneOf ";}"
  if x==';'
  then do
    val' <- vars'
    return $ (makeTuple val):val'
  else return [makeTuple val]
  where makeTuple q = f $ split "=" q
        f (x:y:_) = (x,y)
        f x       = (show x,"Error in "++(show x))
