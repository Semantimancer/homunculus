module Homunculus.Generator where

import Homunculus.Parser

import Graphics.UI.Gtk hiding (Table)
import System.Random

data Table = Table { name, description :: String      --These will be displayed to the user
                   , rows :: [(Int,String)]           --The meat & potatoes
                   , options :: [(String,[String])]   --User-set options (for variables)
                   }
  deriving (Read,Show,Eq,Ord)

generate :: Table -> StdGen -> String
generate t g = case parse (script g') $ list !! (i `mod` length list) of
  [(x,[])]  -> x
  []        -> "Error in parse function!"
  where list = concatMap (uncurry replicate) $ rows t
        (i,g') = random g :: (Int,StdGen)

generateFile :: FilePath -> IO String
generateFile fp = do
  file <- readFile fp 
  g <- getStdGen
  return $ f (readTable file) g
  where f Nothing _   = "Error: Failed to read "++fp
        f (Just t) g  = generate t g

readTable :: String -> Maybe Table
readTable str = case reads str of
  [(x,"")]  -> Just x
  _         -> Nothing

testGen :: Table
testGen = Table { name = "Test"
                , description = "This table should only be used for testing purposes."
                , rows = [(3,"Test (Weight 3)")
                         ,(2,"Test (Weight 2)")
                         ,(1,"Test (Weight 1)")
                         ,(2,"There are 1d4+7 goblins")
                         ,(2,"This should be 6: 2*3")
                         ,(2,"It hails in a radius of d100*100ft")
                         ]
                , options = [("Option 1",["A","B","C"])]
                }
