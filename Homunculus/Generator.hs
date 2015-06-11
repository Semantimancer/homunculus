module Homunculus.Generator where

import Graphics.UI.Gtk hiding (Table)

import System.Random

data Table = Table { name, description :: String      --These will be displayed to the user
                   , rows :: [(Int,String)]           --The meat & potatoes
                   , options :: [(String,[String])]   --User-set options (for variables)
                   }
  deriving (Read,Show,Eq,Ord)

generate :: Table -> StdGen -> String
generate t g = list !! (i `mod` length list)
  where list = concatMap (uncurry replicate) $ rows t
        --I'll probably use the StdGen on this for the parsers
        (i,_) = random g :: (Int,StdGen)

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
                         ]
                , options = [("Option 1",["A","B","C"])]
                }
