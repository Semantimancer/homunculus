module Main where

import Homunculus.Generator

import Graphics.UI.Gtk hiding (Table)
import System.Environment (getArgs)
import System.Random

main :: IO ()
main = main' =<< getArgs

main' :: [String] -> IO ()
main' args = case args of
  []            -> putStrLn "Eventually, this will make the UI start up."
  ("-h":_)      -> putStrLn helpMessage
  ("--help":_)  -> putStrLn helpMessage
  ("-g":[])     -> putStrLn "No generator file given!"
  ("-g":x:_)    -> putStrLn =<< generateFile x
  ("-p":_)      -> makeGeneratorTest >> putStrLn "Generator file created successfully."
  (x:_)         -> putStrLn $ concat ["Invalid option -- '",x
                                     ,"Try 'homunculus -h' for more information."]
  where makeGeneratorTest = writeFile "generatorTest.gen" $ show testGen

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

helpMessage :: String
helpMessage = concat ["Usage: homunculus [OPTION]... [FILE]...\n\n"
                     ,"Homunculus Options\n"
                     ,"  -h or --help   Print this message.\n"
                     ,"  -g <path>      Generate a result from the table at <path>\n"
                     ,"                   (Options will be unset at this time)"
                     ]
