module Main where

import Homunculus.Generator
import Homunculus.Parser

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
  ("-t":_)      -> makeGeneratorTest >> putStrLn "Generator file created successfully."
  ("-r":xs)     -> do
                   g <- newStdGen
                   putStrLn $ fst $ head $ parse (script g) (concat xs)
  (x:_)         -> putStrLn $ concat ["Invalid option -- '",x
                                     ,"Try 'homunculus -h' for more information."]
  where makeGeneratorTest = writeFile "generatorTest.gen" $ show testGen

helpMessage :: String
helpMessage = concat ["Usage: homunculus [OPTION]... [FILE]...\n\n"
                     ,"Homunculus Options\n"
                     ,"  -h or --help   Print this message.\n"
                     ,"  -g <path>      Generate a result from the table at <path>\n"
                     ,"                   (Options will be unset at this time)\n"
                     ,"  -r <line>      Generate a result from <line>\n"
                     ,"                   (Good way to roll dice)\n"
                     ,"  -t             Create a generator for testing purposes\n"
                     ]
