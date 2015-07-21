module Main where

import Homunculus.Generator
import Homunculus.Parser

import Graphics.UI.Gtk
import System.Directory (getAppUserDataDirectory)
import System.Environment (getArgs)
import System.Random

main :: IO ()
main = main' =<< getArgs

main' :: [String] -> IO ()
main' args = case args of
  []            -> start
  ("-h":_)      -> putStrLn helpMessage
  ("--help":_)  -> putStrLn helpMessage
  ("-g":[])     -> putStrLn "No generator file given!"
  ("-g":x:_)    -> putStrLn =<< generateFile x
  ("-t":_)      -> makeGeneratorTest >> putStrLn "Generator file created successfully."
  ("-r":xs)     -> do
                   g <- newStdGen
                   putStrLn $ fst $ head $ parse (script ["All"] g) (concat xs)
  (x:_)         -> putStrLn $ concat ["Invalid option -- '",x
                                     ,"Try 'homunculus -h' for more information."]
  where makeGeneratorTest = writeFile "Test.gen" $ show testGen

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

start :: IO ()
start = do
  initGUI

  {-
    INITIALIZATION
  -}
  window <- windowNew

  menu <- menuBarNew

  file <- menuItemNewWithLabel "File"
  fileMenu <- menuNew
  quit <- menuItemNewWithLabel "Quit"

  vbox <- vBoxNew False 0
  genBox <- vBoxNew False 0

  {-
    CONSTRUCTION
  -}

  --I plan on adding more later, so I'm going to go ahead and use mapM_
  mapM_ (menuShellAppend menu) [file]
  mapM_ (menuShellAppend fileMenu) [quit]

  set file    [ menuItemSubmenu := fileMenu ]
  set vbox    [ containerChild := menu, boxChildPacking menu := PackNatural
              , containerChild := genBox
              ]
  set window  [ containerChild := vbox ]

  {-
    LOGIC
  -}
  --This is the default path where all of your data will be kept. I'm calling this here and
  --passing it along to functions (rather than calling it as needed) so that, in future
  --versions, I can have some logic here for user-defined directories.
  dataPath <- getAppUserDataDirectory "homunculus"

  makeGenerator dataPath genBox

  on quit menuItemActivated mainQuit

  on window objectDestroy mainQuit
  widgetShowAll window

  mainGUI
