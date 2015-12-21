module Main where

import Homunculus.Sidebar
import Homunculus.Parser
import Homunculus.Toolbox
import Homunculus.Generator

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

  fileMenu <- menuNew
  file <- menuItemNewWithLabel "File"
  quit <- menuItemNewWithLabel "Quit"

  viewMenu <- menuNew
  view <- menuItemNewWithLabel "View"
  side <- checkMenuItemNewWithLabel "Sidebar"

  vbox <- vBoxNew False 0
  hbox <- hBoxNew False 0

  sideBox <- vBoxNew False 0
  toolBox <- vBoxNew False 0

  {-
    CONSTRUCTION
  -}

  --I plan on adding more later, so I'm going to go ahead and use mapM_
  mapM_ (menuShellAppend menu) [file,view]
  mapM_ (menuShellAppend fileMenu) [quit]
  mapM_ (menuShellAppend viewMenu) [side]

  set side    [ checkMenuItemActive := True ]
  set file    [ menuItemSubmenu := fileMenu ]
  set view    [ menuItemSubmenu := viewMenu ]
  set toolBox [ containerBorderWidth := 5 ]
  set hbox    [ containerChild := sideBox, boxChildPacking sideBox := PackNatural
              , containerChild := toolBox
              ]
  set vbox    [ containerChild := menu, boxChildPacking menu := PackNatural
              , containerChild := hbox
              ]
  set window  [ containerChild := vbox ]

  {-
    LOGIC
  -}
  --This is the default path where all of your data will be kept. I'm calling this here and
  --passing it along to functions (rather than calling it as needed) so that, in future
  --versions, I can have some logic here for user-defined directories.
  dataPath <- getAppUserDataDirectory "homunculus"

  makeSidebar dataPath sideBox
  makeToolbox dataPath toolBox

  on side checkMenuItemToggled $ do
    b <- checkMenuItemGetActive side
    set sideBox [ widgetVisible := b ]
  on quit menuItemActivated mainQuit
  on window objectDestroy mainQuit

  widgetShowAll window

  mainGUI
