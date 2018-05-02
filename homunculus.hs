module Main where

import Homunculus.Generator
import Homunculus.Names
import Homunculus.Parser
import Homunculus.Sidebar
import Homunculus.Toolbox

import Graphics.UI.Gtk
import System.Directory 
import System.Environment (getArgs)
import System.Random

main :: IO ()
main = main' =<< getArgs

main' :: [String] -> IO ()
main' args = case args of
  []              -> start
  ("-h":_)        -> putStrLn helpMessage
  ("--help":_)    -> putStrLn helpMessage
  ("-g":[])       -> putStrLn "No generator file given!"
  ("-g":x:_)      -> putStrLn =<< generateFile x
  ("-t":_)        -> makeGeneratorTest >> putStrLn "Generator file created successfully."
  ("-r":xs)       -> do
                      g <- newStdGen
                      putStrLn $ fst $ head $ parse (script ["All"] g) (concat xs)
  ("-nM":xs)      -> runMarkov xs
  ("-n":"-M":xs)  -> runMarkov xs
  ("-n":xs)       -> do
                      fs <- mapM readFile xs
                      putStrLn =<< randomName (lines $ concat fs) False
  (x:_)           -> putStrLn $ concat ["Invalid option -- '",x
                                      ,"Try 'homunculus -h' for more information."]
  where makeGeneratorTest = writeFile "Test.gen" $ show testGen
        runMarkov xs = do
          fs <- mapM readFile xs
          g <- newStdGen
          putStrLn =<< randomName (lines $ concat fs) True

helpMessage :: String
helpMessage = concat ["Usage: homunculus [OPTION]... [FILE]...\n\n"
                     ,"Homunculus Options\n"
                     ,"  -h or --help   Print this message.\n"
                     ,"  -g <path>      Generate a result from the table at <path>\n"
                     ,"                   (Options will be unset at this time)\n"
                     ,"  -n <files>     Generate a name from the <files>\n"
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

  helpMenu <- menuNew
  help <- menuItemNewWithLabel "Help"
  abtH <- menuItemNewWithLabel "About"

  vbox <- vBoxNew False 0
  hbox <- hBoxNew False 0

  sideBox' <- scrolledWindowNew Nothing Nothing
  sideBox <- vBoxNew False 0
  toolBox' <- scrolledWindowNew Nothing Nothing
  toolBox <- vBoxNew False 0

  {-
    CONSTRUCTION
  -}

  --I plan on adding more later, so I'm going to go ahead and use mapM_
  mapM_ (menuShellAppend menu) [file,view,help]
  mapM_ (menuShellAppend fileMenu) [quit]
  mapM_ (menuShellAppend viewMenu) [side]
  mapM_ (menuShellAppend helpMenu) [abtH]

  set side      [ checkMenuItemActive := True ]
  set file      [ menuItemSubmenu := fileMenu ]
  set view      [ menuItemSubmenu := viewMenu ]
  set help      [ menuItemSubmenu := helpMenu ]
  set toolBox   [ containerBorderWidth := 5 ]
  set sideBox'  [ containerChild := sideBox 
                , scrolledWindowHscrollbarPolicy := PolicyNever
                , scrolledWindowVscrollbarPolicy := PolicyAutomatic
                ]
  set toolBox'  [ containerChild := toolBox ]
  set hbox      [ containerChild := sideBox', boxChildPacking sideBox' := PackNatural
                , containerChild := toolBox'
                , containerBorderWidth := 5
                ]
  set vbox      [ containerChild := menu, boxChildPacking menu := PackNatural
                , containerChild := hbox
                ]
  set window    [ containerChild := vbox ]

  {-
    LOGIC
  -}
  --This is the default path where all of your data will be kept. I'm calling this here and
  --passing it along to functions (rather than calling it as needed) so that, in future
  --versions, I can have some logic here for user-defined directories.
  dataPath <- getAppUserDataDirectory "homunculus"

  --I need to make sure there's a file structure that Homunculus can follow in dataPath.
  createDirectoryIfMissing True $ dataPath++"/names"
  createDirectoryIfMissing True $ dataPath++"/generators"


  makeSidebar dataPath sideBox
  makeToolbox dataPath toolBox

  on abtH menuItemActivated aboutWindow
  on side checkMenuItemToggled $ do
    b <- checkMenuItemGetActive side
    set sideBox' [ widgetVisible := b ]
  on quit menuItemActivated mainQuit
  on window objectDestroy mainQuit

  widgetShowAll window

  mainGUI

aboutWindow :: IO ()
aboutWindow = do
  dialog <- aboutDialogNew

  set dialog  [ aboutDialogProgramName := "Your Helpful Homunculus"
              , aboutDialogVersion := "v1.0.5"
              , aboutDialogWebsite := "http://github.com/Semantimancer/homunculus"
              , aboutDialogWebsiteLabel := "Your Helpful Homunculus GitHub"
              , aboutDialogAuthors := ["Ben Kugler (ben@bkugler.com)"]
              ]

  _ <- dialogRun dialog
  widgetDestroy dialog
