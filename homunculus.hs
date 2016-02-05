module Main where

import Homunculus.Generator
import Homunculus.Names
import Homunculus.Parser
import Homunculus.Sidebar
import Homunculus.Toolbox

import Graphics.UI.Gtk
import System.Directory (getAppUserDataDirectory)
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
  genH <- menuItemNewWithLabel "Generator Help"

  vbox <- vBoxNew False 0
  hbox <- hBoxNew False 0

  sideBox <- vBoxNew False 0
  toolBox <- vBoxNew False 0

  {-
    CONSTRUCTION
  -}

  --I plan on adding more later, so I'm going to go ahead and use mapM_
  mapM_ (menuShellAppend menu) [file,view,help]
  mapM_ (menuShellAppend fileMenu) [quit]
  mapM_ (menuShellAppend viewMenu) [side]
  mapM_ (menuShellAppend helpMenu) [abtH,genH]

  set side    [ checkMenuItemActive := True ]
  set file    [ menuItemSubmenu := fileMenu ]
  set view    [ menuItemSubmenu := viewMenu ]
  set help    [ menuItemSubmenu := helpMenu ]
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

  on genH menuItemActivated genWindow
  on abtH menuItemActivated aboutWindow
  on side checkMenuItemToggled $ do
    b <- checkMenuItemGetActive side
    set sideBox [ widgetVisible := b ]
  on quit menuItemActivated mainQuit
  on window objectDestroy mainQuit

  widgetShowAll window

  mainGUI

aboutWindow :: IO ()
aboutWindow = do
  dialog <- aboutDialogNew

  set dialog  [ aboutDialogProgramName := "Your Helpful Homunculus"
              , aboutDialogVersion := "v1.0.3"
              , aboutDialogWebsite := "http://github.com/Semantimancer/homunculus"
              , aboutDialogWebsiteLabel := "Your Helpful Homunculus GitHub"
              , aboutDialogAuthors := ["Ben Kugler (ben@bkugler.com)"]
              ]

  _ <- dialogRun dialog
  widgetDestroy dialog

genWindow :: IO ()
genWindow = do
  dialog <- messageDialogNew Nothing [DialogDestroyWithParent] MessageInfo ButtonsClose txt
  _ <- dialogRun dialog
  widgetDestroy dialog
  where txt = "     The generator tool lets GMs create randomized tables which can help them come up with new content on the fly.\n     To get started, make a new generator by selecting the \"New Generator...\" option from the dropdown menu. The text boxes on the left side of the tool let you quickly and easily change your new generator's title, description, and option set. The right side is where you can see your tables, which is where you will be doing most of your work.\n     When you use a generator, it will randomly select a row from the current table. The \"weight\" option on the row determines how likely it is to be selected; the higher the weight, the more often you'll see that result!\n     When a row is selected, the generator will show whatever you put in that row's text-field. While you could use just put plain text in, Your Helpful Homunculus also knows how to read a few types of scripts that can greatly increase your generator's variety. These script types are dice notation, list elements, option elements, and table elements.\n     Dice Notation is something you're probably already familiar with: it's the way most RPGs talk about how to roll dice. Your Helpful Homunculus will take anything like 1d6, 1d8, d10, d12+1, or any other similar notation and automatically roll the dice for you!\n     A list element is a series of items that are selected at random. A list must always begin with a left bracket ([) and end with a right bracket (]), and every item has to be separated with a pipe (|). For example, Your Helpful Homunculus could take a list like [A|B|C] and then choose either A, B, or C to show. The other two results will simply be ignored. This is a great way to change small things about your generator's output, like the colors of an NPC's eyes (something like \"[green|blue|brown]\") or to only sometimes add extra details (\"[|The NPC has a pet]\"). Every item in a list has an equal chance of being selected.\n     Option elements are ways to make the options that you set for a generator matter. Option elements have to start with a left curly-brace ({) and end with a right curly-brace (}), while elements inside are separated with semicolons (;). Each option looks like \"option=result\" where \"option\" is the EXACT text that you select for that option and \"result\" is the text that you want to include when that option is true. At the end of a series of options, it's good to include an \"else clause\" that looks like \"Else=result.\" This tells Your Helpful Homunculus what text to include if NONE of the options you used were set. Options are a good way to give some more fine-grained control over a generator, such as by being able to tell it what season to use for a wilderness encounter generator (something like \"{Spring=springtext;Summer=summertext;Fall=falltext;Else=wintertext}\").\n     Finally, table elements let you use the results from other tables in the same generator. This is just as if you had called the generator on that particular table. A table element always starts with a left brace (<) and ends with a right brace (>). The EXACT name of the table you want to use goes between the braces. But, keep one thing in mind: the order of your tables matters! You can only use table elements on tables that appear AFTER the current one in your generator. For example, if you have three tables (A, B, and C), then results in A can call tables B or C (which would look like \"<B>\" or \"<C>\"). The results in B could ONLY call C, however, and the results in C couldn't call any table at all."
