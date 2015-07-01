module Homunculus.Generator where

import Homunculus.Parser

import Data.List (isInfixOf)
import Graphics.UI.Gtk hiding (Table)
import System.Directory
import System.FilePath ((</>))
import System.Random

{-
  The basic (functional) code
-}

data Table = Table { name, description :: String      --These will be displayed to the user
                   , rows :: [(Int,String)]           --The meat & potatoes
                   , options :: [(String,[String])]   --User-set options (for variables)
                   }
  deriving (Read,Show,Eq,Ord)

generate :: Table -> [String] -> StdGen -> String
generate t opts g = case parse (script opts g') $ list !! (i `mod` length list) of
  [(x,[])]  -> x
  []        -> "Error in parse function!"
  where list = concatMap (uncurry replicate) $ rows t
        (i,g') = random g :: (Int,StdGen)

readTable :: String -> Maybe Table
readTable str = case reads str of
  [(x,"")]  -> Just x
  _         -> Nothing

--Function for taking output of readTable and forming [Table]
maybeToList :: [Maybe a] -> [a]
maybeToList [] = []
maybeToList (x:xs) = case x of
  Nothing -> maybeToList xs
  Just a  -> a : maybeToList xs

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

{-
  IO code (mostly graphics-related)
-}

generateFile :: FilePath -> IO String
generateFile fp = do
  file <- readFile fp 
  g <- newStdGen
  return $ f (readTable file) g
  where f Nothing _   = "Error: Failed to read "++fp
        f (Just t) g  = generate t [] g

--Clears out the box and creates the Generator UI within it
makeGenerator :: FilePath -> VBox -> IO ()
makeGenerator dataPath box = do
  --Have to clear the box before doing anything else
  mapM_ widgetDestroy =<< containerGetChildren box

  {-
    INITIALIZATION
  -}
  top <- hBoxNew False 0
  bottom <- hBoxNew False 0
  left <- vBoxNew False 0
  right <- vBoxNew False 0

  list <- listStoreNew []
  combo <- comboBoxNewWithModel list
  ok <- buttonNewWithLabel "Generate"
  edit <- buttonNewWithLabel "Edit"
  l <- labelNew $ Just "Choose A Generator:  "

  {-
    CONSTRUCTION
  -}
  set top     [ containerChild := l, boxChildPacking l := PackNatural
              , containerChild := combo
              , containerChild := ok, boxChildPacking ok := PackNatural
              , containerChild := edit, boxChildPacking edit := PackNatural
              ]
  set bottom  [ containerChild := left
              , containerChild := right
              ]
  set box     [ containerChild := top, boxChildPacking top := PackNatural
              , containerChild := bottom
              ]

  {-
    LOGIC
  -}
  --Load all generators from dataPath
  fileList <- getDirectoryContents $ dataPath </> "generators"
  files <- mapM readFile $ 
    map ((</>) (dataPath </> "generators")) $ filter (isInfixOf ".gen") fileList
  let tables = maybeToList $ map readTable files
  
  --Put all loaded generators into a ListStore, which is used by combo
  mapM_ (listStoreAppend list) tables

  --This'll cause an error if list is empty.
  --comboBoxSetActive combo 0

  --When the combo box is opened, the ListStore displays the names of the generators
  ren <- cellRendererTextNew
  cellLayoutPackEnd combo ren False
  cellLayoutSetAttributes combo ren list (\gen -> [ cellText := name gen ])

  on ok buttonActivated $ do
    gen <- listStoreGetValue list =<< comboBoxGetActive combo
    g <- newStdGen
    --At some point, I'll actually add the options in. For now, it's just [].
    putStrLn $ generate gen [] g

  if tables==[] 
  then do
    set edit  [ widgetSensitive := False ]
    set ok    [ widgetSensitive := False ]
    set combo [ widgetSensitive := False ]
  else comboBoxSetActive combo 0
