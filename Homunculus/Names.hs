module Homunculus.Names where

import Control.Monad (foldM)
import Data.List
import Data.MarkovChain
import Graphics.UI.Gtk
import System.Directory
import System.FilePath ((</>))
import System.Random

makeNameWidget :: IO Expander
makeNameWidget = do
  {-
    INITIALIZATION
  -}
  exp <- expanderNew "Name Generator"
  frame <- frameNew

  vbox <- vBoxNew False 2
  bar <- hBoxNew False 0
  name <- entryNew
  new <- buttonNewWithLabel "Generate"

  rbox <- hBoxNew False 0
  literal <- radioButtonNewWithLabel "Literal"
  markov <- radioButtonNewWithLabelFromWidget literal "Markov Chain"

  nbox <- vBoxNew False 0
  dataPath <- getAppUserDataDirectory "homunculus"
  --This is unsafe!
  fs <- getDirectoryContents $ dataPath </> "names"
  --cs :: [(CheckButton,[String])]
  cs <- mapM (\f -> do
          file <- readFile $ dataPath </> "names" </> f
          c <- checkButtonNewWithLabel (takeWhile (/='.') f)
          return (c,lines file)) $ filter (isInfixOf ".list") fs

  {-
    CONSTRUCTION
  -}
  set bar   [ containerChild := name
            , containerChild := new, boxChildPacking new := PackNatural
            ]
  set rbox  [ containerChild := literal
            , containerChild := markov
            , containerBorderWidth := 5 
            ]
  set nbox  [ containerBorderWidth := 5 ]
  set vbox  [ containerChild := bar, boxChildPacking bar := PackNatural
            , containerChild := rbox, boxChildPacking rbox := PackNatural
            , containerChild := nbox
            , containerBorderWidth := 5
            ]
  set frame [ containerChild := vbox
            , frameShadowType := ShadowOut
            ]
  set exp   [ containerChild := frame
            , expanderExpanded := True
            , containerBorderWidth := 5
            ]
  --This is more complicated than it needs to be, but I wanted it to put two on a row
  mapM_ (\xs -> do 
            r <- hBoxNew False 2
            mapM_ (\x -> boxPackStart r x PackGrow 0) xs
            boxPackStart nbox r PackNatural 0
            ) $ splitEvery 2 $ map fst cs

  {-
    LOGIC
  -}
  on new buttonActivated $ do
    ns <- foldM (\acc (c,strs) -> do
            bool <- toggleButtonGetActive c
            if bool then return $ acc++strs else return acc
            ) [] cs
    let names = if ns==[] then ["Error: Empty List"] else ns
    radio <- toggleButtonGetActive markov
    g <- newStdGen
    i <- randomRIO (0,length names-1)
    if radio then entrySetText name $ head (runMulti 3 names i g)
             else entrySetText name (names!!i)

  widgetShowAll exp
  return exp

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery i xs = let (l,r) = splitAt i xs in l:splitEvery i r
