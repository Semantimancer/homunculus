module Homunculus.Roller where

import Homunculus.Parser

import Data.IORef
import Graphics.UI.Gtk
import System.Random

makeDiceWidget :: IO Expander
makeDiceWidget = do
  {-
    INITIALIZATION
  -}
  exp <- expanderNew "Dice Roller"
  frame <- frameNew
  box <- vBoxNew False 2
  box' <- vBoxNew True 2

  view' <- scrolledWindowNew Nothing Nothing
  view <- textViewNew
  txt <- textViewGetBuffer view

  num <- spinButtonNewWithRange 1.0 1000.0 1.0

  dX <- radioButtonNew
  dX' <- entryNew
  row <- hBoxNew False 0

  button <- buttonNewWithLabel "Roll"

  timer <- checkButtonNewWithLabel "Roll Automatically"
  check <- newIORef False
  {-
    CONSTRUCTION
  -}
  mapM_ (\x -> do
    row <- hBoxNew True 2
    xs <- mapM (radioButtonNewWithLabelFromWidget dX) x
    mapM_ (\y -> boxPackStart row y PackGrow 0) xs

    set box' [ containerChild := row, boxChildPacking row := PackNatural ]

    return xs
    ) $ groupBy 3 ds

  set row   [ containerChild := dX, boxChildPacking dX := PackNatural
            , containerChild := dX', boxChildPacking dX' := PackGrow
            ]
  set box'  [ containerChild := row, boxChildPacking row := PackNatural ]
  set txt   [ textBufferText := "\n\n" ]
  set view  [ textViewEditable := False 
            , textViewLeftMargin := 2
            ]
  set view' [ containerChild := view
            , scrolledWindowShadowType := ShadowOut
            , scrolledWindowHscrollbarPolicy := PolicyAutomatic
            , scrolledWindowVscrollbarPolicy := PolicyNever
            ]
  set box   [ containerBorderWidth := 5 
            , containerChild := view', boxChildPacking view' := PackNatural
            , containerChild := num, boxChildPacking num := PackNatural
            , containerChild := box', boxChildPacking box' := PackNatural
            , containerChild := button, boxChildPacking button := PackNatural 
            , containerChild := timer, boxChildPacking timer := PackNatural
            ]
  set frame [ containerChild := box ]
  set exp   [ expanderExpanded := True
            , containerChild := frame
            , containerBorderWidth := 5
            ]
  {-
    LOGIC
  -}
  on button buttonActivated $ do
    bools <- mapM toggleButtonGetActive =<< radioButtonGetGroup dX
    t <- entryGetText dX'
    i <- spinButtonGetValue num
    g <- newStdGen
    textBufferSetText txt $ f (filterDice i bools ((reverse ds)++[t])) g

  on timer toggled $ do
    go <- readIORef check
    if (not go)
    then do
      writeIORef check True
      bool <- toggleButtonGetActive timer
      if bool then buttonClicked button else return ()
      tmhandle <- timeoutAdd ((\_ -> do
        bool <- toggleButtonGetActive timer
        if bool
        then buttonClicked button >> return True
        else writeIORef check False >> return False) "") 5000
      return ()
    else return ()

  widgetShowAll exp
  return exp
  where f x g = case parse (dice' g) x of
                [(q,"")]  -> concat ["Rolls: ",show q,"\nTotal: ",show $ sum q,"\n"]
                [(q,s)]   -> case parse ops ((show $ sum q)++s) of
                              [(x,"")]  -> concat ["Rolls: ",show q,"\nTotal: ",x,"\n"]
                              _         -> "Operation error in dice widget\n\n"
                _         -> "Parse error in dice widget\n\n"
        --Figures out which type of dice to use for the roll, and how many
        filterDice i (True:_) (y:_) = if (head y)=='d' then (show $ floor i)++y else y
        filterDice i (False:xs) (_:ys) = filterDice i xs ys

ds :: [String]
ds = ["d3","d4","d6","d8","d10","d12","d20","d30","d100"]

--This will break if given a negative number.
groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy x qs = (take x qs) : (groupBy x $ drop x qs)
