module Homunculus.Roller where

import Homunculus.Parser

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
    textBufferSetText txt $ f (f' i bools ((reverse ds)++[t])) g

  widgetShowAll exp
  return exp
  where f x g = case parse (dice' g) x of--case parse (script [] g) x of
                [(q,"")]  -> concat ["Rolls: ",show q,"\nTotal: "
                                    ,show $ sum q,"\n"]
                _         -> "Parse error in dice widget\n\n"
        f' i (True:_) (y:_) = (show $ floor i)++y
        f' i (False:xs) (_:ys) = f' i xs ys

ds :: [String]
ds = ["d3","d4","d6","d8","d10","d12","d20","d30","d100"]

--This will break if given a negative number.
groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy x qs = (take x qs) : (groupBy x $ drop x qs)
