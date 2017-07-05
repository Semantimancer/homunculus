module Homunculus.Preroller where

import Homunculus.Parser

import Control.Monad
import Graphics.UI.Gtk
import System.Random

makePreroller :: IO VBox
makePreroller = do
  {-
    INITIALIZATION
  -}
  vbox' <- vBoxNew False 0
  scroll <- scrolledWindowNew Nothing Nothing
  vbox <- vBoxNew False 0
  topBar <- hBoxNew True 3
  bottomBar <- hBoxNew False 15

  --labColumns <- labelNew $ Just "Number of Columns"
  frmColumns <- frameNew
  numColumns <- spinButtonNewWithRange 1.0 20.0 1.0

  --labRolls <- labelNew $ Just "Number of Rolls"
  frmRolls <- frameNew
  numRolls <- spinButtonNewWithRange 1.0 50.0 1.0

  frmRoll <- frameNew
  normRoll <- entryNew

  refreshButton <- buttonNewWithLabel "Refresh"
  {-
    CONSTRUCTION
  -}
  set frmColumns  [ containerChild := numColumns
                  , frameLabel := "Number of Columns"
                  , frameShadowType := ShadowNone
                  ]
  set frmRolls    [ containerChild := numRolls
                  , frameLabel := "Number of Rolls"
                  , frameShadowType := ShadowNone
                  ]
  set frmRoll     [ containerChild := normRoll
                  , frameLabel := "Roll"
                  , frameShadowType := ShadowNone
                  ]
  set bottomBar   [ containerChild := frmColumns, boxChildPacking frmColumns := PackNatural
                  , containerChild := frmRolls, boxChildPacking frmRolls := PackNatural
                  , containerChild := frmRoll, boxChildPacking frmRoll := PackNatural
                  , containerChild := refreshButton
                  , boxChildPacking refreshButton := PackNatural
                  , boxChildPackType refreshButton := PackEnd
                  ]
  set vbox        [ containerChild := topBar
                  , containerChild := bottomBar, boxChildPacking bottomBar := PackNatural 
                  , containerBorderWidth := 5
                  ]
  set scroll      [ containerChild := vbox 
                  , scrolledWindowHscrollbarPolicy := PolicyAutomatic
                  , scrolledWindowVscrollbarPolicy := PolicyAutomatic
                  ]
  set vbox'       [ containerChild := scroll ]
  {-
    LOGIC
  -}
  entrySetText normRoll "1d20"

  let refresh = do
        --mapM_ widgetDestroy =<< containerGetChildren topBar
        nColumns <- spinButtonGetValue numColumns
        nRolls <- spinButtonGetValue numRolls
        nRoll <- entryGetText normRoll

        entries <- mapM (\w -> do
          (entry:_) <- containerGetChildren $ castToContainer w
          txt <- entryGetText $ castToEntry entry
          widgetDestroy w
          return txt
          ) =<< containerGetChildren topBar
        mapM_ (\txt -> do
          col <- vBoxNew False 10
          ent <- entryNew

          set col     [ containerChild := ent, boxChildPacking ent := PackNatural ]
          set topBar  [ containerChild := col ]

          entrySetText ent txt
          replicateM_ (floor nRolls) $ do
            gen <- newStdGen
            result <- labelNew $ Just $ roll nRoll gen
            set col [ containerChild := result, boxChildPacking result := PackNatural ]

          ) (take (floor nColumns) $ entries++(drop (length entries) $ map show [1..]))
        widgetShowAll scroll

  on refreshButton buttonActivated refresh

  return vbox'
  where roll x g = case parse (fudgeDice g <> standardDice g) x of
                    [(q,"")]  -> show $ sum q
                    [(q,s)]   -> case parse ops ((show $ sum q)++s) of
                                  [(j,"")]  -> j
                                  _         -> "Parse error in operation"
                    _         -> "Parse error in roll"
