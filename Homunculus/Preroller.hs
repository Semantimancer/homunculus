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

  frmColumns <- frameNew
  numColumns <- spinButtonNewWithRange 1.0 99.0 1.0

  frmRolls <- frameNew
  numRolls <- spinButtonNewWithRange 1.0 99.0 1.0

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
                  , frameLabel := "Default Roll"
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
        nColumns <- spinButtonGetValue numColumns
        nRolls <- spinButtonGetValue numRolls
        nRoll <- entryGetText normRoll

        entries <- mapM (\w -> do
          --Pulls the first and last widgets, because I know these will be entries. Unsafe.
          (entryName:xs) <- containerGetChildren $ castToContainer w
          let entryRoll = last xs
          nameTxt <- entryGetText $ castToEntry entryName
          rollTxt <- entryGetText $ castToEntry entryRoll

          --This would be unsafe, but I know I will always set a placeholder text on these
          Just nameTxt' <- entryGetPlaceholderText $ castToEntry entryName
          Just rollTxt' <- entryGetPlaceholderText $ castToEntry entryRoll

          widgetDestroy w
          return (if nameTxt=="" then nameTxt' else nameTxt
                 ,if rollTxt=="" then rollTxt' else rollTxt)
          ) =<< containerGetChildren topBar
        mapM_ (\(txt,txt') -> do
          col <- vBoxNew False 10
          ent <- entryNew
          rol <- entryNew

          set ent     [ entryPlaceholderText := Just txt ]
          set rol     [ entryPlaceholderText := Just txt' ]
          set col     [ containerChild := ent, boxChildPacking ent := PackNatural ]
          set topBar  [ containerChild := col ]

          replicateM_ (floor nRolls) $ do
            gen <- newStdGen
            result <- checkButtonNewWithLabel $ roll txt' gen
            set col [ containerChild := result, boxChildPacking result := PackNatural ]
            on result buttonActivated $ widgetDestroy result >> widgetShowAll col

          set col     [ containerChild := rol, boxChildPacking rol := PackNatural ]

          ) $ take (floor nColumns) $ 
                --In case you add columns, it has an infinite list of (X,nRoll) to draw on
                entries++(drop (length entries) $ zip (map show [1..]) (repeat nRoll))
        widgetShowAll scroll

  on refreshButton buttonActivated refresh

  return vbox'
  where roll x g = case parse (fudgeDice g <> standardDice g) x of
                    [(q,"")]  -> show $ sum q
                    [(q,s)]   -> case parse ops ((show $ sum q)++s) of
                                  [(j,"")]  -> j
                                  _         -> "Parse error in operation"
                    _         -> "Parse error in roll"
