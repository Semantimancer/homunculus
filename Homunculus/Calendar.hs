module Homunculus.Calendar (makeCalendar) where

import Data.List.Utils (split)
import Graphics.UI.Gtk

makeCalendar :: IO VBox
makeCalendar = do
  {-
    INITIALIZATION
  -}

  vbox <- vBoxNew False 5

  scroll <- scrolledWindowNew Nothing Nothing
  box <- vBoxNew False 20
  --Make a simple, default month just to show the formatting and get the spacing set up
  cal <- makeCalendarMonth "" 7 30 0

  row <- hBoxNew False 15
  rowFrame1 <- frameNew
  spinColumn <- spinButtonNewWithRange 1.0 15.0 1.0

  rowFrame2 <- frameNew
  spinDays <- spinButtonNewWithRange 1.0 60.0 1.0

  rowFrame3 <- frameNew
  spinMonths <- spinButtonNewWithRange 1.0 30.0 1.0

  rowFrame4 <- frameNew
  spinOffset <- spinButtonNewWithRange 0.0 14.0 1.0

  updateButton <- buttonNewWithLabel "Update"

  optionBox <- expanderNew "Additional Options"
  options <- vBoxNew False 5

  checkCarry <- checkButtonNewWithLabel "Carry Offset"

  monthBox <- frameNew
  monthNames <- entryNew

  {-
    CONSTRUCTION
  -}

  set box         [ containerChild := cal ]
  set scroll      [ containerChild := box
                  , scrolledWindowHscrollbarPolicy := PolicyAutomatic
                  , scrolledWindowVscrollbarPolicy := PolicyAutomatic
                  ]
  set rowFrame1   [ containerChild := spinColumn
                  , frameLabel := "Days Per Week"
                  , frameShadowType := ShadowNone
                  ]
  set rowFrame2   [ containerChild := spinDays
                  , frameLabel := "Days Per Month"
                  , frameShadowType := ShadowNone
                  ]
  set rowFrame3   [ containerChild := spinMonths
                  , frameLabel := "Months Per Year"
                  , frameShadowType := ShadowNone
                  ]
  set rowFrame4   [ containerChild := spinOffset
                  , frameLabel := "Starting Day Offset"
                  , frameShadowType := ShadowNone
                  ]
  set row         [ containerChild := rowFrame1, boxChildPacking rowFrame1 := PackNatural
                  , containerChild := rowFrame2, boxChildPacking rowFrame2 := PackNatural
                  , containerChild := rowFrame3, boxChildPacking rowFrame3 := PackNatural
                  , containerChild := rowFrame4, boxChildPacking rowFrame4 := PackNatural
                  , containerChild := updateButton, boxChildPacking updateButton := PackNatural
                  , boxChildPackType updateButton := PackEnd
                  ]
  set checkCarry  [ toggleButtonActive := True 
                  , widgetTooltipText := Just "Make the first of each month start on the next day of the week."
                  ]
  set monthNames  [ entryPlaceholderText := Just "Comma-separated list" ]
  set monthBox    [ containerChild := monthNames
                  , frameLabel := "Month Names"
                  , frameShadowType := ShadowNone
                  ]
  set options     [ containerChild := checkCarry
                  , containerChild := monthBox
                  , containerBorderWidth := 5
                  ]
  set optionBox   [ containerChild := options ]
  set vbox        [ containerChild := scroll
                  , containerChild := row, boxChildPacking row := PackNatural
                  , containerChild := optionBox, boxChildPacking optionBox := PackNatural
                  , containerBorderWidth := 5
                  ]

  {-
    LOGIC
  -}

  spinButtonSetValue spinColumn 7.0
  spinButtonSetValue spinDays 30.0

  on updateButton buttonActivated $ do
    mapM_ widgetDestroy =<< containerGetChildren box

    numMonths <- spinButtonGetValueAsInt spinMonths
    numColumns <- spinButtonGetValueAsInt spinColumn
    numDays <- spinButtonGetValueAsInt spinDays
    numOffset <- spinButtonGetValueAsInt spinOffset

    carry <- toggleButtonGetActive checkCarry
    months' <- entryGetText monthNames :: IO String
    let months = split "," months'

    mapM_ (\n -> do
      let offset = if carry then numOffset+((numDays `mod` numColumns)*(n-1)) else numOffset
          month = if n<=(length months) then (months!!(n-1))++" " else ""
      cal <- makeCalendarMonth month numColumns numDays offset
      set box [ containerChild := cal ]
      ) [1..numMonths]

    widgetShowAll vbox

  widgetShowAll vbox
  return vbox

makeCalendarMonth :: String -> Int -> Int -> Int -> IO Table
makeCalendarMonth name numColumns numDays offset' = do
  let offset = offset' `mod` numColumns --No sense in adding extra columns when they're not necessary
      numRows = (numDays+offset) `div` numColumns --This can be off by 1, but the table doesn't mind
  {-
    INITIALIZATION
  -}
  
  table <- tableNew numRows numColumns True

  {-
    CONSTRUCTION
  -}

  set table [ tableRowSpacing := 5
            , tableColumnSpacing := 5
            , containerBorderWidth := 5
            ]


  days <- mapM (\dayNum -> do
    dayFrame <- frameNew
    dayBox' <- vBoxNew False 0
    dayBox <- vBoxNew True 1
    dayText <- labelNew $ Just ""

    set dayBox    [ containerChild := dayText, boxChildPacking dayText := PackNatural ]
    set dayBox'   [ containerChild := dayBox, boxChildPacking dayBox := PackNatural ]
    set dayFrame  [ containerChild := dayBox'
                  , frameLabel := concat [name,show dayNum]
                  , frameLabelXAlign := 0.01
                  , frameLabelYAlign := 0
                  ]

    let i = dayNum-1+offset
        left = i `mod` numColumns
        right = left+1
        top = i `div` numColumns
        bottom = top+1

    tableAttachDefaults table dayFrame left right top bottom
    ) [1..numDays]

  {-
    LOGIC
  -}

  return table
