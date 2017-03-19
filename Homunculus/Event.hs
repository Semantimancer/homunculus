module Homunculus.Event where

import Control.Monad (filterM)
import Control.Monad.IO.Class
import Graphics.UI.Gtk
import System.Directory (renameFile, doesFileExist)
import System.Random (randomRIO)

makeEventWidget :: FilePath -> IO Expander
makeEventWidget dataPath = do
  {-
    INITIALIZATION
  -}
  exp <- expanderNew "Time Passes!"
  frame <- frameNew
  box <- vBoxNew False 3
  {-
    CONSTRUCTION
  -}
  set box   [ containerBorderWidth := 5 ]
  set frame [ containerChild := box ]
  set exp   [ expanderExpanded := True
            , containerChild := frame
            , containerBorderWidth := 5
            ]
  {-
    LOGIC
  -}
  savedEventsExist <- doesFileExist $ dataPath++"/events.save"
  if savedEventsExist
  then do
    savedEvents <- readFile $ dataPath++"/events.save"
    --This needs some extra safety added in
    makeEventWidget' box dataPath (read savedEvents)
  else makeEventWidget' box dataPath startingEvents
  return exp

makeEventWidget' :: VBox -> FilePath -> [Event] -> IO ()
makeEventWidget' v dataPath events = do
  mapM_ widgetDestroy =<< containerGetChildren v
  {-
    INITIALIZATION
  -}
  box <- vBoxNew False 3

  row <- hBoxNew False 3
  button <- buttonNewWithLabel "What Happens?"
  edit <- buttonNewWithLabel "Edit"

  --ls <- mapM (\x -> labelNew $ Just $ title x) events
  ls <- mapM (\x -> checkButtonNewWithLabel $ title x) events
  ps <- mapM (\_ -> progressBarNew) events

  {-
    CONSTRUCTION
  -}
  mapM_ (\(l,p,e) -> do
    v <- eventBoxNew
    s <- hSeparatorNew

    set l   [ toggleButtonActive := True ]
    set p   [ progressBarFraction := 1.0
            , progressBarText := ""
            ]
    set v   [ containerChild := p
            , eventBoxVisibleWindow := False
            ]
    set box [ containerChild := l, boxChildPacking l := PackNatural
            , containerChild := v, boxChildPacking v := PackNatural
            , containerChild := s, boxChildPacking s := PackNatural
            ]

    on v buttonPressEvent $ tryEvent $ do
        RightButton <- eventButton
        liftIO $ do
          m <- menuNew
          plus1 <- menuItemNewWithLabel "Add 1"
          reset <- menuItemNewWithLabel "Reset"

          mapM_ (menuShellAppend m) [plus1,reset]

          on plus1 menuItemActivate $ do
            x <- progressBarGetFraction p 
            set p [ progressBarFraction := x+(decr e) ]
          on reset menuItemActivate $ 
            set p [ progressBarFraction := 1.0 ]
          menuPopup m Nothing
          widgetShowAll m
    ) $ zip3 ls ps events

  set row [ containerChild := button
          , boxChildPacking button := PackGrow
          , containerChild := edit
          , boxChildPacking edit := PackNatural
          ]
  set box [ containerChild := row
          , boxChildPacking row := PackNatural 
          ]
  set v   [ containerChild := box
          , boxChildPacking box := PackNatural 
          ]
  {-
    LOGIC
  -}
  on button buttonActivated $ do
    ps' <- filterM (\(x,_,_) -> toggleButtonGetActive x)
            $ zip3 ls ps events
    n <- randomRIO (0,length ps'-1)

    let (_,p,e) = ps'!!n
    frac <- progressBarGetFraction p
    progressBarSetFraction p $ frac-(decr e)
  on edit buttonActivated $ editEventWidget v dataPath events

  widgetShowAll v

editEventWidget :: VBox -> FilePath -> [Event] -> IO ()
editEventWidget v dataPath events = do
  mapM_ widgetDestroy =<< containerGetChildren v
  {-
    INITIALIZATION
  -}
  box <- vBoxNew False 3

  new <- buttonNewWithLabel "New Event"
  row <- hBoxNew True 3
  button <- buttonNewWithLabel "Done"
  save <- buttonNewWithLabel "Save"
  cancel <- buttonNewWithLabel "Cancel"

  es <- mapM (\_ -> entryNew) events
  ds <- mapM (\_ -> buttonNewWithLabel "Delete") events
  ss <- mapM (\_ -> spinButtonNewWithRange 1.0 100.0 1.0) events
  {-
    CONSTRUCTION
  -}
  mapM_ (\(e,s,d) -> do
    b <- vBoxNew False 0
    q <- hBoxNew False 0
    r <- hBoxNew True 0
    l <- labelNew $ Just "Steps: "

    set q   [ containerChild := e
            , containerChild := d
            , boxChildPacking d := PackNatural
            ]
    set r   [ containerChild := l
            , containerChild := s
            ]
    set b   [ containerChild := q
            , boxChildPacking q := PackNatural
            , containerChild := r
            , boxChildPacking r := PackNatural
            ]
    set box [ containerChild := b
            , boxChildPacking b := PackNatural 
            ]
    ) $ zip3 es ss ds
  set row [ containerChild := button
          , containerChild := save
          , containerChild := cancel
          ]
  set v   [ containerChild := box
          , boxChildPacking box := PackNatural
          , containerChild := new
          , boxChildPacking new := PackNatural
          , containerChild := row
          , boxChildPacking row := PackNatural
          ]
  {-
    LOGIC
  -}
  mapM_ (\(s,e) -> spinButtonSetValue s (1/decr e)) $ zip ss events
  mapM_ (\(e,v) -> entrySetText e (title v)) $ zip es events
  mapM_ (\(d,e) -> on d buttonActivated $ editEventWidget v dataPath $ filter (/=e) events) $ zip ds events

  on new buttonActivated $ do
    es' <- mapM entryGetText es
    ss' <- mapM spinButtonGetValue ss
    editEventWidget v dataPath $ (map newEvent (zip es' ss'))++[blankEvent]
  on button buttonActivated $ do
    es' <- mapM entryGetText es
    ss' <- mapM spinButtonGetValue ss
    makeEventWidget' v dataPath $ map newEvent (zip es' ss')
  on save buttonActivated $ do
    es' <- mapM entryGetText es
    ss' <- mapM spinButtonGetValue ss
    writeFile (dataPath++"/events.save.temp") $ show $ map newEvent (zip es' ss')
    renameFile (dataPath++"/events.save.temp") (dataPath++"/events.save")
  on cancel buttonActivated $ makeEventWidget' v dataPath events
  widgetShowAll v
  where newEvent (x,y) = Event x "" (1/y)
        blankEvent = Event { title = "", tooltip = "", decr = 0.5 }

data Event = Event { title :: String    --Label above the bar
                   , tooltip :: String  --Tooltip when hoving over
                   , decr :: Double     --Decriment function
                   }
  deriving (Read,Show,Eq,Ord)

startingEvents :: [Event]
startingEvents = [encounterEvent,torchEvent,lanternEvent
                 ,exhaustEvent]

torchEvent :: Event
torchEvent = Event "Torch Goes Out!" "" 0.5

exhaustEvent :: Event
exhaustEvent = Event "Exhausted!" "" 0.1

lanternEvent :: Event
lanternEvent = Event "Lantern Goes Out!" "" 0.125

encounterEvent :: Event
encounterEvent = Event "Random Encounter!" "" 0.25
