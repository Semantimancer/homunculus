module Homunculus.Sidebar where

import Control.Monad.IO.Class (liftIO)
import Graphics.UI.Gtk
import System.Random (randomRIO)

makeSidebar :: FilePath -> VBox -> IO ()
makeSidebar dataPath box = do
  mapM_ widgetDestroy =<< containerGetChildren box
  {-
    INITIALIZATION
  -}
  eventWidget <- makeEventWidget
  {-
    CONSTRUCTION
  -}
  set box [ containerChild := eventWidget, boxChildPacking eventWidget := PackNatural ]
  {-
    LOGIC
  -}
  widgetShowAll box

makeEventWidget :: IO Expander
makeEventWidget = do
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
  makeEventWidget' box startingEvents
  return exp

makeEventWidget' :: VBox -> [Event] -> IO ()
makeEventWidget' v events = do
  mapM_ widgetDestroy =<< containerGetChildren v
  {-
    INITIALIZATION
  -}
  box <- vBoxNew False 3

  row <- hBoxNew False 3
  button <- buttonNewWithLabel "What Happens?"
  edit <- buttonNewWithLabel "Edit"

  ls <- mapM (\x -> labelNew $ Just $ title x) events
  ps <- mapM (\_ -> progressBarNew) events
  {-
    CONSTRUCTION
  -}
  mapM_ (\(l,p,e) -> do
      v <- eventBoxNew
      s <- hSeparatorNew

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
        LeftButton <- eventButton
        liftIO $ set p [ progressBarFraction := 1.0 ]
    ) $ zip3 ls ps events

  set row [ containerChild := button, boxChildPacking button := PackGrow
          , containerChild := edit, boxChildPacking edit := PackNatural
          ]
  set box [ containerChild := row, boxChildPacking row := PackNatural ]
  set v   [ containerChild := box, boxChildPacking box := PackNatural ]
  {-
    LOGIC
  -}
  on button buttonActivated $ do
    n <- randomRIO (0,(length events)-1)
    frac <- progressBarGetFraction (ps!!n)
    progressBarSetFraction (ps!!n) $ frac-(decr $ events!!n)
  on edit buttonActivated $ editEventWidget v events

  widgetShowAll v

editEventWidget :: VBox -> [Event] -> IO ()
editEventWidget v events = do
  mapM_ widgetDestroy =<< containerGetChildren v
  {-
    INITIALIZATION
  -}
  box <- vBoxNew False 3

  new <- buttonNewWithLabel "New Event"
  row <- hBoxNew True 3
  button <- buttonNewWithLabel "Done"
  cancel <- buttonNewWithLabel "Cancel"

  es <- mapM (\_ -> entryNew) events
  ss <- mapM (\_ -> spinButtonNewWithRange 1.0 100.0 1.0) events
  {-
    CONSTRUCTION
  -}
  mapM_ (\(e,s) -> do
      b <- vBoxNew False 0
      r <- hBoxNew True 0
      l <- labelNew $ Just "Steps: "

      set r   [ containerChild := l
              , containerChild := s
              ]
      set b   [ containerChild := e, boxChildPacking e := PackNatural
              , containerChild := r, boxChildPacking r := PackNatural
              ]
      set box [ containerChild := b, boxChildPacking b := PackNatural ]
    ) $ zip es ss
  set row [ containerChild := button
          , containerChild := cancel
          ]
  set v   [ containerChild := box, boxChildPacking box := PackNatural
          , containerChild := new, boxChildPacking new := PackNatural
          , containerChild := row, boxChildPacking row := PackNatural
          ]
  {-
    LOGIC
  -}
  mapM_ (\(s,e) -> spinButtonSetValue s (1/decr e)) $ zip ss events
  mapM_ (\(e,v) -> entrySetText e (title v)) $ zip es events

  on new buttonActivated $ do
    es' <- mapM entryGetText es
    ss' <- mapM spinButtonGetValue ss
    editEventWidget v $ (map newEvent (zip es' ss'))++[blankEvent]
  on button buttonActivated $ do
    es' <- mapM entryGetText es
    ss' <- mapM spinButtonGetValue ss
    makeEventWidget' v $ map newEvent (zip es' ss')
  on cancel buttonActivated $ makeEventWidget' v events
  widgetShowAll v
  where newEvent (x,y) = Event { title = x, tooltip = "", decr = 1/y }
        blankEvent = Event { title = "", tooltip = "", decr = 0.5 }

data Event = Event { title :: String    --Put on the label above the bar
                   , tooltip :: String  --Tooltip when hoving over
                   , decr :: Double     --Decriment function
                   }
  deriving (Eq,Ord)

startingEvents :: [Event]
startingEvents = [encounterEvent,torchEvent,lanternEvent,exhaustEvent]

torchEvent :: Event
torchEvent = Event  { title = "Torch Goes Out!"
                    , tooltip = ""
                    , decr = 0.6 --Two steps, but looks to be at 40% after first tick
                    }

exhaustEvent :: Event
exhaustEvent = Event  { title = "Exhausted!"
                      , tooltip = ""
                      , decr = 0.1 --Ten steps
                      }

lanternEvent :: Event
lanternEvent = Event  { title = "Lantern Goes Out!"
                      , tooltip = ""
                      , decr = 0.125 --Eight steps
                      }

encounterEvent :: Event
encounterEvent = Event  { title = "Random Encounter!"
                        , tooltip = ""
                        , decr = 0.25 --Four steps
                        }
