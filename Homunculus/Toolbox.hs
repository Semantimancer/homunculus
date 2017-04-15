module Homunculus.Toolbox where

import Homunculus.Calendar
import Homunculus.Generator
import Homunculus.Initiative

import Graphics.UI.Gtk

makeToolbox :: FilePath -> VBox -> IO ()
makeToolbox dataPath box = do
  mapM_ widgetDestroy =<< containerGetChildren box
  {-
    INITIALIZATION
  -}
  toolbox <- notebookNew
  gen <- makeGenerator dataPath
  ini <- makeInitiative 
  cal <- makeCalendar

  {-
    CONSTRUCTION
  -}
  notebookAppendPage toolbox gen "Generator"
  notebookAppendPage toolbox ini "Initiative Tracker"
  notebookAppendPage toolbox cal "Calendar"

  set box     [ containerChild := toolbox ]
  {-
    LOGIC
  -}
  widgetShowAll box
