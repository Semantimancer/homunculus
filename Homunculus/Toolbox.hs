module Homunculus.Toolbox where

import Homunculus.Calendar
import Homunculus.Generator
import Homunculus.Initiative
import Homunculus.Preroller

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
  pre <- makePreroller
  cal <- makeCalendar

  {-
    CONSTRUCTION
  -}
  notebookAppendPage toolbox gen "Generator"
  notebookAppendPage toolbox ini "Initiative Tracker"
  notebookAppendPage toolbox pre "Pre-Roller"
  notebookAppendPage toolbox cal "Calendar"

  set box     [ containerChild := toolbox ]
  {-
    LOGIC
  -}
  widgetShowAll box
