module Homunculus.Toolbox where

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

  {-
    CONSTRUCTION
  -}
  _ <- notebookAppendPage toolbox gen "Generator"
  _ <- notebookAppendPage toolbox ini "Initiative Tracker"

  set box     [ containerChild := toolbox ]
  {-
    LOGIC
  -}
  widgetShowAll box
