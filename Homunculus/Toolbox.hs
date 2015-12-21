module Homunculus.Toolbox where

import Homunculus.Generator

import Graphics.UI.Gtk

makeToolbox :: FilePath -> VBox -> IO ()
makeToolbox dataPath box = do
  mapM_ widgetDestroy =<< containerGetChildren box
  {-
    INITIALIZATION
  -}
  toolbox <- notebookNew
  gen <- makeGenerator dataPath
  {-
    CONSTRUCTION
  -}
  _ <- notebookAppendPage toolbox gen "Generator"

  set box     [ containerChild := toolbox ]
  {-
    LOGIC
  -}
  widgetShowAll box
