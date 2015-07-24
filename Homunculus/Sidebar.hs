module Homunculus.Sidebar where

import Homunculus.Event

import Graphics.UI.Gtk

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
