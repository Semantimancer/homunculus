module Homunculus.Sidebar where

import Homunculus.Event
import Homunculus.Roller

import Graphics.UI.Gtk

makeSidebar :: FilePath -> VBox -> IO ()
makeSidebar dataPath box = do
  mapM_ widgetDestroy =<< containerGetChildren box
  {-
    INITIALIZATION
  -}
  widgets <- mapM (\x -> return =<< x) [makeEventWidget,makeDiceWidget]
  {-
    CONSTRUCTION
  -}
  mapM_ (\x -> set box [ containerChild := x, boxChildPacking x:= PackNatural ]) widgets
  {-
    LOGIC
  -}
  widgetShowAll box
