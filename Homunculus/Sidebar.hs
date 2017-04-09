module Homunculus.Sidebar where

import Homunculus.Compass
import Homunculus.Event
import Homunculus.Names
import Homunculus.Roller

import Graphics.UI.Gtk

makeSidebar :: FilePath -> VBox -> IO ()
makeSidebar dataPath box = do
  mapM_ widgetDestroy =<< containerGetChildren box
  {-
    INITIALIZATION
  -}
  widgets <- mapM (\x -> return =<< x) [makeNameWidget,makeEventWidget dataPath,makeDiceWidget,makeCompassWidget]
  {-
    CONSTRUCTION
  -}
  mapM_ (\x -> set box [ containerChild := x, boxChildPacking x:= PackNatural ]) widgets
  {-
    LOGIC
  -}
  widgetShowAll box
