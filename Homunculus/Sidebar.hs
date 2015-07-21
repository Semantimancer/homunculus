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
  timeWidget <- makeTimeWidget
  {-
    CONSTRUCTION
  -}
  set box [ containerChild := timeWidget, boxChildPacking timeWidget := PackNatural ]
  {-
    LOGIC
  -}
  widgetShowAll box

makeTimeWidget :: IO Expander
makeTimeWidget = do
  {-
    INITIALIZATION
  -}
  frame' <- expanderNew "Time Passes!"
  frame <- frameNew
  box <- vBoxNew False 3

  l'@(enc':exh':lan':tor':_) <- mapM (\x -> labelNew $ Just x)
    ["Random Encounter!","Exhausted!","Lantern Goes Out!","Torch Goes Out!"]
  l@(enc:exh:lan:tor:_) <- mapM (\_ -> progressBarNew ) [0..((length l')-1)]
  le <- mapM (\_ -> eventBoxNew ) [0..((length l')-1)]

  button <- buttonNewWithLabel "What Happens?"
  {-
    CONSTRUCTION
  -}
  mapM_ (\(x,x',xe) -> do
          row <- hSeparatorNew
          set x   [ progressBarFraction := 1.0
                  , progressBarText := ""
                  ]
          set xe  [ containerChild := x 
                  , eventBoxVisibleWindow := False
                  ]
          set box [ containerChild := x', boxChildPacking x' := PackNatural
                  , containerChild := xe, boxChildPacking xe := PackNatural
                  , containerChild := row, boxChildPacking row := PackNatural
                  ]
          ) $ zip3 l l' le--[(enc,enc'),(exh,exh'),(lan,lan'),(tor,tor')]

  set (l'!!0) [ widgetTooltipText := Just $ concat ["Full: Nothing around right now.\n"
                                                   ,"Partial: You start seeing signs of "
                                                   ,"something up ahead...\nEmpty: It's "
                                                   ,"right under your nose!"]
              ]
  set (l'!!1) [ widgetTooltipText := Just $ concat ["Full: Everyone is refreshed and ready"
                                                   ," to go!\nPartial: The party's slowing "
                                                   ,"down a bit.\nEmpty: They collapse "
                                                   ,"from exhaustion."]
              ]
  set (l'!!2) [ widgetTooltipText := Just $ concat ["Full: Plenty of oil in the lantern."
                                                   ,"\nPartial: It's flickering a bit, but "
                                                   ,"it's not bad.\nEmpty: It gutters, then"
                                                   ," goes out."]
              ]
  set (l'!!3) [ widgetTooltipText := Just $ concat ["Full: The torch is burning brightly.\n"
                                                   ,"Partial: It's getting a bit dim in "
                                                   ,"here...\nEmpty: The torch dies!"]
              ]
  set button  [ widgetTooltipText := Just $ concat ["Hit the button while the PCs are away "
                                                   ,"from civilization and watch their "
                                                   ,"resources decline while they approach "
                                                   ,"another random encounter.\n\nClick "
                                                   ,"on the progress bars to reset them."]
              ]
  set box     [ containerChild := button, boxChildPacking button := PackNatural 
              , containerBorderWidth := 5]
  set frame   [ containerChild := box
              , frameShadowType := ShadowEtchedOut
              ]
  set frame'  [ expanderExpanded := True 
              , containerChild := frame
              , containerBorderWidth := 5
              ]
  {-
    LOGIC
  -}
  --When you click the "progress bar" (actually the EventWindow around the bar)
  --you will reset the bar to full
  mapM_ (\(x,xe) -> on xe buttonPressEvent $ tryEvent $ do
      LeftButton <- eventButton
      liftIO $ set x [ progressBarFraction := 1.0 ]
    ) $ zip l le

  on button buttonActivated $ do
    n <- randomRIO (0,(length l)-1)
    frac <- progressBarGetFraction (l!!n)
    text <- labelGetText (l'!!n)
    progressBarSetFraction (l!!n) (findNewFrac text frac)
    widgetShowAll frame

  return frame'
  where f x y = if x<y then 0.0 else x-y
        findNewFrac text frac = case text of
          "Exhausted!"        -> f frac 0.05    --20 steps
          "Lantern Goes Out!" -> f frac 0.125   --8 steps
          "Random Encounter!" -> f frac 0.25    --4 steps
          "Torch Goes Out!"   -> f frac 0.5     --2 steps
