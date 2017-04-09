module Homunculus.Compass where

import Data.Fixed (mod')
import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM

import Control.Concurrent.MVar
import System.Random

makeCompassWidget :: IO Expander
makeCompassWidget = do
  {-
    INITIALIZATION
  -}
  exp <- expanderNew "Compass"
  frame <- frameNew
  canvas <- drawingAreaNew

  control <- newMVar False
  currentAngle <- newMVar 0.0
  finalAngle <- newMVar 0.0

  {-
    CONSTRUCTION
  -}

  set frame [ containerChild := canvas ]
  set exp   [ containerChild := frame
            , expanderExpanded := True
            , containerBorderWidth := 5
            ]

  widgetSetSizeRequest frame 200 200

  {-
    LOGIC
  -}

  on canvas draw $ drawCanvasBackground canvas
  on canvas draw $ drawCanvasForeground canvas currentAngle

  on canvas buttonPressEvent $ tryEvent $ do
    LeftButton <- eventButton
    liftIO $ do
      busy <- readMVar control
      if busy
      then return ()
      else do
        swapMVar control True
        i <- randomRIO (1,8) :: IO Int

        swapMVar finalAngle $ (2*pi)+(pi/4)*(realToFrac i)
        a <- takeMVar currentAngle
        putMVar currentAngle (a `mod'` (2*pi))

        timeoutAdd (do widgetQueueDraw frame
                       c <- readMVar currentAngle
                       f <- readMVar finalAngle
                       if (c<f) then return () else swapMVar control False >> return ()
                       return $ c<f) 30
        return ()


  widgetShowAll exp
  return exp

drawCanvasBackground :: WidgetClass widget => widget -> Render ()
drawCanvasBackground canvas = do
  width' <- liftIO $ widgetGetAllocatedWidth canvas
  height' <- liftIO $ widgetGetAllocatedHeight canvas
  let width = realToFrac width'
      height = realToFrac height'
      outerRadius = 65 
      innerRadius = 60

  setSourceRGB 0.98 0.95 0.85 --White
  arc (width/2) (height/2) innerRadius 0 (2*pi)
  fill

  setSourceRGB 0.6 0.6 0.5    --Grey
  setLineWidth 15
  arc ((width/2)+3) ((height/2)+3) (outerRadius-1) 0 (2*pi)
  stroke

  setSourceRGB 1.0 0.84 0     --Gold
  setLineWidth 15
  arc (width/2) (height/2) outerRadius 0 (2*pi)
  --fill
  stroke

drawCanvasForeground :: WidgetClass widget => widget -> MVar Double -> Render ()
drawCanvasForeground canvas angle' = do
  width' <- liftIO $ widgetGetAllocatedWidth canvas
  height' <- liftIO $ widgetGetAllocatedHeight canvas
  angle <- liftIO $ takeMVar angle'
  let width = realToFrac width'
      height = realToFrac height'
      radius = 50

  setSourceRGB 1 0 0          --Red
  setLineWidth 3
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  moveTo (width/2) (height/2)
  lineTo ((width/2) + (radius * sin angle)) ((height/2) + (radius * cos angle))
  stroke

  liftIO $ putMVar angle' (angle+0.1)
