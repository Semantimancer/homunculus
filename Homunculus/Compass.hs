module Homunculus.Compass where

import Data.Fixed (mod')
import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM

import Control.Concurrent.MVar
import System.Directory (getAppUserDataDirectory)
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

  dataPath <- getAppUserDataDirectory "homunculus"
  compass <- imageSurfaceCreateFromPNG $ dataPath++"/compass.png" --These are unsafe because there's no case for what happens if they fail
  needle <- imageSurfaceCreateFromPNG $ dataPath++"/needle.png"   --That will be added soon.

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

  on canvas draw $ drawCanvasBackground canvas compass
  on canvas draw $ drawCanvasForeground canvas currentAngle needle

  on canvas buttonPressEvent $ tryEvent $ do
    LeftButton <- eventButton
    liftIO $ do
      busy <- readMVar control
      if busy
      then return ()
      else do
        swapMVar control True
        i <- randomRIO (1,8) :: IO Int

        swapMVar finalAngle $ (45*(realToFrac i))+360
        a <- takeMVar currentAngle
        putMVar currentAngle (a `mod'` 360)

        timeoutAdd (do widgetQueueDraw frame
                       c <- readMVar currentAngle
                       f <- readMVar finalAngle
                       if (c<f) then return () else swapMVar control False >> return ()
                       return $ c<f) 30
        return ()


  widgetShowAll exp
  return exp

drawCanvasBackground :: WidgetClass widget => widget -> Surface -> Render ()
drawCanvasBackground canvas img = do
  imgWidth <- imageSurfaceGetWidth img
  imgHeight <- imageSurfaceGetHeight img 

  width' <- liftIO $ widgetGetAllocatedWidth canvas
  height' <- liftIO $ widgetGetAllocatedHeight canvas
  let width = realToFrac width'
      height = realToFrac height'
  

  save
  scale (width/fromIntegral imgWidth) (height/fromIntegral imgHeight)
  setSourceSurface img 0 0
  paint
  restore
  
drawCanvasForeground :: WidgetClass widget => widget -> MVar Double -> Surface -> Render ()
drawCanvasForeground canvas angle' img = do
  angle <- liftIO $ takeMVar angle'

  imgWidth <- imageSurfaceGetWidth img
  imgHeight <- imageSurfaceGetHeight img

  width' <- liftIO $ widgetGetAllocatedWidth canvas
  height' <- liftIO $ widgetGetAllocatedHeight canvas
  let width = realToFrac width'
      height = realToFrac height'
  
  save
  translate (width/2) (height/2)
  rotate (angle*pi/180)
  translate ((width/2)*(-1)) ((height/2)*(-1))
  scale (width/fromIntegral imgWidth) (height/fromIntegral imgHeight)
  setSourceSurface img 1 0
  paint
  restore

  liftIO $ putMVar angle' (angle+6)
