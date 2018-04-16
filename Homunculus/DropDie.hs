module Homunculus.DropDie where

import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM

import Control.Monad (when)
import Control.Concurrent.MVar
import Data.List (isInfixOf)
import Data.Fixed (mod')
import System.Random

data DropDie = DropDie { dropX, dropY, dropAngle :: Double
                       , dropR, dropG, dropB :: Double
                       , dropFace :: Int
                       }

randomDice :: StdGen -> [DropDie]
randomDice g = let (i,g') = random g :: (Int,StdGen) in (randomDie $ mkStdGen i) : randomDice g'

randomDie :: StdGen -> DropDie
randomDie g = let is = randomRs (0,6000) g :: [Double]
              in DropDie { dropX = is!!0  --These will later have a modulus applied to them to make sure they fit within the screen
                         , dropY = is!!1  --But that can't be done here, in order to allow for the window being resized
                         , dropAngle = (is!!2) `mod'` 2
                         , dropR = (is!!3) `mod'` 0.5
                         , dropG = (is!!4) `mod'` 0.5
                         , dropB = (is!!5) `mod'` 0.5
                         , dropFace = round $ ((is!!6) `mod'` 5)+1
                         }

corners :: DropDie -> [(Double,Double)]
corners d = let x = dropX d
                y = dropY d
                sinxLength = (sin $ dropAngle d)*50  --The formula is 1/2 * length * [sin/cos] theta.
                cosxLength = (cos $ dropAngle d)*50  --Since we have a static length, I simplified.
            in [(x+sinxLength,y+cosxLength),(x+cosxLength,y-sinxLength),(x-sinxLength,y-cosxLength),(x-cosxLength,y+sinxLength)]

makeDropDice :: IO HBox
makeDropDice = do
  {-
    INITIALIZATION
  -}
  hbox <- hBoxNew False 5

  frame <- frameNew
  canvas <- drawingAreaNew

  optsFrame <- frameNew
  opts <- vBoxNew False 2
  spin <- spinButtonNewWithRange 1.0 20.0 1.0
  fileDialog <- fileChooserDialogNew (Just "Choose Background Image") Nothing FileChooserActionOpen [("Open",ResponseOk),("Cancel",ResponseNone)]
  file <- fileChooserButtonNewWithDialog fileDialog
  drop <- buttonNewWithLabel "Drop"

  dice <- newMVar []
  img <- newMVar Nothing

  {-
    CONSTRUCTION
  -}
  set opts      [ containerChild := spin, boxChildPacking spin := PackNatural 
                , containerChild := file, boxChildPacking file := PackNatural
                , containerChild := drop, boxChildPacking drop := PackNatural 
                , containerBorderWidth := 5
                ]
  set frame     [ containerChild := canvas 
                , frameLabel := "Drop Table"
                ]
  set optsFrame [ containerChild := opts 
                , frameLabel := "Options"
                ]
  set hbox      [ containerChild := frame, boxChildPacking frame := PackGrow
                , containerChild := optsFrame, boxChildPacking optsFrame := PackNatural
                , containerBorderWidth := 5
                ]

  {-
    LOGIC
  -}

  on canvas draw $ drawDiceWidget canvas img dice

  on file fileChooserButtonFileSet $ do
    fp' <- fileChooserGetFilename file

    when (fp'/=Nothing) $ let (Just fp) = fp'
      in when (".png" `isInfixOf` fp) $ do  
          newSurface <- imageSurfaceCreateFromPNG fp --This is dangerous! The check in the line above isn't sufficient.
          swapMVar img $ Just newSurface
          widgetQueueDraw canvas

  on drop buttonActivated $ do
    g <- newStdGen
    num <- spinButtonGetValue spin
    swapMVar dice $ take (round num) $ randomDice g

    widgetQueueDraw canvas

  widgetShowAll hbox
  return hbox

drawDiceWidget :: WidgetClass widget => widget -> MVar (Maybe Surface) -> MVar [DropDie] -> Render ()
drawDiceWidget canvas mImg mDice = do
  width' <- liftIO $ widgetGetAllocatedWidth canvas
  height' <- liftIO $ widgetGetAllocatedHeight canvas
  dice <- liftIO $ readMVar mDice
  img' <- liftIO $ readMVar mImg
  let width = fromIntegral width'
      height = fromIntegral height'

  drawBackground canvas width' height' img'

  mapM_ (\x -> drawDie canvas width height x) dice

drawBackground :: WidgetClass widget => widget -> Int -> Int -> Maybe Surface -> Render ()
drawBackground _ _ _ Nothing = return ()
drawBackground widget width height (Just img) = do
  imgWidth <- imageSurfaceGetWidth img
  imgHeight <- imageSurfaceGetHeight img

  save
  scale (fromIntegral width/fromIntegral imgWidth) (fromIntegral height/fromIntegral imgHeight)
  setSourceSurface img 0 0
  paint
  restore

drawDie :: WidgetClass widget => widget -> Double -> Double -> DropDie -> Render ()
drawDie canvas width height d = do
  save

  let x = width * (((dropX d) `mod'` 100)/100)                  --To account for the window resizing, dropX/dropY are used to calculate
      y = height * (((dropY d) `mod'` 100)/100)                 --a percentage, which is applied to the current canvas width/height

      ps@(p1:p2:p3:p4:_) = corners $ d { dropX = x, dropY = y } --Though x and y are modified here, this modification is not retained.
                                                                --If it was, it could mutate the map when the window is resized.
  uncurry moveTo p1
  mapM_ (uncurry lineTo) ps

  setSourceRGB (dropR d) (dropG d) (dropB d)
  fill

  moveTo (x-12) (y+12)
  setFontSize 40
  setSourceRGB 1 1 1
  showText $ show $ dropFace d

  restore
