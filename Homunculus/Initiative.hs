module Homunculus.Initiative where

import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import Graphics.UI.Gtk
import System.Random

data Row = Row { name :: String
               , initiative :: Int
               , notes :: String
               , start :: Bool
               }
  deriving (Eq,Read,Show)

instance Ord Row where
  a<=b = (initiative a)>=(initiative b)

readRow :: String -> Maybe Row
readRow str = case reads str of
  [(x,"")]  -> Just x
  _         -> Nothing

makeInitiative :: IO VBox
makeInitiative = do
  {-
    INITIALIZATION
  -}
  vbox <- vBoxNew False 0

  model <- listStoreNew []
  tree <- makeTreeView model

  toolbar <- toolbarNew
  new <- toolButtonNewFromStock stockNew
  order <- toolButtonNewFromStock stockSortDescending
  clear <- toolButtonNewFromStock stockClear
  sep <- separatorToolItemNew
  next <- toolButtonNewFromStock stockMediaNext

  bottom <- hBoxNew False 5
  rLabel <- labelNew $ Just "Round Counter:" 
  round <- spinButtonNewWithRange 1.0 999.0 1.0
  {-
    CONSTRUCTION
  -}
  mapM_ (\x -> toolbarInsert toolbar x (-1)) [new,order,clear]
  toolbarInsert toolbar sep (-1)
  toolbarInsert toolbar next (-1)

  set order   [ toolButtonLabel := Just "Sort" ]
  set sep     [ separatorToolItemDraw := False 
              , toolItemExpand := True
              ]
  set toolbar [ toolbarStyle := ToolbarBoth ]
  set round   [ spinButtonSnapToTicks := True
              , spinButtonNumeric := True
              , spinButtonUpdatePolicy := UpdateIfValid
              ]
  set bottom  [ containerChild := rLabel, boxChildPacking rLabel := PackNatural
              , containerChild := round
              , containerBorderWidth := 5
              ]
  set vbox    [ containerChild := toolbar, boxChildPacking toolbar := PackNatural
              , containerChild := tree
              , containerChild := bottom, boxChildPacking bottom := PackNatural
              ]
  {-
    LOGIC
  -}
  onToolButtonClicked new $ do
    i <- randomRIO (1,20)
    len <- listStoreGetSize model
    _ <- listStoreAppend model $ Row "" i "" (len==0)
    return ()
  onToolButtonClicked order $ do
    list <- listStoreToList model
    listStoreClear model
    mapM_ (listStoreAppend model) (sort list)
  onToolButtonClicked clear $ listStoreClear model
  onToolButtonClicked next $ do
    list <- listStoreToList model
    listStoreClear model
    let list' = case list of
          []      -> []
          (x:xs)  -> xs++[x]
    mapM_ (listStoreAppend model) list'
    if list'==[] 
    then return ()
    else if start $ head list'
         then do
          v <- spinButtonGetValue round
          spinButtonSetValue round (v+1.0)
         else return ()


  widgetShowAll vbox
  return vbox

makeTreeView :: ListStore Row -> IO ScrolledWindow
makeTreeView model = do
  {-
    INITIALIZATION
  -}
  view' <- scrolledWindowNew Nothing Nothing
  view <- treeViewNewWithModel model
  
  col0 <- treeViewColumnNew
  col1 <- treeViewColumnNew
  col2 <- treeViewColumnNew
  col3 <- treeViewColumnNew

  renderer0 <- cellRendererToggleNew
  renderer1 <- cellRendererTextNew
  renderer2 <- cellRendererTextNew
  renderer3 <- cellRendererTextNew

  {-
    CONSTRUCTION
  -}
  set col0  [ treeViewColumnResizable := True
            , treeViewColumnFixedWidth := 75
            , treeViewColumnTitle := "Round Start"
            ]
  set col1  [ treeViewColumnResizable := True
            , treeViewColumnFixedWidth := 100
            , treeViewColumnTitle := "Name"
            ]
  set col2  [ treeViewColumnResizable := True
            , treeViewColumnTitle := "Initiative"
            , treeViewColumnFixedWidth := 75
            ]
  set col3  [ treeViewColumnResizable := False
            , treeViewColumnExpand := True
            , treeViewColumnTitle := "Notes"
            , treeViewColumnSizing := TreeViewColumnAutosize
            ]
  set view  [ treeViewHeadersVisible := True
            , treeViewReorderable := True
            , treeViewRulesHint := True
            , treeViewEnableTreeLines := True
            ]
  set view' [ containerChild := view
            , scrolledWindowHscrollbarPolicy := PolicyAutomatic
            , scrolledWindowVscrollbarPolicy := PolicyAutomatic
            ]

  treeViewColumnPackStart col0 renderer0 False
  treeViewColumnPackStart col1 renderer1 False
  treeViewColumnPackStart col2 renderer2 True
  treeViewColumnPackStart col3 renderer3 True

  mapM_ (treeViewAppendColumn view) [col0,col2,col1,col3]

  {-
    LOGIC
  -}
  --This describes what each cell actually displays
  cellLayoutSetAttributes col0 renderer0 model $
    \row -> [ cellToggleActive := start row, cellToggleRadio := True ]
  cellLayoutSetAttributes col1 renderer1 model $
    \row -> [ cellText := name row, cellTextEditable := True ]
  cellLayoutSetAttributes col2 renderer2 model $
    \row -> [ cellText := show $ initiative row, cellTextEditable := True ]
  cellLayoutSetAttributes col3 renderer3 model $
    \row -> [ cellText := notes row, cellTextEditable := True ]

  --React to user input
  on renderer0 cellToggled $ \i' -> do
    l <- listStoreToList model
    listStoreClear model
    mapM_ (\r -> listStoreAppend model $ r { start = False }) l
    let i = read i' :: Int
    listStoreSetValue model i $ (l!!i) { start = True }
  on renderer1 edited $ \[i] str -> do
    val <- listStoreGetValue model i
    listStoreSetValue model i val { name = str }
  on renderer2 edited $ \[i] str -> do
    val <- listStoreGetValue model i
    listStoreSetValue model i val { initiative = read $ filter (`elem` ['0'..'9']) str }
  on renderer3 edited $ \[i] str -> do
    val <- listStoreGetValue model i
    listStoreSetValue model i val { notes = str }

  on view buttonPressEvent $ tryEvent $ do
    RightButton <- eventButton
    liftIO $ do
      m <- menuNew
      new <- menuItemNewWithLabel "New"
      cut <- menuItemNewWithLabel "Cut"
      cop <- menuItemNewWithLabel "Copy"
      pas <- menuItemNewWithLabel "Paste"
      del <- menuItemNewWithLabel "Delete"
      srt <- menuItemNewWithLabel "Sort List"

      mapM_ (menuShellAppend m) [new,cut,cop,pas,del,srt]

      tree <- treeViewGetSelection view
      sel <- treeSelectionGetSelectedRows tree
      let [[index]] = if (length sel>0) then sel else [[-1]]

      clipboard <- clipboardGet selectionClipboard

      on new menuItemActivate $ do
        i <- randomRIO (1,20)
        listStoreInsert model (index+1) $ Row "" i "" (index==(-1))
      on cut menuItemActivate $
        if index==(-1)
        then return ()
        else do
          v <- listStoreGetValue model index
          clipboardSetText clipboard (show v)
          listStoreRemove model index
      on cop menuItemActivate $ 
        if index==(-1) 
        then return ()
        else do
          v <- listStoreGetValue model index 
          clipboardSetText clipboard (show v)
      on pas menuItemActivate $ clipboardRequestText clipboard 
                                  (pasteRow model index) 
      on del menuItemActivate $  
        if index==(-1) then return () 
                       else listStoreRemove model index
      on srt menuItemActivate $ do
        list <- listStoreToList model
        listStoreClear model
        mapM_ (listStoreAppend model) (sort list)

      menuPopup m Nothing
      widgetShowAll m

  widgetShowAll view'
  return view'

pasteRow :: ListStore Row -> Int -> Maybe String -> IO ()
pasteRow model index Nothing = return ()
pasteRow model index (Just s) = case readRow s of
  Just r  -> listStoreInsert model (index+1) r
  Nothing -> return ()
