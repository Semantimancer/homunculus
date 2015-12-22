module Homunculus.Initiative where

import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import Graphics.UI.Gtk
import System.Random

data Row = Row { name :: String
               , initiative :: Int
               , notes :: String
               }
  deriving (Eq)

instance Ord Row where
  a<=b = (initiative a)>=(initiative b)

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
  set vbox    [ containerChild := toolbar, boxChildPacking toolbar := PackNatural
              , containerChild := tree
              ]
  {-
    LOGIC
  -}
  onToolButtonClicked new $ do
    i <- randomRIO (1,20)
    _ <- listStoreAppend model $ Row { name = "", initiative = i, notes = "" }
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

  renderer1 <- cellRendererTextNew
  renderer2 <- cellRendererTextNew
  renderer3 <- cellRendererTextNew

  {-
    CONSTRUCTION
  -}
  set col1  [ treeViewColumnResizable := True
            , treeViewColumnFixedWidth := 100
            , treeViewColumnTitle := "Name"
            ]
  set col2  [ treeViewColumnResizable := False
            , treeViewColumnTitle := "Initiative"
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

  treeViewColumnPackStart col1 renderer1 False
  treeViewColumnPackStart col2 renderer2 True
  treeViewColumnPackStart col3 renderer3 True

  mapM_ (treeViewAppendColumn view) [col0,col2,col1,col3]

  {-
    LOGIC
  -}
  --This describes what each cell actually displays
  cellLayoutSetAttributes col1 renderer1 model $
    \row -> [ cellText := name row, cellTextEditable := True ]
  cellLayoutSetAttributes col2 renderer2 model $
    \row -> [ cellText := show $ initiative row, cellTextEditable := True ]
  cellLayoutSetAttributes col3 renderer3 model $
    \row -> [ cellText := notes row, cellTextEditable := True ]

  --React to user input
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
      cop <- menuItemNewWithLabel "Copy"
      pas <- menuItemNewWithLabel "Paste"
      del <- menuItemNewWithLabel "Delete"
      srt <- menuItemNewWithLabel "Sort"

      mapM_ (menuShellAppend m) [new,cop,pas,del,srt]

      tree <- treeViewGetSelection view
      sel <- treeSelectionGetSelectedRows tree
      let [[index]] = if (length sel>0) then sel else [[-1]]

      on new menuItemActivate $ do
        i <- randomRIO (1,20)
        _ <- listStoreAppend model $ Row { name = "", initiative = i, notes = "" }
        return ()
      on del menuItemActivate $ 
        if index==(-1) then return () else listStoreRemove model index
      on srt menuItemActivate $ do
        list <- listStoreToList model
        listStoreClear model
        mapM_ (listStoreAppend model) (sort list)

      menuPopup m Nothing
      widgetShowAll m

  widgetShowAll view'
  return view'
