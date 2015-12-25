module Homunculus.Generator where

import Homunculus.Parser

import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlphaNum)
import Data.IORef
import Data.List
import Data.List.Utils (replace)
import Data.Text (pack,unpack)
import Graphics.UI.Gtk hiding (Table)
import System.Directory
import System.FilePath ((</>))
import System.Random

{-
  The basic (functional) code
-}

data Generator = Generator { name, description :: String    -- Displayed to user
                           , tables :: [Table]              -- Meat & Potatoes
                           , options :: [Option]            -- User-set options
                           }
  deriving (Read,Show,Eq,Ord)

type Option = (String,[String])

data Table = Table { title :: String                        -- Displayed to user
                   , rows :: [Row]                          -- Actual Information
                   }
  deriving (Read,Show,Eq,Ord)

type Row = (Int,String)

{-
  I need one extra parser, which I can't include in Homunculus.Parser for reasons of
  dependency. This parser goes through the string and pulls out table references in the
  format "<TableName>" and then generates a result from THAT table, substituting it in.

  The idea is that this should be called before the other parsers, so that when the
  actual generate' function is called, all of that has already been sorted out already.
-}
tableRef :: [Table] -> [String] -> StdGen -> Parser String
tableRef ts opts g = do
  x <- (tableRef' ts opts g) <> getChar'
  xs <- many $ tableRef ts opts g'
  return $ concat $ x:xs
  where (_,g') = random g :: (Int,StdGen)

tableRef' :: [Table] -> [String] -> StdGen -> Parser String
tableRef' ts opts g = do
  _ <- char '<'
  xs <- Homunculus.Parser.until ">"
  _ <- char '>'
  if xs `elem` (map title ts)
  then return $ generate' (getTableFromName ts xs) ts opts g
  else return $ "ERROR: No table of name \""++xs++"\""

--Generate function decides what tables to use during the generation
generate :: Generator -> [String] -> StdGen -> String
generate g (ts:opts) gen = case ts of
  "All" -> concatMap (\t -> concat [title t,"\n     "
                                   ,(generate' (Just t) (tables g) opts gen),"\n"]
                      ) $ tables g
  x     -> generate' (getTableFromName (tables g) x) (tables g) opts gen

--Sub-function actually generates, based on a single Table rather than a whole Generator
generate' :: Maybe Table -> [Table] -> [String] -> StdGen -> String
generate' Nothing _ _ _ = "Error: No Table With That Name!"
generate' (Just t) ts' opts g = case parse (script opts g') full of
  [(x,[])]  -> replace "\\\\" "\n" x
  []        -> "Error in parse function!"
  where --This is the full string, with all table references already taken care of
        full = case parse (tableRef ts opts g) $ list !! (i `mod` length list) of
                  [(x,[])]  -> x
                  []        -> "Error in tableRef function!"
        list = concatMap (uncurry replicate) $ rows t
        --So after a table has been called, it can't be called again. No recursion.
        ts = filter (/=t) ts'
        (i,g') = random g :: (Int,StdGen)

readGenerator :: String -> Maybe Generator
readGenerator str = case reads str of
  [(x,"")]  -> Just x
  _         -> Nothing

--Function for taking output of readGenerator and forming [Generator]
maybeToList :: [Maybe a] -> [a]
maybeToList [] = []
maybeToList (x:xs) = case x of
  Nothing -> maybeToList xs
  Just a  -> a : maybeToList xs

getTableFromName :: [Table] -> String -> Maybe Table
getTableFromName [] _ = Nothing
getTableFromName (x:xs) n = if (title x)==n then Just x else getTableFromName xs n

getOptionFromName :: [Option] -> String -> Maybe (Option,Int)
getOptionFromName xs s = getOptionFromName' xs s 0

getOptionFromName' :: [Option] -> String -> Int -> Maybe (Option,Int)
getOptionFromName' [] _ _ = Nothing
getOptionFromName' (x:xs) s n = if (fst x)==s then Just (x,n) 
                                              else getOptionFromName' xs s (n+1)

testGen :: Generator
testGen = Generator { name = "Test"
                    , description = "This table should only be used for testing purposes."
                    , tables = [testTable]
                    , options = [("Option 1",["A","B","C"])]
                }
  where testTable = Table { title = "TestTable"
                          , rows = [(3,"Test (Weight 3)")
                                   ,(2,"Test (Weight 2)")
                                   ,(1,"Test (Weight 1)")
                                   ,(1,"There are 1d4+7 goblins")
                                   ,(1,"This should be 6: 2*3")
                                   ,(1,"It hails in a radius of d100*100ft")
                                   ,(1,"Option A is set? {A=True|Else=False}")
                                   ,(1,"Option B is set? {B=True|Else=False}")
                                   ,(1,"Option C is set? {C=True|Else=False}")
                                   ]
                          }

{-
  Dummy Generators will never actually be used as a generator, but serve as a way for
  users to send commands using the same interface as they would to select a generator
-}

dummyNewGen :: Generator
dummyNewGen = Generator { name = "New Generator..."
                        , description = "Dummy-Generator used to create a new one."
                        , tables = [], options = []
                        }

dummyMakeNewGen :: Generator
dummyMakeNewGen = Generator { name = "New Generator"
                            , description = concat ["To finish your new generator, edit "
                                                   ,"this one and hit 'Save'"]
                            , tables = [], options = []
                            }

dummySeparator :: Generator
dummySeparator = Generator { name = "--------------"  
                           , description = "", tables = [], options = [] 
                           }

{-
  IO code (mostly graphics-related)
-}

generateFile :: FilePath -> IO String
generateFile fp = do
  file <- readFile fp 
  g <- newStdGen
  return $ f (readGenerator file) g
  where f Nothing _   = "Error: Failed to read "++fp
        f (Just t) g  = generate t ["All"] g

--Clears out the box and creates the Generator UI within it
makeGenerator :: FilePath -> IO VBox
makeGenerator dataPath = do
  {-
    INITIALIZATION
  -}
  box <- vBoxNew False 5
  top <- hBoxNew False 2
  bottom <- frameNew
  {-
    CONSTRUCTION
  -}
  --set bottom  [ containerBorderWidth := 5 ]
  set box     [ containerChild := top, boxChildPacking top := PackNatural
              , containerChild := bottom
              , containerBorderWidth := 5
              ]

  {-
    LOGIC
  -}
  updateTop top dataPath bottom

  widgetShowAll box
  return box

updateTop :: HBox -> FilePath -> Frame -> IO ()
updateTop top dataPath bottom = do
  mapM_ widgetDestroy =<< containerGetChildren top
  {-
    INITIALIZATION
  -}
  list <- listStoreNew []
  combo <- comboBoxNewWithModel list
  ok <- buttonNewWithLabel "Open"
  edit <- buttonNewWithLabel "Edit"
  l <- labelNew $ Just "Choose A Generator: "
  {-
    CONSTRUCTION
  -}
  set top     [ containerChild := l, boxChildPacking l := PackNatural
              , containerChild := combo
              , containerChild := ok, boxChildPacking ok := PackNatural
              , containerChild := edit, boxChildPacking edit := PackNatural
              ]
  comboBoxSetRowSeparatorSource combo $ Just (list,\x -> x==dummySeparator)
  {-
    LOGIC
  -}
  --Load all generators from dataPath
  fileList <- getDirectoryContents $ dataPath </> "generators"
  files <- mapM readFile $ 
    map ((</>) (dataPath </> "generators")) $ filter (isInfixOf ".gen") fileList
  let tables = maybeToList $ map readGenerator files
  
  --Put all loaded generators into a ListStore, which is used by combo
  mapM_ (listStoreAppend list) (tables++[dummySeparator,dummyNewGen])

  --When the combo box is opened, the ListStore displays the names of the generators
  ren <- cellRendererTextNew
  cellLayoutPackEnd combo ren False
  cellLayoutSetAttributes combo ren list (\gen -> [ cellText := name gen ])

  on ok buttonActivated $ do
    gen <- listStoreGetValue list =<< comboBoxGetActive combo
    runGenerator gen bottom
  on edit buttonActivated $ do
    gen <- listStoreGetValue list =<< comboBoxGetActive combo
    editGenerator gen bottom (top,dataPath)
  on combo changed $ do
    gen <- listStoreGetValue list =<< comboBoxGetActive combo
    if gen==dummyNewGen 
    then do
      set ok    [ widgetSensitive := False ]
      set edit  [ widgetSensitive := False ]
      editGenerator dummyMakeNewGen bottom (top,dataPath) 
    else do
      set ok    [ widgetSensitive := True ]
      set edit  [ widgetSensitive := True ]

  if tables==[] 
  then do
    set edit  [ widgetSensitive := False ]
    set ok    [ widgetSensitive := False ]
    set combo [ widgetSensitive := False ]
  else comboBoxSetActive combo 0
  widgetShowAll top

runGenerator :: Generator -> Frame -> IO ()
runGenerator gen box' = do
  mapM_ widgetDestroy =<< containerGetChildren box'

  {-
    INITIALIZATION
  -}
  box <- hBoxNew True 2
  left' <- scrolledWindowNew Nothing Nothing
  left <- vBoxNew False 5
  right <- frameNew
  view <- textViewNew

  txt <- textViewGetBuffer view

  descLabel <- labelNew $ Just $ description gen
  optBox <- vBoxNew False 0
  genButton <- buttonNewWithLabel "Generate"

  tabRow   <- hBoxNew True 0
  tabLabel <- labelNew $ Just "Choose A Table: "
  tabCombo <- comboBoxNewText

  {-
    CONSTRUCTION
  -}
  mapM_ (comboBoxAppendText tabCombo) 
    $ if ((title $ head $ tables gen)=="Main") 
      then (map (pack . title) (tables gen))++[pack "All"]
      else (pack "All"):(map (pack . title) (tables gen))
  set tabRow [ containerChild := tabLabel
             , containerChild := tabCombo
             ]
  set optBox [ containerChild := tabRow ]
  opts <- mapM (\(x,y) -> do
    row <- hBoxNew True 0
    label <- labelNew $ Just $ x++": "
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo) (map pack y)

    set row [ containerChild := label
            , containerChild := combo
            ]
    set optBox  [ containerChild := row ]

    return combo
    ) $ options gen

  set descLabel [ labelWrap := True ]
  set left  [ containerBorderWidth := 10
            , containerChild := descLabel, boxChildPacking descLabel := PackNatural
            , containerChild := optBox, boxChildPacking optBox := PackNatural
            , containerChild := genButton, boxChildPacking genButton := PackNatural
                                         , boxChildPackType genButton := PackEnd
            ]
  set view  [ textViewEditable := False
            , textViewWrapMode := WrapWordChar
            , textViewLeftMargin := 3
            , textViewRightMargin := 3
            ]
  set right [ frameShadowType := ShadowIn
            , containerChild := view ]
  set left' [ scrolledWindowHscrollbarPolicy := PolicyNever
            , scrolledWindowVscrollbarPolicy := PolicyAutomatic
            , scrolledWindowShadowType := ShadowEtchedOut
            ]
  scrolledWindowAddWithViewport left' left
  set box   [ containerChild := left'
            , containerChild := right
            ]
  set box'  [ containerChild := box ]

  {-
    LOGIC
  -}
  comboBoxSetActive tabCombo 0
  on genButton buttonActivated $ do
    g <- newStdGen
    tab <- comboBoxGetActiveText tabCombo
    options <- mapM comboBoxGetActiveText opts
    set txt [ textBufferText := generate gen (getOpts (tab:options)) g ]
  widgetShowAll box
  where getOpts []            = []
        getOpts (Nothing:xs)  = getOpts xs
        getOpts ((Just x):xs) = (unpack x) : getOpts xs

editGenerator :: Generator -> Frame -> (HBox,FilePath) -> IO ()
editGenerator gen box' (top,dataPath) = do
  mapM_ widgetDestroy =<< containerGetChildren box'

  {-
    INITIALIZATION
  -}
  pane <- hBoxNew True 0
  box <- vBoxNew False 0
  bar <- toolbarNew
  pane <- hPanedNew
  left' <- scrolledWindowNew Nothing Nothing
  left <- vBoxNew False 0
  right' <- frameNew
  right <- notebookNew

  new <- toolButtonNewFromStock stockNew
  save <- toolButtonNewFromStock stockSave
  del <- toolButtonNewFromStock stockDelete
  exit <- toolButtonNewFromStock stockQuit

  nameText <- labelNew $ Just "Name: "
  nameBox <- entryNew
  nameRow <- hBoxNew True 0

  descText <- labelNew $ Just "Description: \n\n\n\n\n"
  descFra <- scrolledWindowNew Nothing Nothing
  descBox <- textViewNew
  descBuf <- textViewGetBuffer descBox
  descRow <- hBoxNew True 0

  optText <- labelNew $ Just "Options: "
  optBox <- vBoxNew False 0
  optFra <- frameNew
  optRow <- hBoxNew True 0

  os <- newIORef $ options gen
  ts <- newIORef [] :: IO (IORef [(Entry,ListStore Row)])

  tableRow <- hBoxNew True 0

  {-
    CONSTRUCTION
  -}
  makeOptionBox optBox os
  mapM_ (makePage right ts) (tables gen)

  set new     [ toolButtonLabel := Just "New Table" ]
  set bar     [ containerChild := new
              , containerChild := save
              , containerChild := exit
              , toolbarStyle := ToolbarBoth
              ]
  set nameBox [ entryText := name gen ]
  set nameRow [ containerChild := nameText
              , containerChild := nameBox
              ]
  set descFra [ containerChild := descBox
              , scrolledWindowShadowType := ShadowEtchedIn
              , scrolledWindowHscrollbarPolicy := PolicyAutomatic
              , scrolledWindowVscrollbarPolicy := PolicyAutomatic
              ]
  set descBox [ textViewWrapMode := WrapWordChar ]
  set descBuf [ textBufferText := description gen ]
  set descRow [ containerChild := descText
              , containerChild := descFra
              ]
  set optBox  [ containerBorderWidth := 2 ]
  set optFra  [ containerChild := optBox ]
  set optRow  [ containerChild := optText
              , containerChild := optFra
              ]
  set right   [ notebookTabPos := PosBottom
              , notebookScrollable := True
              , containerBorderWidth := 2
              ]
  set left    [ containerChild := nameRow, boxChildPacking nameRow := PackNatural
              , containerChild := descRow, boxChildPacking descRow := PackNatural
              , containerChild := optRow, boxChildPacking optRow := PackNatural
              , containerChild := tableRow, boxChildPacking tableRow := PackNatural
              , containerBorderWidth := 2
              ]
  set left'   [ scrolledWindowHscrollbarPolicy := PolicyNever
              , scrolledWindowVscrollbarPolicy := PolicyAutomatic
              , scrolledWindowShadowType := ShadowIn
              ]
  set right'  [ containerChild := right 
              , frameShadowType := ShadowIn
              ]
  set pane    [ containerChild := left'
              , containerChild := right'
              ]
  set box     [ containerChild := bar, boxChildPacking bar := PackNatural
              , containerChild := pane
              ]
  set box'    [ containerChild := box ]

  scrolledWindowAddWithViewport left' left

  {-
    LOGIC
  -}

  onToolButtonClicked new $ do
    makePage right ts $ Table { title = "New Table", rows = [] }
    notebookSetCurrentPage right (-1)
  onToolButtonClicked save $ saveGen gen nameBox descBuf os ts
  onToolButtonClicked exit $ do
    mapM_ widgetDestroy =<< containerGetChildren box'
    updateTop top dataPath box'
    widgetShowAll top
    widgetShowAll box'

  widgetShowAll box'

makeOptionBox :: VBox -> IORef [Option] -> IO ()
makeOptionBox v list = do
  mapM_ widgetDestroy =<< containerGetChildren v 
  {-
    INITIALIZATION
  -}
  optCom <- comboBoxNewText
  optOpt <- vBoxNew False 0
  os <- readIORef list
  {-
    CONSTRUCTION
  -}
  mapM_ (comboBoxAppendText optCom) $ map pack $ (map fst os)++["New Option..."]
  set v [ containerChild := optCom
        , containerChild := optOpt
        ]
  {-
    LOGIC
  -}
  on optCom changed $ do
    x <- comboBoxGetActiveText optCom
    if x==Nothing 
    then return ()
    else let (Just t) = x in case unpack t of
      "New Option..." -> do
                         modifyIORef list (\x -> x++[("New Option",[""])])
                         os' <- readIORef list
                         editOption v list (getOptionFromName os' "New Option")
      x               -> editOption v list (getOptionFromName os x)
  widgetShowAll v

editOption :: VBox -> IORef [Option] -> Maybe (Option,Int) -> IO ()
editOption v list Nothing = return ()
editOption v list (Just (o,i)) = do
  mapM_ widgetDestroy =<< containerGetChildren v
  {-
    INITIALIZATION
  -}
  row1 <- hBoxNew False 0
  label <- labelNew $ Just "Name: "
  entry <- entryNew

  row2 <- hBoxNew True 0
  txt <- labelNew $ Just "Number Of Rows: "
  spin <- spinButtonNewWithRange 1.0 100.0 1.0
  update <- buttonNewWithLabel "Update"

  row3 <- hBoxNew True 0
  button <- buttonNewWithLabel "Save Option"

  row4 <- hBoxNew True 0
  del <- buttonNewWithLabel "Delete Option"
  cancel <- buttonNewWithLabel "Cancel"

  box <- vBoxNew True 2

  os <- mapM (\x -> do
    e <- entryNew

    set e   [ entryText := x ]
    set box [ containerChild := e, boxChildPacking e := PackNatural ]

    return e
    ) $ snd o
  {-
    CONSTRUCTION
  -}
  set entry [ entryText := fst o ]
  set spin  [ spinButtonValue := toEnum $ length $ snd o 
            , spinButtonNumeric := True
            , spinButtonUpdatePolicy := UpdateIfValid
            ]
  set box   [ containerBorderWidth := 3 ]
  set row1  [ containerChild := label, boxChildPacking label := PackNatural
            , containerChild := entry
            ]
  set row2  [ containerChild := txt
            , containerChild := spin
            ]
  set row3  [ containerChild := button ]
  set row4  [ containerChild := del
            , containerChild := cancel
            ]
  set v     [ containerChild := row1, boxChildPacking row1 := PackNatural
            , containerChild := box, boxChildPacking box := PackNatural
            , containerChild := row2, boxChildPacking row2 := PackNatural
            , containerChild := update, boxChildPacking update := PackNatural
            , containerChild := row3, boxChildPacking row3 := PackNatural
            , containerChild := row4, boxChildPacking row4 := PackNatural
            ]
  {-
    LOGIC
  -}
  afterValueSpinned spin $ do 
    name <- entryGetText entry
    opts <- mapM entryGetText os

    n <- spinButtonGetValue spin

    editOption v list $ Just ((name,take (floor n) $ opts++(repeat "")),i)
  on button buttonActivated $ do
    name <- entryGetText entry
    opts <- mapM entryGetText os

    xs <- readIORef list
    writeIORef list $ (take i xs)++[(name,opts)]++(drop (i+1) xs)
    makeOptionBox v list
  on cancel buttonActivated $ makeOptionBox v list
  on del buttonActivated $ do
    modifyIORef list (delete o)
    makeOptionBox v list

  widgetShowAll v

saveGen :: Generator -> Entry -> TextBuffer -> IORef [Option]
        -> IORef ([(Entry,ListStore Row)]) -> IO ()
saveGen gen nameBox descBuf os ts = do
  name' <- entryGetText nameBox
  (i,i') <- textBufferGetBounds descBuf
  desc' <- textBufferGetText descBuf i i' True
  ts'' <- readIORef ts
  ts' <- mapM (\(e,m) -> do
    text <- entryGetText e
    list <- listStoreToList m
    return $ Table { title = text, rows = list } 
    ) ts''
  os' <- readIORef os
  let newGen = Generator  { name = name'
                          , description = desc'
                          , options = os' 
                          , tables = ts'
                          }
  dataPath <- getAppUserDataDirectory "homunculus"

  --Delete the file so that if the generator's name has changed, it doesn't
  --leave an older duplicate behind.
  bool <- doesFileExist (dataPath </> "generators" </> (toFileName $ name gen))
  if bool 
  then removeFile (dataPath </> "generators" </> (toFileName $ name gen))
  else return ()

  writeFile (dataPath </> "generators" </> (toFileName name')) (show newGen)
  where toFileName str = (filter isAlphaNum str)++".gen"

makePage :: Notebook -> IORef [(Entry,ListStore Row)] -> Table -> IO ()
makePage right ts x = do
  {-
    INITIALIZATION
  -}
  page <- vBoxNew False 0
  entry <- entryNew
  box <- hBoxNew False 0
  l <- labelNew $ Just "     " --This is just a hack
  bar <- toolbarNew

  new <- toolButtonNewFromStock stockNew
  del <- toolButtonNewFromStock stockDelete
  sep <- separatorToolItemNew
  lb <- toolButtonNewFromStock stockGotoFirst
  rb <- toolButtonNewFromStock stockGotoLast

  {-
    CONSTRUCTION
  -}
  set rb    [ widgetHasTooltip := True, widgetTooltipText := Just "Move Table Right" ]
  set lb    [ widgetHasTooltip := True, widgetTooltipText := Just "Move Table Left" ]
  set sep   [ separatorToolItemDraw := False
            , toolItemExpand := True
            ]
  set del   [ widgetHasTooltip := True, widgetTooltipText := Just "Delete Row" ]
  set new   [ widgetHasTooltip := True, widgetTooltipText := Just "New Row" ]
  set bar   [ containerChild := new
            , containerChild := del
            , containerChild := sep
            , containerChild := lb
            , containerChild := rb
            ]
  set page  [ containerChild := bar, boxChildPacking bar := PackNatural ]
  set entry [ entryText := title x ]
  set box   [ containerChild := entry
            , containerChild := l
            ]
  notebookAppendPage right page ""
  notebookSetTabLabel right page box

  {-
    LOGIC
  -}
  model <- showTable page x [new,del]

  onToolButtonClicked lb $ do
    --This is unsafe, but I don't think it's possible for users to make an error
    (Just i) <- notebookPageNum right page
    notebookReorderChild right page (i-1)
    modifyIORef ts (moveLeft i)
  onToolButtonClicked rb $ do
    --This is unsafe, but I don't think it's possible for users to make an error
    (Just i) <- notebookPageNum right page
    notebookReorderChild right page (i+1)
    modifyIORef ts (moveRight i)

  modifyIORef' ts (++[(entry,model)])
  widgetShowAll box
  where moveRight i xs = if i>=(length xs-1) 
                         then xs
                         else let (l,r:rs) = splitAt i xs in l++[head rs,r]++(tail rs)
        moveLeft 0 xs = xs
        moveLeft i xs = let (l,r:rs) = splitAt i xs in (init l)++[r,last l]++rs

showTable :: VBox -> Table -> [ToolButton] -> IO (ListStore Row)
showTable v t [new,del] = do
  {-
    INITIALIZATION
  -}
  model <- listStoreNew $ rows t
  view' <- scrolledWindowNew Nothing Nothing
  view <- treeViewNewWithModel model

  --TreeView has two columns, which will require renderers
  col1 <- treeViewColumnNew
  col2 <- treeViewColumnNew
  renderer1 <- cellRendererTextNew
  renderer2 <- cellRendererTextNew

  {-
    CONSTRUCTION
  -}
  set col1  [ treeViewColumnResizable := False
            , treeViewColumnTitle := "Weight"
            ]
  set col2  [ treeViewColumnResizable := False
            , treeViewColumnExpand := True
            , treeViewColumnTitle := "Result"
            , treeViewColumnSizing := TreeViewColumnAutosize
            ]
  set view  [ treeViewHeadersVisible := True
            , treeViewReorderable := True
            , treeViewRulesHint := True
            , treeViewEnableTreeLines := True
            ]
  set view' [ scrolledWindowHscrollbarPolicy := PolicyAutomatic
            , scrolledWindowVscrollbarPolicy := PolicyAutomatic
            , containerChild := view
            ]
  set v     [ containerChild := view' ]

  treeViewColumnPackStart col1 renderer1 False
  treeViewColumnPackStart col2 renderer2 True

  {-
    LOGIC
  -}
  cellLayoutSetAttributes col1 renderer1 model $
    \row -> [ cellText := show $ fst row, cellTextEditable := True ]
  cellLayoutSetAttributes col2 renderer2 model $
    \row -> [ cellText := snd row, cellTextEditable := True
            , cellTextEllipsize := EllipsizeEnd ]

  on renderer1 edited $ \[i] int -> do
    val <- listStoreGetValue model i
    listStoreSetValue model i (read $ filter (`elem` ['0'..'9']) int,snd val)
  on renderer2 edited $ \[i] str -> do
    val <- listStoreGetValue model i
    listStoreSetValue model i (fst val,str)

  view `on` buttonPressEvent $ tryEvent $ do
    RightButton <- eventButton
    liftIO $ do
      m <- menuNew
      new <- menuItemNewWithLabel "New"
      cut <- menuItemNewWithLabel "Cut"
      cop <- menuItemNewWithLabel "Copy"
      pas <- menuItemNewWithLabel "Paste"
      del <- menuItemNewWithLabel "Delete"
      
      mapM_ (menuShellAppend m) [new,cut,cop,pas,del]

      tree <- treeViewGetSelection view
      sel <- treeSelectionGetSelectedRows tree
      let [[index]] = if length sel>0 then sel else [[-1]]

      clipboard <- clipboardGet selectionClipboard

      on new menuItemActivate $ listStoreInsert model (index+1) (1,"")
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
      on pas menuItemActivate $ clipboardRequestText clipboard (pasteRow model index)
      on del menuItemActivate $ 
        if index==(-1) then return () else listStoreRemove model index
      menuPopup m Nothing
      widgetShowAll m
  onToolButtonClicked new $ do
    _ <- listStoreAppend model (1,"")
    return ()
  onToolButtonClicked del $ do
    tree <- treeViewGetSelection view
    sel <- treeSelectionGetSelectedRows tree
    if length sel>0
    then let [[index]] = sel in listStoreRemove model index else return ()

  mapM_ (treeViewAppendColumn view) [col1,col2]

  widgetShowAll v
  return model

readRow :: String -> Maybe Row
readRow str = case reads str of
  [(x,"")]  -> Just x
  _         -> Nothing

pasteRow :: ListStore Row -> Int -> Maybe String -> IO ()
pasteRow model index Nothing = return ()
pasteRow model index (Just s) = case readRow s of
  Just r  -> listStoreInsert model (index+1) r
  Nothing -> return ()
