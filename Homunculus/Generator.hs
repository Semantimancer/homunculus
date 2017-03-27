module Homunculus.Generator where

import Homunculus.Parser

import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlphaNum)
import Data.IORef
import Data.List
import Data.List.Utils (replace)
import Data.Text (pack,unpack, Text)
import Graphics.UI.Gtk hiding (Table)
import System.Directory
import System.FilePath ((</>))
import System.Random

{-
  The basic (functional) code
-}

data Generator = Generator { name, description :: String
                           , tables :: [Table]
                           , options :: [Option] }
  deriving (Read,Show,Eq,Ord)

type Option = (String,[String])

data Table = Table { title :: String
                   , rows :: [Row]
                   }
  deriving (Read,Show,Eq,Ord)

type Row = (Int,String)

{-
  I need one extra parser, which I can't include in 
  Homunculus.Parser for reasons of dependency. This parser goes 
  through the string and pulls out table references in the format 
  "<TableName>" and then generates a result from THAT table, 
  substituting it in.  

  The idea is that this should be called before the other parsers, 
  so that when the actual generate' function is called, all of that 
  has already been sorted out already.
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
  "All" -> concatMap 
            (\t -> concat [title t,"\n     "
                         ,(generate' (Just t) (tables g) opts gen)
                         ,"\n"]
            ) $ tables g
  x     -> generate' 
            (getTableFromName (tables g) x) (tables g) opts gen

--Sub-function actually generates
generate' :: Maybe Table -> [Table] -> [String] -> StdGen -> String
generate' Nothing _ _ _ = "Error: No Table With That Name!"
generate' (Just t) ts' opts g = case parse (script opts g') full of
  [(x,[])]  -> replace "\\\\" "\n" x
  x         -> "Error in parse function!\n\n\n"++(show x)
  where --This is the full string, with all table references 
        --already taken care of
        full = case parse (tableRef ts opts g) $ list 
                    !! (i `mod` length list) 
               of
                  [(x,[])]  -> x
                  []        -> "Error in tableRef function!"
        list = concatMap (uncurry replicate) $ rows t
        --After a table is called, it can't be called again.
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
                          , rows = []
                          }

{-
  Dummy Generators will never actually be used as a generator, but 
  serve as a way for users to send commands using the same 
  interface as they would to select a generator
-}

dummyNewGen :: Generator
dummyNewGen = Generator { name = "New Generator..."
                        , description = ""
                        , tables = [], options = []
                        }

dummyMakeNewGen :: Generator
dummyMakeNewGen = 
  Generator { name = "New Generator"
            , description = concat ["To finish your new "
                                   ,"generator, edit this one "
                                   ,"and hit 'Save'"]
            , tables = [], options = []
            }

dummySeparator :: Generator
dummySeparator = 
  Generator { name = "--------------"  
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
  set box     [ containerChild := top
              , boxChildPacking top := PackNatural
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
  then return ()
  else comboBoxSetActive combo 0
  widgetShowAll top

runGenerator :: Generator -> Frame -> IO ()
runGenerator gen frame' = do
  mapM_ widgetDestroy =<< containerGetChildren frame'

  {-
    INITIALIZATION
  -}
  frame <- vBoxNew False 0
  box <- hBoxNew True 2
  left' <- scrolledWindowNew Nothing Nothing
  left <- vBoxNew False 5
  right' <- vBoxNew False 0
  right <- frameNew
  view' <- scrolledWindowNew Nothing Nothing
  view <- vBoxNew False 3

  row <- hBoxNew False 2
  hide <- checkButtonNewWithLabel "Hide Options"
  genButton <- buttonNewWithLabel "Generate"
  clear <- buttonNewWithLabel "Clear"

  descLabel <- labelNew $ Just $ description gen
  optBox <- vBoxNew False 0

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
  set left    [ containerBorderWidth := 10
              , containerChild := descLabel, boxChildPacking descLabel := PackNatural
              , containerChild := optBox, boxChildPacking optBox := PackNatural
              ]
  set view    [ containerBorderWidth := 2 ]
  set view'   [ containerChild := view 
              , scrolledWindowHscrollbarPolicy := PolicyNever
              , scrolledWindowVscrollbarPolicy := PolicyAutomatic
              ]
  set row     [ containerChild := hide, boxChildPacking hide := PackNatural
              , containerChild := genButton, boxChildPacking genButton := PackRepel
              , containerChild := clear, boxChildPacking clear := PackNatural
                                       , boxChildPackType clear := PackEnd 
              ]
  set right'  [ containerChild := view', boxChildPacking view' := PackGrow
              , containerBorderWidth := 3
              ]
  set right   [ frameShadowType := ShadowIn, containerChild := right' ]
  set left'   [ scrolledWindowHscrollbarPolicy := PolicyNever
              , scrolledWindowVscrollbarPolicy := PolicyAutomatic
              , scrolledWindowShadowType := ShadowEtchedOut
              ]
  scrolledWindowAddWithViewport left' left
  set box     [ containerChild := left'
              , containerChild := right
              ]
  set frame   [ containerChild := box 
              , containerChild := row, boxChildPacking row := PackNatural
              ]
  set frame'  [ containerChild := frame ]

  {-
    LOGIC
  -}
  comboBoxSetActive tabCombo 0

  on hide toggled $ do
    bool <- toggleButtonGetActive hide
    if bool then widgetHide left' else widgetShow left'
  on genButton buttonActivated $ do
    g <- newStdGen
    tab <- comboBoxGetActiveText tabCombo
    os <- mapM comboBoxGetActiveText opts
    let opts = getOpts g $ (tab,[]):(zip os (map snd $ options gen))

    bar <- hSeparatorNew
    label <- labelNew $ Just $ generate gen opts g

    set label [ labelLineWrap := True 
              , labelSelectable := True
              , miscXalign := 0
              ]
    set view  [ containerChild := label, boxChildPacking label := PackNatural 
                                       , boxChildPosition label := 0
              , containerChild := bar, boxChildPacking bar := PackNatural
                                     , boxChildPosition bar := 0
              ]

    widgetShowAll view
  on clear buttonActivated $ mapM_ widgetDestroy =<< containerGetChildren view
  widgetShowAll frame

--After pulling in user-made choices, determine any other options randomly
getOpts :: StdGen -> [(Maybe Text,[String])] -> [String]
getOpts _ [] = []
getOpts g ((Nothing,os):xs) = (os!!(i `mod` length os)) : getOpts g' xs
  where (i,g') = random g :: (Int,StdGen)
getOpts g ((Just x,_):xs) = (unpack x) : getOpts g xs

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
  ts <- newIORef [] :: IO (IORef [(String,ListStore Row)])

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
    widgetShowAll right
  onToolButtonClicked save $ do
    updateTop top dataPath box'
    saveGen gen nameBox descBuf os ts
    widgetShowAll right
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
        -> IORef ([(String,ListStore Row)]) -> IO ()
saveGen gen nameBox descBuf os ts = do
  dataPath <- getAppUserDataDirectory "homunculus"

  newName <- entryGetText nameBox
  newDesc <- (\(x,y) -> textBufferGetText descBuf x y True) =<< textBufferGetBounds descBuf
  newTables <- mapM (\(text,model) -> 
    (\l -> return (Table text l)) =<< listStoreToList model
    ) =<< readIORef ts
  newOpts <- readIORef os

  --Delete the file so that if the generator's name has changed, it doesn't
  --leave an older duplicate behind.
  bool <- doesFileExist (dataPath </> "generators" </> (toFileName $ name gen))
  if bool 
  then removeFile (dataPath </> "generators" </> (toFileName $ name gen))
  else return ()

  writeFile (dataPath </> "generators" </> (toFileName newName)) 
            (show $ Generator newName newDesc newTables newOpts)
  where toFileName str = (filter isAlphaNum str)++".gen"

makePage :: Notebook -> IORef [(String,ListStore Row)] -> Table -> IO ()
makePage right ts x = do
  {-
    INITIALIZATION
  -}
  page <- vBoxNew False 0
  view <- vBoxNew False 0
  box <- hBoxNew False 0
  bar <- toolbarNew
  info <- hBoxNew False 0

  new <- toolButtonNewFromStock stockNew
  del <- toolButtonNewFromStock stockDelete
  sep <- separatorToolItemNew
  ren <- toolButtonNew (Nothing :: Maybe Image) (Just "Rename Table")
  delTable <- toolButtonNew (Nothing :: Maybe Image) (Just "Delete Table")

  weight <- labelNew $ Just ""

  {-
    CONSTRUCTION
  -}
  set sep   [ separatorToolItemDraw := False
            , toolItemExpand := True
            ]
  set del   [ widgetHasTooltip := True, widgetTooltipText := Just "Delete Row" ]
  set new   [ widgetHasTooltip := True, widgetTooltipText := Just "New Row" ]
  set bar   [ containerChild := new
            , containerChild := del
            , containerChild := sep
            , containerChild := delTable
            , containerChild := ren
            ]
  set info  [ containerChild := weight, boxChildPacking weight := PackNatural 
            , containerBorderWidth := 3
            ]
  set page  [ containerChild := bar, boxChildPacking bar := PackNatural 
            , containerChild := view
            , containerChild := info, boxChildPacking info := PackNatural
            ]
  notebookAppendPage right page $ title x
  notebookSetTabReorderable right page True

  {-
    LOGIC
  -}
  model <- showTable view x [new,del]

  let updateInfo = (do temp <- listStoreToList model
                       set weight [ labelLabel := "Total Weight: "++(show $ sum $ map fst temp) ])

  updateInfo

  on model rowChanged $ \_ _ -> updateInfo
  on model rowInserted $ \_ _ -> updateInfo
  on model rowDeleted $ \_ -> updateInfo

  onToolButtonClicked delTable $ do
    Just txt <- notebookGetTabLabelText right page
    i <- notebookGetCurrentPage right
    notebookRemovePage right i
    modifyIORef ts $ deleteBy (\(x,_) (y,_) -> x==y) (txt,model)
  onToolButtonClicked ren $ nameTable right page ts False
  on right pageReordered $ \_ _ -> do
    strs <- mapM (notebookGetTabLabelText right) =<< containerGetChildren right 
    oldRef <- readIORef ts
    writeIORef ts $ reorderList oldRef $
      foldl (\acc x -> if x==Nothing then acc else let Just y = x in acc++[y]) [] strs

  modifyIORef' ts (++[(title x,model)])
  widgetShowAll box

reorderList :: [(String,a)] -> [String] -> [(String,a)]
reorderList _ [] = []
reorderList xs (str:strs) = (get str xs) : reorderList xs strs
  where get elem list = head $ filter (\(s,a) -> s==elem) list --This is unsafe

nameTable :: Notebook -> VBox -> IORef [(String,ListStore Row)] -> Bool -> IO ()
nameTable right page ts bool = do
    Just currentTitle <- notebookGetTabLabelText right page :: IO (Maybe String)
    list <- readIORef ts

    dialog <- dialogNew
    upper' <- dialogGetContentArea dialog
    let upper = castToContainer upper'
    entry <- entryNew
    label <- labelNew $ Just $ "Table Name:"++(if bool then "\nInvalid name!" else "")

    set entry   [ entryText := currentTitle ]
    set upper   [ containerChild := label
                , containerChild := entry 
                ]
    set dialog  [ windowTitle := "Rename Table"
                , windowResizable := False
                , windowModal := True
                , windowWindowPosition := WinPosCenterAlways
                , windowDeletable := False
                , containerBorderWidth := 5
                ]

    dialogAddButton dialog stockOk ResponseOk
    dialogAddButton dialog stockCancel ResponseCancel
    widgetShowAll upper

    response <- dialogRun dialog
    if response==ResponseOk
    then do
      txt <- entryGetText entry
      if txt `elem` (map fst list)
      then widgetDestroy dialog >> nameTable right page ts True
      else do
        i <- notebookGetCurrentPage right
        writeIORef ts $ (take i list)++[(txt,snd $ list!!i)]++(drop (i+1) list)
        notebookSetTabLabelText right page txt
        widgetShowAll right
        widgetDestroy dialog
    else widgetDestroy dialog

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
      sep <- separatorMenuItemNew
      edt <- menuItemNewWithLabel "Edit Row..."
      mny <- menuItemNewWithLabel "Insert Many..."
      
      mapM_ (menuShellAppend m) [new,cut,cop,pas,del]
      menuShellAppend m sep
      mapM_ (menuShellAppend m) [edt,mny]

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
      on edt menuItemActivate $ 
        if index==(-1)
        then return ()
        else editWindow model index
      on mny menuItemActivate $ insertMany model index
        --if index==(-1)
        --then return ()
        --else insertMany model index
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

editWindow :: ListStore Row -> Int -> IO ()
editWindow model index = do
  dialog <- dialogNew
  upper' <- dialogGetContentArea dialog
  let upper = castToBox upper'
  scroll <- scrolledWindowNew Nothing Nothing
  view <- textViewNew
  buff <- textViewGetBuffer view

  (weight,oldText) <- listStoreGetValue model index

  set view    [ textViewWrapMode := WrapWordChar ]
  set buff    [ textBufferText := replace "\\\\" "\n" oldText ]
  set scroll  [ containerChild := view
              , widgetWidthRequest := 400
              , widgetHeightRequest := 300
              , scrolledWindowShadowType := ShadowEtchedOut
              ]
  set upper   [ containerChild := scroll 
              , boxSpacing := 6
              ]
  set dialog  [ windowTitle := "Edit Row"
              , windowModal := True
              , windowWindowPosition := WinPosCenter
              , windowDeletable := True
              , containerBorderWidth := 5
              ]

  dialogAddButton dialog stockOk ResponseOk
  dialogAddButton dialog stockCancel ResponseCancel
  widgetShowAll upper

  response <- dialogRun dialog
  if response==ResponseOk
  then do
    newText <- (\(i,i') -> textBufferGetText buff i i' True) =<< textBufferGetBounds buff
    listStoreSetValue model index (weight,replace "\n" "\\\\" newText)
    widgetDestroy dialog
  else widgetDestroy dialog

insertMany :: ListStore Row -> Int -> IO ()
insertMany model index = do
  dialog <- dialogNew
  upper' <- dialogGetContentArea dialog
  --let upper = castToContainer upper'
  let upper = castToBox upper'
  scroll <- scrolledWindowNew Nothing Nothing
  view <- textViewNew
  buff <- textViewGetBuffer view

  set scroll  [ containerChild := view
              , widgetWidthRequest := 600
              , widgetHeightRequest := 400
              , scrolledWindowShadowType := ShadowEtchedOut
              ]
  set upper   [ containerChild := scroll 
              , boxSpacing := 6
              ]
  set dialog  [ windowTitle := "Insert Multiple Rows"
              , windowModal := True
              , windowWindowPosition := WinPosCenter
              , windowDeletable := True
              , containerBorderWidth := 5
              ]
  
  dialogAddButton dialog stockOk ResponseOk
  dialogAddButton dialog stockCancel ResponseCancel
  widgetShowAll upper

  response <- dialogRun dialog
  if response==ResponseOk
  then do
    (i,i') <- textBufferGetBounds buff
    txt <- textBufferGetText buff i i' True
    mapM_ (listStoreInsert model (index+1)) $ 
      foldl (\acc x -> if x=="" then acc else (1,trim x):acc) [] (lines txt)
    widgetDestroy dialog
  else widgetDestroy dialog
  where trim [] = []
        trim (' ':xs) = trim xs
        trim xs = xs

readRow :: String -> Maybe Row
readRow str = case reads str of
  [(x,"")]  -> Just x
  _         -> Nothing

pasteRow :: ListStore Row -> Int -> Maybe String -> IO ()
pasteRow model index Nothing = return ()
pasteRow model index (Just s) = case readRow s of
  Just r  -> listStoreInsert model (index+1) r
  Nothing -> return ()
