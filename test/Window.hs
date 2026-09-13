{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the window, driven from code and without one.
--
-- The window is a function of one value now, so this suite hands events to
-- 'update' and looks at the value that comes back -- the same events the
-- widgets post, the same update the application runs, the real kernel over a
-- real pipe, and the real folder on disk.  What it leaves out is the drawing,
-- which is what the scripts under tests/ cover.
--
-- It needs no display.  Anything the update asks GTK for -- a toast, a
-- dialog -- is queued on a main loop that is not running here, so it never
-- happens and never gets in the way.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, isJust)
import Data.Word (Word32)
import System.Directory
import System.Environment (setEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

import GI.Gtk.Declarative.App.Simple (Transition (..))

import Cellar.App.Env
import Cellar.App.Event
import Cellar.App.State
import Cellar.App.Update (update)
import Cellar.Client
import Cellar.Config
import Cellar.Grid.Model
import Cellar.Ref
import Cellar.Sexp (asInt, lookupKey)
import Cellar.Store
import Cellar.View

main :: IO ()
main = do
  root <- makeTemporaryDirectory
  -- A home, a data directory and a preferences file of its own: this suite
  -- writes recent workbooks and scratch workbooks, and neither belongs in the
  -- real ones.
  createDirectoryIfMissing True (root </> "home")
  setEnv "HOME" (root </> "home")
  setEnv "XDG_DATA_HOME" (root </> "data")
  setEnv "CELLAR_CONFIG" (root </> "config.scm")
  setEnv "GUILE_AUTO_COMPILE" "0"

  failures <- newIORef (0 :: Int)
  found <- findKernel
  case found of
    Nothing -> putStrLn "  SKIP the kernel was not found (set CELLAR_KERNEL)"
    Just (program, arguments) -> do
      kernel <- startKernel program arguments
      posted <- newIORef []
      env <- quietEnv kernel (\event -> modifyIORef' posted (++ [event]))
      config <- loadConfig
      state <- newIORef (newState config (root </> "home"))
      let window = Window env kernel posted state failures
      tests window root
      stopKernel kernel

  removeDirectoryRecursive root
  count <- readIORef failures
  putStrLn ""
  if count == 0
    then putStrLn "ALL TESTS PASSED" >> exitSuccess
    else putStrLn (show count ++ " FAILURE(S)") >> exitFailure

-- | Everything a test needs: the window's state, and the way in.
data Window = Window
  { windowEnv :: Env
  , windowKernel :: Kernel
  , windowPosted :: IORef [Event]
  , windowState :: IORef State
  , windowFailures :: IORef Int
  }

tests :: Window -> FilePath -> IO ()
tests window root = do
  let sales = root </> "sales.cellar"
      budget = root </> "budget.cellar"
  createWorkbook sales "Summary"
  createWorkbook budget "Sheet 1"

  section "the start page"
  start <- stateOf window
  check window "the window opens on the start page" StartPage (statePage start)
  check window "with no workbook" False (isJust (stateWorkbook start))
  check window "and nothing to show in the title" "No workbook open" (subtitleOf start)

  section "opening a workbook"
  happens window (Act (OpenRecentAt sales))
  opened <- stateOf window
  check window "the sheet page is what is showing now" SheetPage (statePage opened)
  check window "the title names the workbook" "sales.cellar" (subtitleOf opened)
  check window "and its sheet has a tab" ["Summary"] (tabOrder opened)
  check window "which is the one selected" (Just "Summary")
    (tabName <$> currentTab opened)

  section "the recent workbooks"
  listed <- stateRecent <$> stateOf window
  check window "opening a workbook remembers it" [sales] listed
  happens window (Act (OpenRecentAt budget))
  both <- stateRecent <$> stateOf window
  check window "the one opened last is at the top" [budget, sales] both
  saved <- recentWorkbooks <$> loadConfig
  check window "and the list is on disk as well as on screen" [budget, sales] saved
  happens window (Act ClearRecent)
  cleared <- stateRecent <$> stateOf window
  check window "clearing empties the list" [] cleared

  section "sheets"
  happens window (Act (OpenRecentAt sales))
  happens window (SheetNamed Nothing "Q1")
  added <- stateOf window
  check window "adding a sheet adds a tab" ["Summary", "Q1"] (tabOrder added)
  folder <- doesDirectoryExist (sales </> "sheets" </> "Q1")
  check window "and a folder for it" True folder

  forM_ (tabNamed "Q1" added) $ \tab ->
    happens window (SheetNamed (Just (tabId tab)) "Quarter One")
  renamed <- stateOf window
  check window "renaming a sheet renames its tab" ["Summary", "Quarter One"]
    (tabOrder renamed)
  moved <- doesDirectoryExist (sales </> "sheets" </> "Quarter One")
  check window "and its folder" True moved

  forM_ (tabNamed "Quarter One" renamed) $ \tab ->
    happens window (SheetDeleted (tabId tab) "Quarter One")
  deleted <- stateOf window
  check window "deleting a sheet takes the tab with it" ["Summary"] (tabOrder deleted)
  gone <- doesDirectoryExist (sales </> "sheets" </> "Quarter One")
  check window "and the folder goes too" False gone

  section "a cell, through the window and back"
  sheet <- stateOf window
  forM_ (tabNamed "Summary" sheet) $ \tab -> do
    happens window (CellEdited (tabId tab) (Ref 0 0) "(* 6 7)")
    settled <- settle window $ \s -> case tabById (tabId tab) s of
      Nothing -> False
      Just found -> displayAt (modelView (tabGrid found)) (Ref 0 0) == "42"
    check window "the grid shows what the kernel made of the cell" True settled
    after <- stateOf window
    forM_ (tabById (tabId tab) after) $ \found -> do
      let view = modelView (tabGrid found)
      check window "which is a number, and marked as one" True
        (numberAt view (Ref 0 0))
      written <- readFile (cellFilePath (sales </> "sheets" </> "Summary") "A1")
      check window "and the source is in the cell's own file" "(* 6 7)\n" written

  section "a cell through the grid"
  atWork <- stateOf window
  forM_ (currentTab atWork) $ \tab -> do
    -- Delete on a cell is the grid asking for it to be cleared, which is the
    -- same round trip a cell being written is.
    happens window (GridSaid (tabId tab) (Pressed (Ref 0 0) 1))
    happens window (GridSaid (tabId tab) (KeyDown keyDelete))
    emptied <- settle window $ \s -> case tabById (tabId tab) s of
      Nothing -> False
      Just found -> null (tabSources found)
    check window "Delete clears the cell" True emptied
    file <- doesFileExist (cellFilePath (sales </> "sheets" </> "Summary") "A1")
    check window "and takes its file with it" False file

  section "moving and inserting"
  rows <- stateOf window
  forM_ (currentTab rows) $ \tab -> do
    happens window (CellEdited (tabId tab) (Ref 1 0) "\"second\"")
    wrote <- settle window (holds (tabId tab) (Ref 1 0) "second")
    check window "a cell is written where it was asked for" True wrote
    -- What moves is the row the active cell is on, so the cell just written
    -- is the one to be standing on.
    happens window (GridSaid (tabId tab) (Pressed (Ref 1 0) 1))
    happens window (Act (MoveLine Row (-1)))
    moved <- settle window (holds (tabId tab) (Ref 0 0) "second")
    check window "a row moves, and its cell goes with it" True moved
    happens window (Act (InsertLine Row True))
    taller <- settle window $ \s -> case tabById (tabId tab) s of
      Nothing -> False
      Just found -> viewRows (modelView (tabGrid found)) > 100
    check window "inserting a row makes the sheet taller" True taller
    pushed <- settle window (holds (tabId tab) (Ref 1 0) "second")
    check window "and pushes the cell below it down" True pushed
    happens window (Act RecalculateSheet)
    again <- settle window (holds (tabId tab) (Ref 1 0) "second")
    check window "recalculating leaves the sheet saying the same thing" True again

  section "the tabs"
  happens window (SheetNamed Nothing "Q2")
  two <- stateOf window
  forM_ (tabNamed "Summary" two) $ \tab -> happens window (TabSelected (tabId tab))
  selected <- stateOf window
  check window "selecting a tab shows that sheet" (Just "Summary")
    (tabName <$> currentTab selected)
  saved <- workbookActiveSheet =<< resolved sales
  check window "and the workbook remembers which" (Just "Summary") saved
  happens window (TabsReordered (reverse (map tabId (stateTabs selected))))
  reordered <- stateOf window
  check window "dragging a tab reorders the sheets" ["Q2", "Summary"]
    (tabOrder reordered)
  order <- workbookSheetNames =<< resolved sales
  check window "and the folder is told" ["Q2", "Summary"] order

  section "what the window refuses"
  forM_ (tabNamed "Q2" reordered) $ \tab ->
    happens window (SheetDeleted (tabId tab) "Q2")
  lonely <- stateOf window
  forM_ (currentTab lonely) $ \tab -> happens window (TabCloseAsked (tabId tab))
  kept <- stateOf window
  check window "the last sheet of a workbook cannot be closed" 1
    (length (stateTabs kept))
  check window "and the tab view is told to keep it" (Just False)
    (snd <$> stateCloseAnswer kept)
  happens window (Act (OpenRecentAt (root </> "nowhere")))
  refused <- stateOf window
  check window "a folder that is not a workbook is not opened" (Just "sales.cellar")
    (workbookName <$> stateWorkbook refused)

  section "the folder changing underneath"
  onDisk <- stateOf window
  forM_ (currentTab onDisk) $ \tab -> do
    saveCell (sales </> "sheets" </> "Summary") "C3" (Just "\"outside\"")
    happens window DiskChanged
    noticed <- settle window (holds (tabId tab) (Ref 2 2) "outside")
    check window "a cell written from outside is taken in" True noticed

  section "the kernel"
  happens window Tick
  settled <- stateOf window
  check window "with nothing outstanding, nothing is being waited for" False
    (stateAskingAboutKernel settled)
  happens window (KernelRefused Ignored "no such thing")
  check window "a refusal for a request nobody holds changes nothing" True True

  section "the sheets in order"
  happens window (SheetNamed Nothing "Q3")
  stepping <- stateOf window
  forM_ (tabNamed "Summary" stepping) $ \tab -> happens window (TabSelected (tabId tab))
  happens window (Act NextSheet)
  forward <- stateOf window
  check window "the next sheet is the one after this" (Just "Q3")
    (tabName <$> currentTab forward)
  happens window (Act NextSheet)
  atEnd <- stateOf window
  check window "and the last one stays the last one" (Just "Q3")
    (tabName <$> currentTab atEnd)
  happens window (Act PreviousSheet)
  back <- stateOf window
  check window "the one before it is the one before" (Just "Summary")
    (tabName <$> currentTab back)
  forM_ (tabNamed "Q3" back) $ \tab -> happens window (SheetDeleted (tabId tab) "Q3")

  section "a column, and a cell somewhere else"
  columns <- stateOf window
  forM_ (currentTab columns) $ \tab -> do
    happens window (CellEdited (tabId tab) (Ref 0 1) "\"beside\"")
    _ <- settle window (holds (tabId tab) (Ref 0 1) "beside")
    happens window (GridSaid (tabId tab) (Pressed (Ref 0 1) 1))
    happens window (Act (MoveLine Column (-1)))
    shifted <- settle window (holds (tabId tab) (Ref 0 0) "beside")
    check window "a column moves, and its cell goes with it" True shifted
    happens window (Act (InsertLine Column False))
    wider <- settle window $ \s -> case tabById (tabId tab) s of
      Nothing -> False
      Just found -> viewColumns (modelView (tabGrid found)) > 26
    check window "inserting a column makes the sheet wider" True wider
    -- An empty cell has no file until somebody asks to open one, which is
    -- what brings it into being.
    happens window (GridSaid (tabId tab) (Pressed (Ref 5 5) 1))
    happens window (Act OpenCellElsewhere)
    made <- doesFileExist (cellFilePath (sales </> "sheets" </> "Summary") "F6")
    check window "opening a cell elsewhere gives it a file to open" True made

  section "the drag, as the state sees it"
  dragging <- stateOf window
  forM_ (currentTab dragging) $ \tab -> do
    happens window (DragBegun (tabId tab) Row 2)
    started <- stateOf window
    check window "a drag says which line it has hold of" (Just (Row, 2, 2))
      (modelDrag . tabGrid =<< tabById (tabId tab) started)
    happens window (DragMoved (tabId tab) 5)
    over <- stateOf window
    check window "and where it is over" (Just (Row, 2, 5))
      (modelDrag . tabGrid =<< tabById (tabId tab) over)
    happens window (DragCancelled (tabId tab))
    given <- stateOf window
    check window "a drag given up leaves nothing behind" Nothing
      (modelDrag . tabGrid =<< tabById (tabId tab) given)
    happens window (DragBegun (tabId tab) Row 0)
    happens window (DragDropped (tabId tab) 2)
    dropped <- stateOf window
    check window "and one that lands is not still going" Nothing
      (modelDrag . tabGrid =<< tabById (tabId tab) dropped)
    happens window (LineChosen (tabId tab) Column 3)
    picked <- stateOf window
    check window "right-clicking a heading picks that column" (Just 3)
      (refColumn . modelActive . tabGrid <$> tabById (tabId tab) picked)

  section "the things that open a window of their own"
  -- None of these can be answered here: what they put on screen is queued on
  -- a main loop that is not running.  What is checked is that asking for one
  -- leaves the workbook alone.
  before <- stateOf window
  forM_ [ Act NewWorkbook, Act CopyTo, Act OpenWorkbook, Act AddSheet
        , Act RenameSheet, Act EditCell, Act Preferences, Act Shortcuts
        , Act About, Act SaveNothing ] (happens window)
  untouched <- stateOf window
  check window "asking for a dialog changes nothing by itself"
    (tabOrder before, statePage before) (tabOrder untouched, statePage untouched)

  section "a workbook Cellar makes"
  happens window (WorkbookMade (root </> "made.cellar") False False)
  -- Settling on the workbook being open is not enough: its sheets are written
  -- out when the kernel answers, so this waits for the file to say so.
  made <- settle window $ \s ->
    (workbookName <$> stateWorkbook s) == Just "made.cellar"
  check window "a new workbook is made and opened" True made
  _ <- settleOn window $ do
    written <- readSheetMetadata (root </> "made.cellar" </> "sheets" </> "Sheet 1")
    pure ((lookupKey "rows" written >>= asInt) == Just 100)
  sized <- readSheetMetadata (root </> "made.cellar" </> "sheets" </> "Sheet 1")
  check window "and its sheet is written out at the size on screen"
    (Just 100) (lookupKey "rows" sized >>= asInt)
  happens window (Act NewScratch)
  scratch <- settle window (\s -> stateScratch s && isJust (stateWorkbook s))
  check window "a scratch workbook opens, and says it is one" True scratch
  offered <- stateRecent <$> stateOf window
  check window "and is not offered as a recent workbook" False
    (any (isInfixOf "scratch") offered)

  section "the preferences"
  happens window (EditorCommandSet "hx")
  written <- externalEditorCommand <$> loadConfig
  check window "the editor command is saved as it is typed" "hx" written

  section "the selection"
  selected <- stateOf window
  forM_ (currentTab selected) $ \tab -> do
    happens window (GridSaid (tabId tab) (Pressed (Ref 2 1) 1))
    clicked <- stateOf window
    check window "a click moves the active cell" (Just (Ref 2 1))
      (modelActive . tabGrid <$> tabById (tabId tab) clicked)
    check window "and the cell bar says what is in it" Nothing
      (sourceOf (Ref 2 1) clicked)

-- | Whether a cell of a sheet says this.
holds :: TabId -> Ref -> String -> State -> Bool
holds tab r said state = case tabById tab state of
  Nothing -> False
  Just found -> displayAt (modelView (tabGrid found)) r == said

-- | The workbook in a folder, which the tests made a moment ago.
resolved :: FilePath -> IO Workbook
resolved path = do
  found <- resolveWorkbook path
  maybe (fail (path ++ " is not a workbook")) pure found

-- | The keyval for Delete, without pulling GDK into a suite that draws
-- nothing.  GDK_KEY_Delete is 0xffff, and has been since X11.
keyDelete :: Word32
keyDelete = 0xffff

--
-- Driving the window
--

-- | Hand the window an event, and everything that came of it.
--
-- This is the application's own loop with the drawing left out: the update
-- says what the state becomes and what to do, the doing posts more events, and
-- this goes round until there is nothing left to answer.
happens :: Window -> Event -> IO ()
happens window event = do
  state <- readIORef (windowState window)
  case update (windowEnv window) state event of
    Exit -> pure ()
    Transition next action -> do
      writeIORef (windowState window) next
      answer <- action
      forM_ answer (happens window)
      drain window
  drain window

-- | Deal with whatever the kernel has said and whatever the last turn posted.
drain :: Window -> IO ()
drain window = do
  replies <- takeReplies (windowKernel window)
  queued <- atomicModifyIORef' (windowPosted window) (\events -> ([], events))
  forM_ replies $ \reply -> case reply of
    Answered requestId payload -> do
      tag <- tagOf (windowEnv window) requestId
      happens window (KernelSaid tag payload)
    Refused requestId why -> do
      tag <- tagOf (windowEnv window) requestId
      happens window (KernelRefused tag why)
  forM_ queued (happens window)

-- | Turn the loop over until something out in the world says so.
settleOn :: Window -> IO Bool -> IO Bool
settleOn window wanted = go (200 :: Int)
  where
    go 0 = pure False
    go tries = do
      ready <- wanted
      if ready then pure True else do
        threadDelay 20000
        drain window
        go (tries - 1)

-- | Turn the loop over until the state says what it is waiting for.
settle :: Window -> (State -> Bool) -> IO Bool
settle window wanted = go (200 :: Int)
  where
    go 0 = pure False
    go tries = do
      state <- readIORef (windowState window)
      if wanted state then pure True else do
        threadDelay 20000
        drain window
        go (tries - 1)

stateOf :: Window -> IO State
stateOf = readIORef . windowState

-- | An environment with nothing behind the parts that draw.
--
-- The update only ever uses these inside the actions it hands back, and the
-- actions that use them are the ones that put something on screen.  Nothing
-- here asks for a window, so nothing here needs one.
quietEnv :: Kernel -> (Event -> IO ()) -> IO Env
quietEnv kernel poster = do
  windowRef <- newIORef Nothing
  toastsRef <- newIORef Nothing
  gestures <- newIORef M.empty
  watcher <- newIORef Nothing
  previews <- newIORef M.empty
  tags <- newIORef M.empty
  stall <- newIORef Nothing
  pure Env
    { envKernel = kernel
    , envPost = poster
    , envUiDirectory = "ui"
    , envBuilder = error "the tests draw nothing, so there is no .ui file"
    , envWindow = windowRef
    , envToasts = toastsRef
    , envGestures = gestures
    , envWatcher = watcher
    , envTags = tags
    , envPreviews = previews
    , envRecentSection = error "the tests draw nothing, so there is no menu"
    , envStallDialog = stall
    }

--
-- Saying what happened
--

section :: String -> IO ()
section name = putStrLn ("\n-- " ++ name)

check :: (Eq a, Show a) => Window -> String -> a -> a -> IO ()
check window what wanted got
  | wanted == got = putStrLn ("  ok   " ++ what)
  | otherwise = do
      modifyIORef' (windowFailures window) (+ 1)
      putStrLn ("  FAIL " ++ what ++ ": expected " ++ show wanted
                ++ " got " ++ show got)

makeTemporaryDirectory :: IO FilePath
makeTemporaryDirectory = do
  base <- getTemporaryDirectory
  let root = base </> "cellar-window-test"
  exists <- doesDirectoryExist root
  when' exists (removeDirectoryRecursive root)
  createDirectoryIfMissing True root
  pure root
  where when' condition action = if condition then action else pure ()

-- | Where the kernel is.  @CELLAR_KERNEL@ names it outright; otherwise it is
-- looked for beside the tests, which is where it lives in the source tree.
findKernel :: IO (Maybe (FilePath, [String]))
findKernel = do
  here <- getCurrentDirectory
  let script = here </> "bin" </> "cellar-kernel.scm"
  exists <- doesFileExist script
  pure $ if exists
    then Just ("guile", ["-L", here </> "src", "-s", script])
    else Nothing
