{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the window, driven from code.
--
-- This is the same window `cellar` puts on screen: the real .ui file, the real
-- widgets, the real kernel behind it.  What it leaves out is the person.  Every
-- check here calls the function a signal handler would have called and then
-- asks the widgets what they now say, which is the half of the smoke scripts
-- that does not need a mouse -- and the half that fails there for reasons that
-- have nothing to do with Cellar, since a click at the wrong coordinate and a
-- broken program look the same in a screenshot.
--
-- The scripts under tests/ still earn their keep: nothing here can tell you
-- that Ctrl+T reaches the action, that a click lands on the row it looks like
-- it lands on, or that the window drew at all.  This is the other layer.
--
-- It needs a display, which is why it is `make check-window` and not part of
-- `make check`.  Two things are deliberately not touched: the modal dialogs,
-- which answer through a callback nobody is here to click, and the pointer
-- gestures in the grid, which need events this cannot synthesise.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (unless, void, when)
import Data.IORef
import Data.Int (Int32)
import Data.Text (Text)
import System.Directory
import System.Environment (setEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

import Data.GI.Base
import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import Cellar.App (withWindow)
import Cellar.App.Kernel
import Cellar.App.Recent
import Cellar.App.Types
import Cellar.App.Workbook
import Cellar.Config
import Cellar.Grid
import Cellar.Ref
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
  -- The same pair the smoke scripts set: a nested X server has no GL worth
  -- speaking of.
  setEnv "GDK_BACKEND" "x11"
  setEnv "GSK_RENDERER" "cairo"
  setEnv "GUILE_AUTO_COMPILE" "0"

  failures <- newIORef (0 :: Int)
  withWindow $ \app -> do
    outcome <- try (tests failures root app) :: IO (Either SomeException ())
    case outcome of
      Right () -> pure ()
      Left thrown -> do
        modifyIORef' failures (+ 1)
        putStrLn ("  FAIL the suite threw: " ++ show thrown)

  removeDirectoryRecursive root
  count <- readIORef failures
  putStrLn ""
  if count == 0
    then putStrLn "ALL TESTS PASSED" >> exitSuccess
    else putStrLn (show count ++ " FAILURE(S)") >> exitFailure

tests :: IORef Int -> FilePath -> App -> IO ()
tests failures root app = do
  section "the start page"
  page <- Gtk.stackGetVisibleChildName (appStack app)
  showing <- sheetShowing app
  cellBar <- Gtk.widgetGetVisible (appCellBar app)
  tabBar <- Gtk.widgetGetVisible (appTabBar app)
  subtitle <- Adw.windowTitleGetSubtitle (appWindowTitle app)
  listed <- recentTitles app
  check failures "the window opens on the start page" (Just "start") page
  check failures "with no sheet showing" False showing
  check failures "the cell bar is put away" False cellBar
  check failures "and the tab bar with it" False tabBar
  check failures "the title says nothing is open" "No workbook open" subtitle
  check failures "and nothing has been opened before" [] listed

  section "opening a workbook"
  let budget = root </> "books" </> "budget.cellar"
  createWorkbook budget "Summary"
  made <- resolveWorkbook budget
  book <- maybe (fail "the workbook that was just made is not one") pure made
  _ <- addWorkbookSheet book "Q1"
  opened <- openWorkbook app budget
  order <- tabOrder app
  onSheet <- sheetShowing app
  named <- Adw.windowTitleGetSubtitle (appWindowTitle app)
  barNow <- Gtk.widgetGetVisible (appTabBar app)
  check failures "a workbook on disk opens" True opened
  check failures "with a tab for each sheet it has" ["Summary", "Q1"] order
  check failures "the sheet page is what is showing now" True onSheet
  check failures "the title names the workbook" "budget.cellar" named
  check failures "and the tab bar is out" True barNow

  section "the recent workbooks"
  first <- recentTitles app
  boxShown <- Gtk.widgetGetVisible (appRecentBox app)
  items <- Gio.menuModelGetNItems (appRecentSection app)
  check failures "opening a workbook remembers it" ["budget.cellar"] first
  check failures "the list is on the start page" True boxShown
  check failures "and a submenu is in the menu" (1 :: Int32) items
  let sales = root </> "books" </> "sales.cellar"
  createWorkbook sales "Summary"
  _ <- openWorkbook app sales
  second <- recentTitles app
  saved <- recentWorkbooks <$> loadConfig
  check failures "the one opened last is at the top"
    ["sales.cellar", "budget.cellar"] second
  check failures "and the list is on disk as well as on screen" 2 (length saved)
  -- A workbook that has gone is dropped rather than offered and refused.
  removeDirectoryRecursive budget
  refreshRecent app
  pruned <- recentTitles app
  check failures "a workbook that went is dropped" ["sales.cellar"] pruned
  clearRecent app
  emptied <- recentTitles app
  boxGone <- Gtk.widgetGetVisible (appRecentBox app)
  itemsGone <- Gio.menuModelGetNItems (appRecentSection app)
  cleared <- recentWorkbooks <$> loadConfig
  check failures "clearing empties the list" [] emptied
  check failures "takes it off the start page" False boxGone
  check failures "takes the submenu out of the menu" (0 :: Int32) itemsGone
  check failures "and empties the file" [] cleared

  section "sheets"
  addSheet app "Q2"
  added <- tabOrder app
  folder <- doesDirectoryExist (sales </> "sheets" </> "Q2")
  check failures "adding a sheet adds a tab" ["Summary", "Q2"] added
  check failures "and a folder for it" True folder
  found <- tabNamed app "Q2"
  tab <- maybe (fail "the tab that was just added is not there") pure found
  renameSheet app tab "Later"
  renamed <- tabOrder app
  movedTo <- doesDirectoryExist (sales </> "sheets" </> "Later")
  movedFrom <- doesDirectoryExist (sales </> "sheets" </> "Q2")
  check failures "renaming a sheet renames its tab" ["Summary", "Later"] renamed
  check failures "and its folder" (True, False) (movedTo, movedFrom)
  -- Deleting is what the confirmation dialog does once somebody has answered
  -- it.  The answering itself stays with the smoke scripts: these bindings
  -- have no adw_alert_dialog_response, so a modal put up from here can only be
  -- clicked, not called.  What follows the answer is all here.
  deleted <- deleteSheet app tab
  goneFromTabs <- tabNamed app "Later"
  goneFromDisk <- doesDirectoryExist (sales </> "sheets" </> "Later")
  check failures "deleting a sheet says it deleted it" True deleted
  check failures "the shell stops holding the tab" True (null goneFromTabs)
  check failures "and the folder goes with it" False goneFromDisk
  -- Taking the page off the bar is the dialog's last step.  A page whose tab
  -- the shell has already forgotten closes without being asked about again,
  -- which is the branch this goes through.
  Adw.tabViewClosePage (appTabView app) (tabPage tab)
  closed <- settle 100 $ do
    left <- tabOrder app
    pure (left == ["Summary"])
  check failures "and the tab bar is left with the sheet that remains" True closed

  section "a cell, through the window and back"
  summary <- tabNamed app "Summary"
  sheet <- maybe (fail "the first sheet is not there") pure summary
  setCell app sheet (Ref 0 0) "(* 6 7)"
  -- The kernel answers on its own thread and the answer is handed out by a
  -- timer, so the loop has to be turned over until it arrives.
  arrived <- settle 200 $ do
    view <- gridCurrentView (tabGrid sheet)
    pure (displayAt view (Ref 0 0) == "42")
  view <- gridCurrentView (tabGrid sheet)
  let cells = workbookSheetDirectory (asWorkbook sales) "Summary"
  written <- readFile (cellFilePath cells "A1")
  check failures "the grid shows what the kernel made of the cell" True arrived
  check failures "which is a number, and marked as one" True (numberAt view (Ref 0 0))
  check failures "and the source is in the cell's own file" "(* 6 7)\n" written

  section "a scratch workbook"
  scratchWorkbook app
  scratch <- readIORef (appScratch app)
  scratchTitle <- Adw.windowTitleGetSubtitle (appWindowTitle app)
  scratchSheets <- tabOrder app
  stillEmpty <- recentTitles app
  check failures "a scratch workbook opens" True scratch
  check failures "and says so in the title" "Scratch" scratchTitle
  check failures "with one sheet to write in" [firstSheetName] scratchSheets
  -- It is made by a keypress and thrown away as often as it is kept, so it
  -- stays out of the list until Copy To makes it a workbook somewhere.
  check failures "and it is not offered as a recent workbook" [] stillEmpty

  section "back to the start page"
  showStartPage app
  ended <- Gtk.stackGetVisibleChildName (appStack app)
  endedBar <- Gtk.widgetGetVisible (appCellBar app)
  check failures "the start page comes back" (Just "start") ended
  check failures "and the cell bar goes away again" False endedBar

-- | The workbook a folder holds, for the checks that only want a path out of
-- it.  Everything here made its own workbooks a moment ago, so a folder that
-- is not one is the test being wrong rather than Cellar.
asWorkbook :: FilePath -> Workbook
asWorkbook path = Workbook path SheetsUnder

-- | The titles on the start page's list of recent workbooks, top first.
recentTitles :: App -> IO [Text]
recentTitles app = go 0
  where
    go index = do
      row <- Gtk.listBoxGetRowAtIndex (appRecentList app) index
      case row of
        Nothing -> pure []
        Just this -> do
          action <- castTo Adw.ActionRow this
          title <- maybe (pure "") (\r -> get r #title) action
          (title :) <$> go (index + 1)

-- | Turn the main loop over until something is true, or give up.
--
-- The window is built and driven from inside the activate handler, so the loop
-- is not spinning while these checks run.  Anything that waits on the kernel
-- has to hand the loop the time to deliver the answer, which is what this does.
settle :: Int -> IO Bool -> IO Bool
settle 0 _ = pure False
settle tries condition = do
  met <- condition
  if met then pure True else do
    turn 1
    settle (tries - 1) condition

-- | Turn the loop over a few times with nothing in particular to wait for,
-- which is what putting a dialog on screen needs.
turn :: Int -> IO ()
turn 0 = pure ()
turn rounds = do
  context <- GLib.mainContextDefault
  let drain = do
        pending <- GLib.mainContextPending (Just context)
        when pending (void (GLib.mainContextIteration (Just context) False) >> drain)
  drain
  threadDelay 20000
  turn (rounds - 1)

check :: (Eq a, Show a) => IORef Int -> String -> a -> a -> IO ()
check failures label expected actual
  | expected == actual = putStrLn ("  ok   " ++ label)
  | otherwise = do
      modifyIORef' failures (+ 1)
      putStrLn ("  FAIL " ++ label ++ ": expected " ++ show expected
                ++ " got " ++ show actual)

section :: String -> IO ()
section title = putStrLn ("-- " ++ title)

makeTemporaryDirectory :: IO FilePath
makeTemporaryDirectory = do
  base <- getTemporaryDirectory
  let path = base </> "cellar-window-test"
  exists <- doesDirectoryExist path
  unless (not exists) (removeDirectoryRecursive path)
  createDirectoryIfMissing True path
  pure path
