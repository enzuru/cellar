{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The window's state, and the small things everything asks of it.
--
-- The records the rest of the shell is written against, plus the handful of
-- operations that are about the window as a whole: what is on screen, what the
-- title says, and how a failure gets in front of somebody.
--
module Cellar.App.Types where

import Control.Exception (SomeException, fromException, try)
import Control.Monad (filterM, forM, forM_)
import Data.IORef
import Data.Text (Text)
import Data.Word (Word32)
import qualified Data.Text as T

import Data.GI.Base
import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import Cellar.Client
import Cellar.Config
import Cellar.Grid
import Cellar.Ref
import Cellar.Store
import Cellar.Watch


applicationId :: Text
applicationId = "dev.enzuru.Cellar"

-- | How much empty room a new sheet gets.  A question about what looks right
-- in a window, which is why it is settled here and not in the kernel.

defaultRows, defaultColumns :: Int
defaultRows = 100

defaultColumns = 26

-- | What the first sheet of a new workbook is called until it is renamed.

firstSheetName :: String
firstSheetName = "Sheet 1"

-- | How often to hand out whatever the kernel has said.  The reading is done
-- by a thread of its own, so this is a swap of a reference and a few calls;
-- once a frame is plenty.

pumpMilliseconds :: Word32
pumpMilliseconds = 16

-- | How long a cell may take before Cellar assumes something has gone wrong
-- and offers to stop it.  Long enough that an honestly slow expression is not
-- interrupted, short enough that a cell that will never finish does not look
-- like the application hanging -- which, before the kernel was its own
-- process, is exactly what it was.

patienceSeconds :: Double
patienceSeconds = 10

-- | A sheet of the workbook and everything that shows it.
--
-- A tab is found by the title of its page, never by the page object.  Sheet
-- names are unique within a workbook -- the store refuses a duplicate -- so the
-- title is a key, and using it means nothing here has to reason about whether
-- two handles on the same GObject are the same value.

data Tab = Tab
  { tabName :: IORef String
    -- | What the kernel calls this sheet: a number, handed out once and never
    -- reused, because the kernel must not care that a tab can be renamed.
  , tabId :: Int
  , tabPage :: Adw.TabPage
  , tabGrid :: Grid
    -- | The cell sources.  The shell owns these -- it is the side that reads
    -- and writes the files -- and the kernel owns what they come to.
  , tabSources :: IORef [(String, String)]
  }


data App = App
  { appWindow :: Adw.ApplicationWindow
  , appBuilder :: Gtk.Builder
  , appUiDirectory :: FilePath
  , appToasts :: Adw.ToastOverlay
  , appTabView :: Adw.TabView
  , appTabBar :: Adw.TabBar
  , appStack :: Gtk.Stack
  , appCellBar :: Gtk.Box
  , appRecalculate :: Gtk.Button
  , appWindowTitle :: Adw.WindowTitle
  , appReferenceLabel :: Gtk.Label
  , appSourceLabel :: Gtk.Label
  , appLineMenu :: Maybe Gio.MenuModel
  , appKernel :: Kernel
  , appConfig :: IORef Config
    -- | The workbook being edited, or nothing when none is open.
  , appWorkbook :: IORef (Maybe FilePath)
  , appScratch :: IORef Bool
  , appWatcher :: IORef (Maybe Watcher)
  , appWatching :: IORef [FilePath]
  , appTabs :: IORef [Tab]
  , appNextId :: IORef Int
    -- | True while tabs are being built or torn down, which is when the tab
    -- view fires the same signals a person switching tabs would.  Nothing is
    -- written to disk during that.
  , appLoading :: IORef Bool
  , appLocation :: IORef FilePath
  , appCopying :: IORef Bool
  , appRenaming :: IORef (Maybe Tab)
  , appPendingDelete :: IORef (Maybe (Tab, Adw.TabPage))
    -- | Set once the kernel has answered anything at all.  Until then it is
    -- still starting, and the first real request would otherwise be timed as
    -- though a cell had gone wrong.
  , appKernelAnswered :: IORef Bool
  , appWaitingOnPurpose :: IORef Bool
  , appAskingAboutKernel :: IORef Bool
  , appStallDialog :: IORef (Maybe Adw.AlertDialog)
  }

-- Startup


currentTab :: App -> IO (Maybe Tab)
currentTab app = do
  page <- Adw.tabViewGetSelectedPage (appTabView app)
  case page of
    Nothing -> pure Nothing
    Just p -> tabForPage app p


tabForPage :: App -> Adw.TabPage -> IO (Maybe Tab)
tabForPage app page = do
  title <- Adw.tabPageGetTitle page
  tabNamed app (T.unpack title)


tabNamed :: App -> String -> IO (Maybe Tab)
tabNamed app name = do
  tabs <- readIORef (appTabs app)
  matches <- filterM (\tab -> (== name) <$> readIORef (tabName tab)) tabs
  pure (case matches of { (tab : _) -> Just tab; [] -> Nothing })

-- | The sheet names in the order the tab bar shows them.

tabOrder :: App -> IO [String]
tabOrder app = do
  count <- Adw.tabViewGetNPages (appTabView app)
  forM [0 .. count - 1] $ \position -> do
    page <- Adw.tabViewGetNthPage (appTabView app) position
    T.unpack <$> Adw.tabPageGetTitle page


orderedTabs :: App -> IO [Tab]
orderedTabs app = do
  names <- tabOrder app
  found <- mapM (tabNamed app) names
  pure [ tab | Just tab <- found ]


tabDirectory :: App -> Tab -> IO (Maybe FilePath)
tabDirectory app tab = do
  workbook <- readIORef (appWorkbook app)
  case workbook of
    Nothing -> pure Nothing
    Just path -> do
      name <- readIORef (tabName tab)
      Just <$> workbookSheetDirectory path name


selectTab :: App -> String -> IO ()
selectTab app name = do
  found <- tabNamed app name
  forM_ found $ \tab -> Adw.tabViewSetSelectedPage (appTabView app) (tabPage tab)

-- | Put a tab for a sheet on screen, reading it off the disk.
--
-- The tab is on screen before the kernel has said a word about it: it starts
-- with an empty view of the ordinary size and fills in when the snapshot
-- arrives.  Everything here works that way round, because the alternative is a
-- window that waits on another process before it will draw.

reportFailure :: App -> String -> IO () -> IO ()
reportFailure app what action = do
  outcome <- try action :: IO (Either SomeException ())
  case outcome of
    Right () -> pure ()
    Left thrown -> notify app (T.pack (explain thrown))
  where
    explain thrown = case fromException thrown of
      Just (StoreError why) -> why
      Nothing -> "Could not " ++ what

-- | The grid asking for something it cannot do itself.


axisName :: Axis -> String
axisName Row = "row"
axisName Column = "column"

-- Editing


sameSet :: [String] -> [String] -> Bool
sameSet a b = all (`elem` b) a && all (`elem` a) b

-- | Bring one tab back in line with its folder.  Answers whether the folder
-- had in fact changed.


sortByName :: [(String, String)] -> [(String, String)]
sortByName = foldr insert []
  where
    insert x [] = [x]
    insert x (y : ys) | fst x <= fst y = x : y : ys
                      | otherwise = y : insert x ys

-- Pages and titles


notify :: App -> Text -> IO ()
notify app message = do
  toast <- new Adw.Toast [#title := message]
  Adw.toastOverlayAddToast (appToasts app) toast


sheetShowing :: App -> IO Bool
sheetShowing app = do
  name <- Gtk.stackGetVisibleChildName (appStack app)
  pure (name == Just "sheet")


retitle :: App -> IO ()
retitle app = do
  scratch <- readIORef (appScratch app)
  workbook <- readIORef (appWorkbook app)
  Adw.windowTitleSetSubtitle (appWindowTitle app) $ case (scratch, workbook) of
    (True, _) -> "Scratch"
    (_, Just path) -> T.pack (workbookName path)
    _ -> "No workbook open"


showStartPage :: App -> IO ()
showStartPage app = do
  Gtk.stackSetVisibleChildName (appStack app) "start"
  -- The cell bar, the tab bar and the recalculate button all speak about a
  -- workbook; with none open there is nothing for them to say.
  Gtk.widgetSetVisible (appCellBar app) False
  Gtk.widgetSetVisible (appTabBar app) False
  Gtk.widgetSetVisible (appRecalculate app) False
  retitle app


showSheetPage :: App -> IO ()
showSheetPage app = do
  Gtk.stackSetVisibleChildName (appStack app) "sheet"
  Gtk.widgetSetVisible (appCellBar app) True
  Gtk.widgetSetVisible (appTabBar app) True
  Gtk.widgetSetVisible (appRecalculate app) True
  retitle app
  tab <- currentTab app
  forM_ tab $ \t -> do
    gridActiveRef (tabGrid t) >>= showSelection app
    gridFocus (tabGrid t)


showSelection :: App -> Ref -> IO ()
showSelection app r = do
  Gtk.labelSetLabel (appReferenceLabel app) (T.pack (refName r))
  tab <- currentTab app
  source <- case tab of
    Nothing -> pure Nothing
    Just t -> lookup (refName r) <$> readIORef (tabSources t)
  case source of
    Just text -> do
      Gtk.labelSetLabel (appSourceLabel app) (T.pack (oneLine text))
      Gtk.widgetRemoveCssClass (appSourceLabel app) "dim-label"
    Nothing -> do
      Gtk.labelSetLabel (appSourceLabel app)
        "empty — double-click a cell to write Guile"
      Gtk.widgetAddCssClass (appSourceLabel app) "dim-label"

-- | Collapse text onto a single line for the cell bar.

oneLine :: String -> String
oneLine = unwords . words

-- Opening and making workbooks


object :: (GObject o, TypedObject o) => Gtk.Builder -> Text -> (ManagedPtr o -> o) -> IO o
object builder name constructor = do
  found <- Gtk.builderGetObject builder name
  case found of
    Nothing -> error ("cellar: the UI file has no " ++ T.unpack name)
    Just value -> unsafeCastTo constructor value


optionalObject
  :: (GObject o, TypedObject o)
  => Gtk.Builder -> Text -> (ManagedPtr o -> o) -> IO (Maybe o)
optionalObject builder name constructor = do
  found <- Gtk.builderGetObject builder name
  case found of
    Nothing -> pure Nothing
    Just value -> Just <$> unsafeCastTo constructor value
