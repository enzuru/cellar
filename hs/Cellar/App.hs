{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The application: what starts, what it is called, and what the keys do.
--
module Cellar.App (runApp, withWindow) where

import Control.Exception (finally)
import Control.Monad (forM_, unless, void, when)
import Data.IORef
import Data.Maybe (fromMaybe)
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import Data.GI.Base
import qualified Data.Text as T
import qualified GI.Adw as Adw
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified GI.GtkSource as Source

import Cellar.Client
import Cellar.Config
import Cellar.Grid
import Cellar.Ref
import Cellar.Store
import Cellar.App.Types
import Cellar.App.Kernel
import Cellar.App.Workbook
import Cellar.App.Recent
import Cellar.App.Dialogs


runApp :: [String] -> IO ()
runApp arguments = do
  app <- new Adw.Application [#applicationId := applicationId]
  _ <- on app #activate (activate app (listToMaybe' arguments))
  void (Gio.applicationRun app Nothing)
  where listToMaybe' xs = case xs of { (x : _) -> Just x; [] -> Nothing }


activate :: Adw.Application -> Maybe FilePath -> IO ()
activate application file = do
  app <- buildWindow application
  -- A workbook named on the command line opens straight away; without one the
  -- start page asks what to open, since a sheet has to live somewhere.
  forM_ file $ \path -> do
    opened <- openWorkbook app path
    unless opened (showStartPage app)

-- | Build the window and everything behind it, and put it on screen.
--
-- Split out of 'activate' so that it has two callers: the application, which
-- presents the window and lets somebody use it, and the window tests, which
-- drive the same window from code.  A test that built a window of its own
-- would be testing its own construction rather than Cellar's.
buildWindow :: Adw.Application -> IO App
buildWindow application = do
  -- GtkSourceView registers its types here; without this the builder cannot
  -- instantiate the source view in editor.ui.
  Source.init
  uiDirectory <- findUiDirectory
  builder <- Gtk.builderNewFromFile (uiDirectory </> "cellar.ui")

  window <- object builder "main_window" Adw.ApplicationWindow
  toasts <- object builder "toast_overlay" Adw.ToastOverlay
  tabView <- object builder "tab_view" Adw.TabView
  tabBar <- object builder "tab_bar" Adw.TabBar
  stack <- object builder "main_stack" Gtk.Stack
  cellBar <- object builder "cell_bar" Gtk.Box
  recalculate <- object builder "recalculate_button" Gtk.Button
  windowTitle <- object builder "window_title" Adw.WindowTitle
  referenceLabel <- object builder "reference_label" Gtk.Label
  sourceLabel <- object builder "source_label" Gtk.Label
  editButton <- object builder "edit_button" Gtk.Button
  lineMenu <- optionalObject builder "line_menu" Gio.MenuModel
  recentList <- object builder "recent_list" Gtk.ListBox
  recentBox <- object builder "recent_box" Gtk.Box
  -- Empty, and filled in from the preferences once the window is built.
  recentSection <- Gio.menuNew

  (program, arguments) <- kernelCommandLine
  kernel <- startKernel program arguments
  config <- loadConfig >>= newIORef
  home <- fromMaybe "." <$> lookupEnv "HOME"

  -- Named at construction rather than positional: a record this wide, built
  -- out of a row of `newIORef Nothing`, is one inserted field away from being
  -- silently wrong.
  app <- App window builder uiDirectory toasts tabView tabBar stack cellBar
             recalculate windowTitle referenceLabel sourceLabel lineMenu
             recentList recentBox recentSection kernel config
    <$> newIORef Nothing        -- appWorkbook
    <*> newIORef False          -- appScratch
    <*> newIORef Nothing        -- appWatcher
    <*> newIORef []             -- appWatching
    <*> newIORef []             -- appTabs
    <*> newIORef 0              -- appNextId
    <*> newIORef False          -- appLoading
    <*> newIORef home           -- appLocation
    <*> newIORef False          -- appKernelAnswered
    <*> newIORef False          -- appWaitingOnPurpose
    <*> newIORef False          -- appAskingAboutKernel
    <*> newIORef Nothing        -- appStallDialog

  installCss uiDirectory
  installIcons
  Gtk.windowSetIconName window (Just applicationId)
  warmUp app
  installPump app
  installActions app application
  wireDialogs app editButton
  installRecent app
  showStartPage app

  Gtk.applicationAddWindow application window
  Gtk.windowPresent window
  pure app

-- | Build the window, hand it to something, and take it down again.
--
-- This is 'runApp' with the person replaced by a program: the same window,
-- built out of the same .ui file with the same kernel behind it, driven from
-- code instead of from a mouse.  The application is deliberately not unique,
-- so that a test does not hand its work to the Cellar you already have open.
--
-- The body runs inside the activate handler, which is to say inside the main
-- loop but not while it is spinning; anything waiting on an answer from the
-- kernel has to turn the loop over itself.
withWindow :: (App -> IO ()) -> IO ()
withWindow body = do
  application <- new Adw.Application
    [ #applicationId := applicationId
    , #flags := [Gio.ApplicationFlagsNonUnique] ]
  _ <- on application #activate $ do
    app <- buildWindow application
    body app `finally` do
      stopKernel (appKernel app)
      Gio.applicationQuit application
  void (Gio.applicationRun application (Just ["cellar-window-test"]))

-- Talking to the kernel

-- | Ask the kernel for something and carry on.  A refusal says so in a toast,
-- which is the whole of the shell's error handling because the kernel's
-- failures are all things a person can read.


installActions :: App -> Adw.Application -> IO ()
installActions app application = do
  let define name accelerators action = do
        simple <- Gio.simpleActionNew name Nothing
        _ <- on simple #activate (\_ -> action)
        Gio.actionMapAddAction application simple
        unless (null accelerators) $
          Gtk.applicationSetAccelsForAction application ("app." <> name) accelerators
      -- An action that means nothing with no workbook on screen, and does
      -- nothing there.
      onSheet action = do
        showing <- sheetShowing app
        when showing $ currentTab app >>= mapM_ action

  define "new" ["<Control>n"] (askForNewWorkbook app "workbook" False)
  define "new-scratch" ["<Control><Shift>n"] (scratchWorkbook app)
  define "open" ["<Control>o"] (chooseFolder app (void . openWorkbook app))
  -- There is nothing to save: the workbook on disk is already this one.
  -- Ctrl+S is too deep a reflex to leave doing nothing silently.
  define "save" ["<Control>s"] $ onSheet $ \_ ->
    notify app "Cellar saves each cell as you edit it"
  define "copy-to" ["<Control><Shift>s"] $ onSheet $ \_ -> do
    workbook <- readIORef (appWorkbook app)
    scratch <- readIORef (appScratch app)
    let suggestion = case (workbook, scratch) of
          (Just open, False) -> workbookName open
          _ -> "workbook"
    askForNewWorkbook app suggestion True
  define "add-sheet" ["<Control>t"] (onSheet (const (askForSheetName app Nothing)))
  define "rename-sheet" ["<Control><Shift>r"] (onSheet (askForSheetName app . Just))
  define "delete-sheet" ["<Control>w"] $ onSheet $ \tab ->
    Adw.tabViewClosePage (appTabView app) (tabPage tab)
  define "next-sheet" ["<Control>Page_Down"] (onSheet (const (stepSheet app 1)))
  define "previous-sheet" ["<Control>Page_Up"] (onSheet (const (stepSheet app (-1))))
  define "recalculate" ["<Control>r"] $ onSheet $ \tab ->
    askSheet app tab "recalculate" [] $ \payload -> do
      takeSnapshot app tab payload
      notify app "Recalculated"
  define "clear-cell" ["Delete"] $ onSheet $ \tab -> do
    r <- gridActiveRef (tabGrid tab)
    setCell app tab r ""
  define "edit-cell" ["<Control>e"] $ onSheet $ \tab ->
    gridActiveRef (tabGrid tab) >>= editCell app tab
  -- The same letter, shifted: Ctrl+E edits the cell here, Ctrl+Shift+E edits
  -- it elsewhere.  GNOME has no settled chord for handing a file to another
  -- program, but this window already reads Ctrl+Shift as "the other one of
  -- these" -- new workbook, rename sheet, copy to.
  define "open-cell" ["<Control><Shift>e"] $ onSheet $ \tab ->
    gridActiveRef (tabGrid tab) >>= openCellExternally app tab

  let moveLine axis delta = onSheet $ \tab -> do
        moved <- gridMoveLine (tabGrid tab) axis delta
        unless moved $ notify app $ case axis of
          Row -> "The row is already at the edge of the sheet"
          Column -> "The column is already at the edge of the sheet"
  define "move-row-up" ["<Control><Shift>Up"] (moveLine Row (-1))
  define "move-row-down" ["<Control><Shift>Down"] (moveLine Row 1)
  define "move-column-left" ["<Control><Shift>Left"] (moveLine Column (-1))
  define "move-column-right" ["<Control><Shift>Right"] (moveLine Column 1)

  let insertLine axis before = onSheet $ \tab -> do
        inserted <- gridInsertLine (tabGrid tab) axis before
        when inserted $ notify app $ case axis of
          Row -> "Row inserted"
          Column -> "Column inserted"
  define "insert-row-before" ["<Control><Alt>Up"] (insertLine Row True)
  define "insert-row-after" ["<Control><Alt>Down"] (insertLine Row False)
  define "insert-column-before" ["<Control><Alt>Left"] (insertLine Column True)
  define "insert-column-after" ["<Control><Alt>Right"] (insertLine Column False)

  -- A row on the start page and an item in the Open Recent submenu both fire
  -- this, with the workbook's folder as the target -- which is why it is
  -- defined by hand: it is the one action of Cellar's that takes one.
  openRecent <- Gio.simpleActionNew "open-recent" . Just =<< GLib.variantTypeNew "s"
  _ <- on openRecent #activate $ \parameter -> forM_ parameter $ \variant -> do
         wanted <- fromGVariant variant
         forM_ wanted $ \path -> do
           opened <- openWorkbook app (T.unpack path)
           -- A folder that will not open is one the list should stop offering.
           unless opened (refreshRecent app)
  Gio.actionMapAddAction application openRecent
  define "clear-recent" [] (clearRecent app)

  define "preferences" ["<Control>comma"] (openPreferences app)
  define "shortcuts" ["<Control>question"] (showShortcuts app)
  define "about" [] (showAbout app)
  define "quit" ["<Control>q"] $ do
    stopKernel (appKernel app)
    Gtk.windowClose (appWindow app)


-- | The grid's own styling, which lives beside the .ui files because it is the
-- same kind of thing: a description of how the window looks, kept out of the
-- program that decides what it does.

installCss :: FilePath -> IO ()
installCss uiDirectory = do
  provider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromPath provider (uiDirectory </> "cellar.css")
  display <- Gdk.displayGetDefault
  forM_ display $ \d -> Gtk.styleContextAddProviderForDisplay d provider 600


installIcons :: IO ()
installIcons = do
  directory <- findIconDirectory
  forM_ directory $ \path -> do
    display <- Gdk.displayGetDefault
    forM_ display $ \d -> do
      theme <- Gtk.iconThemeGetForDisplay d
      Gtk.iconThemeAddSearchPath theme path

-- | Locate the .ui files and the stylesheet, whether running from the source
-- tree or installed.

findUiDirectory :: IO FilePath
findUiDirectory = do
  override <- lookupEnv "CELLAR_UI_DIR"
  case override of
    Just path | not (null path) -> pure path
    _ -> do
      here <- getCurrentDirectory
      found <- firstThatHas [here </> "ui"] "cellar.ui"
      case found of
        Just path -> pure path
        Nothing -> error "cellar: cannot find the ui directory; set CELLAR_UI_DIR"


findIconDirectory :: IO (Maybe FilePath)
findIconDirectory = do
  override <- lookupEnv "CELLAR_ICON_DIR"
  case override of
    Just path | not (null path) -> pure (Just path)
    _ -> do
      here <- getCurrentDirectory
      let candidate = here </> "data" </> "icons"
      exists <- doesDirectoryExist candidate
      pure (if exists then Just candidate else Nothing)


firstThatHas :: [FilePath] -> FilePath -> IO (Maybe FilePath)
firstThatHas [] _ = pure Nothing
firstThatHas (directory : more) file = do
  exists <- doesFileExist (directory </> file)
  if exists then pure (Just directory) else firstThatHas more file

-- | How to start the kernel.
--
-- @CELLAR_KERNEL@ names it outright, which is how a packaged Cellar points at
-- the one it shipped.  Failing that the kernel is looked for beside the working
-- directory, which is where it lives in the source tree, and failing that it is
-- looked for on PATH -- which is what an installed Cellar has.

kernelCommandLine :: IO (FilePath, [String])
kernelCommandLine = do
  override <- lookupEnv "CELLAR_KERNEL"
  case override of
    Just command | not (null (words command)) -> case words command of
      (program : arguments) -> pure (program, arguments)
      [] -> pure ("cellar-kernel", [])
    _ -> do
      here <- getCurrentDirectory
      let script = here </> "bin" </> "cellar-kernel.scm"
      exists <- doesFileExist script
      pure $ if exists
        then ("guile", ["-L", here </> "src", "-s", script])
        else ("cellar-kernel", [])
