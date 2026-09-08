{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The workbooks opened lately: the list on the start page, and the submenu
-- beside Open.
--
-- GTK keeps a recent-files list of its own -- the one behind
-- @recently-used.xbel@ that the shell shows under an application's icon -- but
-- it was deprecated in GTK 4.10 and it is keyed on files, while a Cellar
-- workbook is a folder.  So this list is Cellar's own, which is what GNOME's
-- own applications do now: a handful of paths in the preferences file, shown
-- as a boxed list where the start page has room for one.
--
-- Nothing here opens a workbook.  A row and a menu item both fire
-- @app.open-recent@ with the folder as its target, which keeps this module
-- underneath the one that knows how to open a workbook rather than beside it.
module Cellar.App.Recent
  ( installRecent
  , refreshRecent
  , rememberWorkbook
  , clearRecent
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM_, unless, void, when)
import Data.IORef
import qualified Data.Text as T
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, takeFileName)

import Data.GI.Base
import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import Cellar.Config
import Cellar.Store
import Cellar.App.Types

-- | Put the submenu where the menu can find it, and fill both halves in.
--
-- The section is empty until there is something to put in it, and an empty
-- section is one GTK draws nothing for -- which is why the menu is built here
-- rather than in the Blueprint file: a menu written there is fixed, and this
-- one is as long as the list is.
installRecent :: App -> IO ()
installRecent app = do
  primary <- object (appBuilder app) "primary_menu" Gio.Menu
  -- Position 1: after the section that opens and makes workbooks, which is
  -- where somebody looking for Open Recent looks.
  Gio.menuInsertSection primary 1 (Nothing :: Maybe T.Text) (appRecentSection app)
  refreshRecent app

-- | Show the list as it now stands.
--
-- A remembered folder that is no longer a workbook -- deleted, renamed, on a
-- drive that is not mounted -- is dropped rather than shown greyed out, since
-- the only thing to be done about one is to forget it.
refreshRecent :: App -> IO ()
refreshRecent app = do
  config <- readIORef (appConfig app)
  let remembered = recentWorkbooks config
  present <- filterM isWorkbookDirectory remembered
  when (present /= remembered) (saveRecent app config present)
  fillList app present
  fillMenu app present

-- | Note that a workbook has been opened.  It goes to the front of the list.
rememberWorkbook :: App -> FilePath -> IO ()
rememberWorkbook app path = do
  -- Canonical, so that the same workbook reached by two paths -- a relative
  -- one on the command line, a symlink, a sheet folder inside it -- is one
  -- entry rather than several.
  resolved <- try (canonicalizePath path) :: IO (Either SomeException FilePath)
  config <- readIORef (appConfig app)
  let canonical = either (const path) id resolved
      updated = rememberRecent canonical (recentWorkbooks config)
  when (updated /= recentWorkbooks config) (saveRecent app config updated)
  refreshRecent app

-- | Forget the lot, for when the list says more about where you have been than
-- you would like it to.
clearRecent :: App -> IO ()
clearRecent app = do
  config <- readIORef (appConfig app)
  unless (null (recentWorkbooks config)) $ do
    saveRecent app config []
    notify app "Cleared the recent workbooks"
  refreshRecent app

saveRecent :: App -> Config -> [FilePath] -> IO ()
saveRecent app config paths = do
  let updated = config { recentWorkbooks = paths }
  writeIORef (appConfig app) updated
  reportFailure app "save the list of recent workbooks" (saveConfig updated)

-- The start page

fillList :: App -> [FilePath] -> IO ()
fillList app paths = do
  emptyList (appRecentList app)
  home <- lookupEnv "HOME"
  forM_ paths $ \path -> do
    row <- new Adw.ActionRow
      [ #title := T.pack (takeFileName path)
      , #subtitle := T.pack (abbreviate home (takeDirectory path))
      , #useMarkup := False
      , #activatable := True ]
    icon <- new Gtk.Image [#iconName := "folder-symbolic"]
    Adw.actionRowAddPrefix row icon
    _ <- on row #activated (openRecent row path)
    Gtk.listBoxAppend (appRecentList app) row
  Gtk.widgetSetVisible (appRecentBox app) (not (null paths))

-- | Ask for the workbook this row stands for.  The row is not actionable --
-- no list box row is -- so the action is fired at it rather than set on it.
openRecent :: Adw.ActionRow -> FilePath -> IO ()
openRecent row path = do
  target <- toGVariant (T.pack path)
  void (Gtk.widgetActivateAction row "app.open-recent" (Just target))

emptyList :: Gtk.ListBox -> IO ()
emptyList listBox = do
  child <- Gtk.widgetGetFirstChild listBox
  forM_ child $ \c -> do
    Gtk.listBoxRemove listBox c
    emptyList listBox

-- The menu

fillMenu :: App -> [FilePath] -> IO ()
fillMenu app paths = do
  let section = appRecentSection app
  Gio.menuRemoveAll section
  unless (null paths) $ do
    inner <- Gio.menuNew
    forM_ paths $ \path -> do
      item <- Gio.menuItemNew (Just (T.pack (menuLabel path))) Nothing
      target <- toGVariant (T.pack path)
      Gio.menuItemSetActionAndTargetValue item (Just "app.open-recent") (Just target)
      Gio.menuAppendItem inner item
    forgetting <- Gio.menuNew
    Gio.menuAppend forgetting (Just "_Clear Recent Workbooks") (Just "app.clear-recent")
    Gio.menuAppendSection inner (Nothing :: Maybe T.Text) forgetting
    Gio.menuAppendSubmenu section (Just "Open _Recent") inner
