{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The dialogs, and the signals that raise them.
--
-- Naming a sheet, deleting one, making a workbook, choosing a folder: each is a
-- question with a couple of answers, and this is where they are asked.
--
module Cellar.App.Dialogs where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Data.IORef
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))

import Data.GI.Base
import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import Cellar.Client
import Cellar.Config
import Cellar.Grid
import Cellar.Sexp
import Cellar.Store
import Cellar.App.Types
import Cellar.App.Kernel
import Cellar.App.Workbook


wireDialogs :: App -> Gtk.Button -> IO ()
wireDialogs app editButton = do
  newSheetDialog <- object (appBuilder app) "new_sheet_dialog" Adw.AlertDialog
  newSheetLocation <- object (appBuilder app) "new_sheet_location" Gtk.Button
  newSheetLocationLabel <- object (appBuilder app) "new_sheet_location_label" Gtk.Label
  sheetNameDialog <- object (appBuilder app) "sheet_name_dialog" Adw.AlertDialog
  deleteSheetDialog <- object (appBuilder app) "delete_sheet_dialog" Adw.AlertDialog

  _ <- on editButton #clicked $ do
    tab <- currentTab app
    forM_ tab $ \t -> gridActiveRef (tabGrid t) >>= editCell app t
  _ <- on (appRecalculate app) #clicked $ do
    tab <- currentTab app
    forM_ tab $ \t ->
      ask app "recalculate" [Num (fromIntegral (tabId t))] $ \payload -> do
        takeSnapshot app t payload
        notify app "Recalculated"
  _ <- on newSheetLocation #clicked $ chooseFolder app $ \path -> do
    writeIORef (appLocation app) path
    Gtk.labelSetLabel newSheetLocationLabel (T.pack path)
  _ <- on newSheetDialog #response $ \response ->
    when (response == "create") (createNewWorkbook app)
  _ <- on sheetNameDialog #response $ \response ->
    when (response == "name") $ do
      entry <- object (appBuilder app) "sheet_name_entry" Gtk.Entry
      typed <- T.unpack . T.strip <$> Gtk.editableGetText entry
      renaming <- readIORef (appRenaming app)
      writeIORef (appRenaming app) Nothing
      case renaming of
        Just tab -> renameSheet app tab typed
        Nothing -> addSheet app typed
  _ <- on deleteSheetDialog #response $ \response -> do
    pending <- readIORef (appPendingDelete app)
    writeIORef (appPendingDelete app) Nothing
    forM_ pending $ \(tab, page) ->
      if response == "delete"
        then do
          deleted <- deleteSheet app tab
          Adw.tabViewClosePageFinish (appTabView app) page deleted
        else Adw.tabViewClosePageFinish (appTabView app) page False

  _ <- on (appTabView app) #closePage $ \page -> onClosePage app page
  _ <- on (appTabView app) #pageReordered $ \_ _ -> persistOrder app
  _ <- on (appTabView app) (PropertyNotify #selectedPage) $ \_ -> onTabSelected app
  _ <- on (appWindow app) #closeRequest $ do
    stopKernel (appKernel app)
    -- False: carry on closing.  This is tidying up, not a veto.
    pure False
  pure ()

onClosePage :: App -> Adw.TabPage -> IO Bool
onClosePage app page = do
  loading <- readIORef (appLoading app)
  if loading
    -- Tabs being torn down to open another workbook.  Nothing is being
    -- deleted, so let the default handler take the page away.
    then pure False
    else do
      tab <- tabForPage app page
      tabs <- readIORef (appTabs app)
      case tab of
        Nothing -> do
          Adw.tabViewClosePageFinish (appTabView app) page True
          pure True
        Just t
          | length tabs <= 1 -> do
              notify app "A workbook has to keep at least one sheet"
              Adw.tabViewClosePageFinish (appTabView app) page False
              pure True
          | otherwise -> do
              askToDelete app t page
              pure True


askToDelete :: App -> Tab -> Adw.TabPage -> IO ()
askToDelete app tab page = do
  writeIORef (appPendingDelete app) (Just (tab, page))
  dialog <- object (appBuilder app) "delete_sheet_dialog" Adw.AlertDialog
  name <- readIORef (tabName tab)
  set dialog
    [ #heading := T.pack ("Delete " ++ name ++ "?")
    , #body := "The sheet's folder and every cell file in it are deleted from \
               \the workbook. Cellar cannot undo that — though if the workbook \
               \is a Git repository, Git can." ]
  Adw.dialogPresent dialog (Just (appWindow app))


askForSheetName :: App -> Maybe Tab -> IO ()
askForSheetName app tab = do
  writeIORef (appRenaming app) tab
  dialog <- object (appBuilder app) "sheet_name_dialog" Adw.AlertDialog
  entry <- object (appBuilder app) "sheet_name_entry" Gtk.Entry
  set dialog
    [ #heading := if isJust tab then "Rename Sheet" else "Add Sheet"
    , #body := if isJust tab
        then "The sheet's folder is renamed along with it, so a cell keeps the \
             \history it already had."
        else "A sheet is a folder of its own inside the workbook, so its name \
             \is a folder's name." ]
  Adw.alertDialogSetResponseLabel dialog "name" (if isJust tab then "Rename" else "Add")
  suggestion <- case tab of
    Just t -> readIORef (tabName t)
    Nothing -> do
      workbook <- readIORef (appWorkbook app)
      tabs <- readIORef (appTabs app)
      case workbook of
        Nothing -> pure firstSheetName
        Just path -> uniqueSheetName path ("Sheet " ++ show (length tabs + 1))
  Gtk.editableSetText entry (T.pack suggestion)
  Adw.dialogPresent dialog (Just (appWindow app))


askForNewWorkbook :: App -> String -> Bool -> IO ()
askForNewWorkbook app suggestion copying = do
  writeIORef (appCopying app) copying
  dialog <- object (appBuilder app) "new_sheet_dialog" Adw.AlertDialog
  nameEntry <- object (appBuilder app) "new_sheet_name" Gtk.Entry
  locationLabel <- object (appBuilder app) "new_sheet_location_label" Gtk.Label
  set dialog
    [ #heading := if copying then "Copy Workbook To" else "New Workbook"
    , #body := if copying
        then "The workbook is written to a new folder, and that is the one you \
             \carry on editing. The folder you were in is left as it stands."
        else "A workbook is a folder, and a Git repository worth making one of: \
             \a folder for each sheet in it, and one small file for every cell." ]
  Adw.alertDialogSetResponseLabel dialog "create" (if copying then "Copy" else "Create")
  Gtk.editableSetText nameEntry (T.pack suggestion)
  location <- readIORef (appLocation app)
  Gtk.labelSetLabel locationLabel (T.pack location)
  Adw.dialogPresent dialog (Just (appWindow app))


createNewWorkbook :: App -> IO ()
createNewWorkbook app = do
  nameEntry <- object (appBuilder app) "new_sheet_name" Gtk.Entry
  gitCheck <- object (appBuilder app) "new_sheet_git" Gtk.CheckButton
  typed <- T.unpack . T.strip <$> Gtk.editableGetText nameEntry
  location <- readIORef (appLocation app)
  wantsGit <- Gtk.checkButtonGetActive gitCheck
  copying <- readIORef (appCopying app)
  let name = if null typed then "workbook" else typed
      directory = location </> workbookFolderName name
  if copying
    then copyTo app directory wantsGit
    else do
      outcome <- try (createWorkbook directory firstSheetName)
      case outcome :: Either StoreError () of
        Left (StoreError why) -> notify app (T.pack why)
        Right () -> do
          when wantsGit (gitInit app directory)
          opened <- openWorkbook app directory
          when opened $ do
            persistFreshLayouts app
            notify app (T.pack ("Created " ++ workbookName directory))

-- | Write every sheet of the workbook to a folder of its own and carry on
-- editing it there.  The workbook you were in is left exactly as it was.

chooseFolder :: App -> (FilePath -> IO ()) -> IO ()
chooseFolder app continue = do
  dialog <- new Gtk.FileDialog [#title := "Choose a Folder"]
  Gtk.fileDialogSelectFolder dialog (Just (appWindow app))
    (Nothing :: Maybe Gio.Cancellable) $ Just $ \_ result -> do
      -- Cancelling raises, which is the one outcome we expect and ignore.
      outcome <- try (Gtk.fileDialogSelectFolderFinish dialog result)
      case outcome :: Either SomeException Gio.File of
        Left _ -> pure ()
        Right file -> do
          path <- Gio.fileGetPath file
          forM_ path continue

-- The rest of the dialogs


openPreferences :: App -> IO ()
openPreferences app = do
  builder <- Gtk.builderNewFromFile (appUiDirectory app </> "preferences.ui")
  dialog <- object builder "preferences_dialog" Adw.PreferencesDialog
  switch <- object builder "external_editor_row" Adw.SwitchRow
  commandRow <- object builder "editor_command_row" Adw.EntryRow
  config <- readIORef (appConfig app)
  Adw.switchRowSetActive switch (externalEditorEnabled config)
  Gtk.editableSetText commandRow (T.pack (externalEditorCommand config))
  let remember = do
        enabled <- Adw.switchRowGetActive switch
        command <- T.unpack <$> Gtk.editableGetText commandRow
        let updated = Config enabled command
        writeIORef (appConfig app) updated
        saveConfig updated
  _ <- on switch (PropertyNotify #active) (\_ -> remember)
  _ <- on commandRow #changed remember
  Adw.dialogPresent dialog (Just (appWindow app))


showAbout :: App -> IO ()
showAbout app = do
  dialog <- new Adw.AboutDialog
    [ #applicationName := "Cellar"
    , #applicationIcon := applicationId
    , #version := "0.1.0"
    , #developerName := "A Haskell shell around a Guile kernel, in GTK4"
    , #comments := "A spreadsheet with no formula language. Every cell is a \
                   \Guile expression, and references like A1 are just variables \
                   \you can use in it."
    , #licenseType := Gtk.LicenseGpl30
    , #website := "https://www.gnu.org/software/guile/" ]
  Adw.dialogPresent dialog (Just (appWindow app))


shortcuts :: [(Text, Text)]
shortcuts =
  [ ("Arrow keys / Tab", "Move the active cell")
  , ("Double-click / Enter", "Edit the active cell's Guile source")
  , ("Ctrl+Return", "Apply, while in the editor")
  , ("Delete", "Clear the active cell")
  , ("Ctrl+Shift+Up / Down", "Move the active row up or down")
  , ("Ctrl+Shift+Left / Right", "Move the active column left or right")
  , ("Ctrl+Alt+Up / Down", "Insert a row before or after the active one")
  , ("Ctrl+Alt+Left / Right", "Insert a column before or after the active one")
  , ("Right-click a row number or a column header", "The same four inserts")
  , ("Ctrl+R", "Recalculate the sheet")
  , ("Ctrl+T", "Add a sheet to this workbook")
  , ("Ctrl+Shift+R", "Rename the sheet showing")
  , ("Ctrl+W", "Delete the sheet showing")
  , ("Ctrl+Page Up / Page Down", "Move to the sheet before or after this one")
  , ("Drag a tab", "Reorder the sheets")
  , ("Ctrl+N / Ctrl+Shift+N", "New workbook / New scratch workbook")
  , ("Ctrl+O", "Open a workbook folder")
  , ("Ctrl+Shift+S", "Copy this workbook to another folder")
  , ("Ctrl+,", "Preferences")
  , ("Ctrl+Q", "Quit")
  ]


showShortcuts :: App -> IO ()
showShortcuts app = do
  dialog <- new Adw.AlertDialog
    [ #heading := "Keyboard Shortcuts"
    , #body := T.intercalate "\n"
        [ key <> " — " <> what | (key, what) <- shortcuts ] ]
  Adw.alertDialogAddResponse dialog "close" "Close"
  Adw.dialogPresent dialog (Just (appWindow app))

-- Styling and resources
