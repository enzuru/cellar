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
import System.FilePath ((</>), takeFileName)

import Data.GI.Base
import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import Cellar.Client
import Cellar.Config
import Cellar.Grid
import Cellar.Store
import Cellar.App.Types
import Cellar.App.Kernel
import Cellar.App.Workbook


-- | Put an alert dialog up and do something with the answer.
--
-- This replaced three fields on the 'App' record.  A dialog whose response is
-- handled by a callback wired once at startup has to leave a note somewhere
-- saying what the answer will mean -- which sheet is being renamed, whether
-- this is a copy or a new workbook, which page is waiting to close -- and that
-- note is state which exists only between a question and its answer, in a
-- record that lives as long as the window.  Asking here instead means the
-- answer arrives where the question was asked, with everything it needs
-- already in scope and nothing left behind afterwards.
choose :: App -> Adw.AlertDialog -> (Text -> IO ()) -> IO ()
choose app dialog continue =
  Adw.alertDialogChoose dialog (Just (appWindow app))
    (Nothing :: Maybe Gio.Cancellable) $ Just $ \_ result -> do
      response <- Adw.alertDialogChooseFinish dialog result
      continue response

wireDialogs :: App -> Gtk.Button -> IO ()
wireDialogs app editButton = do
  newSheetLocation <- object (appBuilder app) "new_sheet_location" Gtk.Button
  newSheetLocationLabel <- object (appBuilder app) "new_sheet_location_label" Gtk.Label

  _ <- on editButton #clicked $ do
    tab <- currentTab app
    forM_ tab $ \t -> gridActiveRef (tabGrid t) >>= editCell app t
  _ <- on (appRecalculate app) #clicked $ do
    tab <- currentTab app
    forM_ tab $ \t ->
      askSheet app t "recalculate" [] $ \payload -> do
        takeSnapshot app t payload
        notify app "Recalculated"
  _ <- on newSheetLocation #clicked $ chooseFolder app $ \path -> do
    writeIORef (appLocation app) path
    Gtk.labelSetLabel newSheetLocationLabel (T.pack path)
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
  dialog <- object (appBuilder app) "delete_sheet_dialog" Adw.AlertDialog
  name <- readIORef (tabName tab)
  set dialog
    [ #heading := T.pack ("Delete " ++ name ++ "?")
    , #body := "The sheet's folder and every cell file in it are deleted from \
               \the workbook. Cellar cannot undo that — though if the workbook \
               \is a Git repository, Git can." ]
  choose app dialog $ \response ->
    if response == "delete"
      then do
        deleted <- deleteSheet app tab
        Adw.tabViewClosePageFinish (appTabView app) page deleted
      else Adw.tabViewClosePageFinish (appTabView app) page False


askForSheetName :: App -> Maybe Tab -> IO ()
askForSheetName app tab = do
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
        Just open -> uniqueSheetName open ("Sheet " ++ show (length tabs + 1))
  Gtk.editableSetText entry (T.pack suggestion)
  choose app dialog $ \response ->
    when (response == "name") $ do
      typed <- T.unpack . T.strip <$> Gtk.editableGetText entry
      case tab of
        Just t -> renameSheet app t typed
        Nothing -> addSheet app typed


askForNewWorkbook :: App -> String -> Bool -> IO ()
askForNewWorkbook app suggestion copying = do
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
  choose app dialog $ \response ->
    when (response == "create") (createNewWorkbook app copying)


createNewWorkbook :: App -> Bool -> IO ()
createNewWorkbook app copying = do
  nameEntry <- object (appBuilder app) "new_sheet_name" Gtk.Entry
  gitCheck <- object (appBuilder app) "new_sheet_git" Gtk.CheckButton
  typed <- T.unpack . T.strip <$> Gtk.editableGetText nameEntry
  location <- readIORef (appLocation app)
  wantsGit <- Gtk.checkButtonGetActive gitCheck
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
            notify app (T.pack ("Created " ++ takeFileName directory))

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
  commandRow <- object builder "external_editor_command" Adw.EntryRow
  overrideRow <- object builder "override_row" Adw.ActionRow
  config <- readIORef (appConfig app)
  Gtk.editableSetText commandRow (T.pack (externalEditorCommand config))

  -- CELLAR_EDITOR wins over whatever is set here, so when it is set the dialog
  -- says so rather than letting somebody change a preference that is not the
  -- one in effect.
  override <- editorOverride
  case override of
    NoOverride -> Gtk.widgetSetVisible overrideRow False
    UseDesktop -> do
      Adw.actionRowSetSubtitle overrideRow
        "CELLAR_EDITOR is set to nothing, so a cell opened elsewhere goes to \
        \whatever the desktop opens text files with, however this is left."
      Gtk.widgetSetVisible overrideRow True
    UseCommand command -> do
      Adw.actionRowSetSubtitle overrideRow
        (T.pack ("CELLAR_EDITOR is set to " ++ command
                 ++ ", so that is what cells open in however this is left."))
      Gtk.widgetSetVisible overrideRow True

  let remember = do
        command <- T.unpack <$> Gtk.editableGetText commandRow
        let updated = Config command
        writeIORef (appConfig app) updated
        saveConfig updated
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
  , ("Double-click / Enter / Ctrl+E", "Edit the active cell in Cellar")
  , ("Ctrl+Shift+E", "Open the active cell's file in your text editor")
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
