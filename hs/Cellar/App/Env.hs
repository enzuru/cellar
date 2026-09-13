{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The parts of the window that are not a function of its state.
--
-- A dialog is asked once and answered once, a toast appears and fades, a
-- gesture belongs to a widget GTK built for itself.  None of that is a
-- function of anything, so none of it is in the view.  What is here is the
-- handles those things need and the procedures that use them, and every one of
-- them says what happened by posting an 'Event' rather than by changing
-- anything.
--
-- Everything here touches GTK, and the update runs off the main thread, so
-- everything here goes through 'onMain'.
module Cellar.App.Env
  ( Env (..)
  , newEnv
  , onMain
  , post
  , notify
  , asking
  , tagOf
  , forgetTags
  , gesturesFor
  , forgetGestures
  , tookWindow
  , tookToasts
  , showDrag
  , askSheetName
  , askNewWorkbook
  , askToDelete
  , chooseFolder
  , openPreferences
  , showAbout
  , showShortcuts
  , askAboutTheKernel
  , neverMindTheKernel
  , openEditor
  , answerEditor
  , watchPathsFor
  , unwatchAll
  , fillRecentMenu
  , openWithDesktop
  , object
  , optionalObject
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>), takeFileName)

import Data.GI.Base
import qualified GI.Adw as Adw
import qualified GI.GLib as GLib
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import Cellar.App.Event
import Cellar.App.State
import Cellar.Client
import Cellar.Config
import Cellar.Editor
import Cellar.Grid.Gestures
import Cellar.Grid.Model (GridModel, modelDrag)
import Cellar.Ref
import Cellar.Sexp
import Cellar.Store (workbookFolderName)
import Cellar.Watch

data Env = Env
  { envKernel :: Kernel
    -- | Where an event goes.  The loop reads the other end of this.
  , envPost :: Event -> IO ()
  , envUiDirectory :: FilePath
    -- | The Blueprint file, which still holds the menus and the dialogs.
  , envBuilder :: Gtk.Builder
  , envWindow :: IORef (Maybe Adw.ApplicationWindow)
  , envToasts :: IORef (Maybe Adw.ToastOverlay)
    -- | The gestures on each sheet's grid, made the first time its widgets
    -- are.
  , envGestures :: IORef (M.Map TabId Gestures)
  , envWatcher :: IORef (Maybe Watcher)
    -- | The cell editor asks the kernel what a half-written expression comes
    -- to, and is handed the answer.  It is the one part of Cellar that is
    -- still a window of its own rather than part of this one, so it keeps the
    -- old way of asking.
    -- | What each request the kernel owes an answer for was for.  It lives
    -- here rather than in the state because it has to be written down before
    -- the answer can arrive, and only something holding a reference can do
    -- that: the update changes the state by handing an event back, which is
    -- one turn of the loop too late.
  , envTags :: IORef (M.Map Int Tag)
  , envPreviews :: IORef (M.Map Int (Sexp -> IO ()))
    -- | The submenu of workbooks opened lately, which Cellar fills in because
    -- its length is not known until the preferences are read.
  , envRecentSection :: Gio.Menu
  , envStallDialog :: IORef (Maybe Adw.AlertDialog)
  }

newEnv :: Kernel -> (Event -> IO ()) -> FilePath -> Gtk.Builder -> IO Env
newEnv kernel poster uiDirectory builder = do
  section <- Gio.menuNew
  primary <- object builder "primary_menu" Gio.Menu
  -- Position 1: after the section that opens and makes workbooks, which is
  -- where somebody looking for Open Recent looks.
  Gio.menuInsertSection primary 1 (Nothing :: Maybe Text) section
  Env kernel poster uiDirectory builder
    <$> newIORef Nothing
    <*> newIORef Nothing
    <*> newIORef M.empty
    <*> newIORef Nothing
    <*> newIORef M.empty
    <*> newIORef M.empty
    <*> pure section
    <*> newIORef Nothing

post :: Env -> Event -> IO ()
post = envPost

-- | Do this on the main loop's own thread, which is where GTK belongs.  The
-- update runs on a thread of its own, so nothing it asks for can touch a
-- widget without coming through here.
onMain :: IO () -> IO ()
onMain action = void (GLib.idleAdd GLib.PRIORITY_DEFAULT (action >> pure False))

-- | Say something in passing.
notify :: Env -> Text -> IO ()
notify env message = onMain $ do
  overlay <- readIORef (envToasts env)
  forM_ overlay $ \toasts -> do
    toast <- new Adw.Toast [#title := message]
    Adw.toastOverlayAddToast toasts toast

-- | Ask the kernel for these things, and say what each request was for.
--
-- The request is numbered and written down before it is sent, because the
-- kernel is quick: an answer can be read off the pipe by the time a request
-- that was sent first has been noted.
asking :: Env -> [(String, [Sexp], Tag)] -> IO ()
asking env wanted = forM_ wanted $ \(op, arguments, tag) -> do
  requestId <- reserve (envKernel env)
  modifyIORef' (envTags env) (M.insert requestId tag)
  sendRequest (envKernel env) requestId op arguments

-- | What a request was for, forgetting it on the way out.  A number nobody
-- wrote anything down for is 'Ignored', which is what an answer to something
-- abandoned when the kernel restarted looks like.
tagOf :: Env -> Int -> IO Tag
tagOf env requestId = atomicModifyIORef' (envTags env) $ \tags ->
  (M.delete requestId tags, fromMaybe Ignored (M.lookup requestId tags))

-- | Forget everything the kernel owed, because it will not be answering.
forgetTags :: Env -> IO ()
forgetTags env = writeIORef (envTags env) M.empty

-- | The window, once the view has built it.  Dialogs are shown over it and
-- toasts inside it, and neither is part of what is drawn.
tookWindow :: Env -> Adw.ApplicationWindow -> IO ()
tookWindow env window = writeIORef (envWindow env) (Just window)

tookToasts :: Env -> Adw.ToastOverlay -> IO ()
tookToasts env toasts = writeIORef (envToasts env) (Just toasts)

--
-- The gestures
--

-- | The gestures for one sheet's grid, made the first time they are asked
-- after.  What they notice is posted with the sheet they belong to.
gesturesFor :: Env -> TabId -> IO Gestures
gesturesFor env tab = do
  known <- readIORef (envGestures env)
  case M.lookup tab known of
    Just gestures -> pure gestures
    Nothing -> do
      menu <- optionalObject (envBuilder env) "line_menu" Gio.MenuModel
      gestures <- newGestures menu (post env . fromGesture tab)
      modifyIORef' (envGestures env) (M.insert tab gestures)
      pure gestures

fromGesture :: TabId -> GridGesture -> Event
fromGesture tab gesture = case gesture of
  LinePicked axis index -> LineChosen tab axis index
  DragBegan axis index -> DragBegun tab axis index
  DragMovedTo index -> DragMoved tab index
  DragDroppedOn index -> DragDropped tab index
  DragGaveUp -> DragCancelled tab

forgetGestures :: Env -> TabId -> IO ()
forgetGestures env tab = modifyIORef' (envGestures env) (M.delete tab)

-- | Paint the drag on the column headers, which are GTK's widgets rather than
-- ours to draw.
showDrag :: Env -> TabId -> GridModel -> IO ()
showDrag env tab model = onMain $ do
  gestures <- gesturesFor env tab
  gestureDragShown gestures (modelDrag model)

--
-- The dialogs
--

-- | Ask for a sheet's name, to add one or to rename one.
askSheetName :: Env -> Maybe TabId -> String -> String -> IO ()
askSheetName env tab suggestion heading = onMain $ do
  dialog <- object (envBuilder env) "sheet_name_dialog" Adw.AlertDialog
  entry <- object (envBuilder env) "sheet_name_entry" Gtk.Entry
  set dialog
    [ #heading := T.pack heading
    , #body := if isRename
        then "The sheet's folder is renamed along with it, so a cell keeps the \
             \history it already had."
        else "A sheet is a folder of its own inside the workbook, so its name \
             \is a folder's name." ]
  Adw.alertDialogSetResponseLabel dialog "name" (if isRename then "Rename" else "Add")
  Gtk.editableSetText entry (T.pack suggestion)
  choose env dialog $ \response ->
    when (response == "name") $ do
      typed <- T.unpack . T.strip <$> Gtk.editableGetText entry
      post env (SheetNamed tab typed)
  where isRename = maybe False (const True) tab

-- | Ask where a new workbook goes, and what it is called.
askNewWorkbook :: Env -> String -> FilePath -> Bool -> IO ()
askNewWorkbook env suggestion location copying = onMain $ do
  dialog <- object (envBuilder env) "new_sheet_dialog" Adw.AlertDialog
  nameEntry <- object (envBuilder env) "new_sheet_name" Gtk.Entry
  locationButton <- object (envBuilder env) "new_sheet_location" Gtk.Button
  locationLabel <- object (envBuilder env) "new_sheet_location_label" Gtk.Label
  gitToggle <- object (envBuilder env) "new_sheet_git" Gtk.CheckButton
  set dialog
    [ #heading := if copying then "Copy Workbook To" else "New Workbook"
    , #body := if copying
        then "The workbook is written to a new folder, and that is the one you \
             \carry on editing. The folder you were in is left as it stands."
        else "A workbook is a folder, and a Git repository worth making one of: \
             \a folder for each sheet in it, and one small file for every cell." ]
  Adw.alertDialogSetResponseLabel dialog "create" (if copying then "Copy" else "Create")
  Gtk.editableSetText nameEntry (T.pack suggestion)
  Gtk.labelSetLabel locationLabel (T.pack location)
  -- The folder button answers into the label, which is where the answer is
  -- read from when the dialog is answered.
  _ <- on locationButton #clicked $ chooseFolderWith env $ \path ->
    onMain (Gtk.labelSetLabel locationLabel (T.pack path))
  choose env dialog $ \response ->
    when (response == "create") $ do
      typed <- T.unpack . T.strip <$> Gtk.editableGetText nameEntry
      chosen <- T.unpack <$> Gtk.labelGetLabel locationLabel
      wantsGit <- Gtk.checkButtonGetActive gitToggle
      -- A workbook is a folder whose name ends in .cellar, which is what the
      -- desktop and the shell both go by.
      let name = if null typed then "workbook" else typed
      post env (WorkbookMade (chosen </> workbookFolderName name) copying wantsGit)

-- | A tab's close button, or Delete Sheet.  A tab is a sheet of the workbook
-- rather than a view of one, so closing it is deleting it.
askToDelete :: Env -> TabId -> String -> IO ()
askToDelete env tab name = onMain $ do
  dialog <- object (envBuilder env) "delete_sheet_dialog" Adw.AlertDialog
  set dialog [ #body := T.pack ("The sheet " ++ name ++ " and its cells are \
                                \deleted from the workbook folder.") ]
  choose env dialog $ \response ->
    post env (if response == "delete" then SheetDeleted tab name else TabKept tab)

-- | Ask for a folder, and post it.
chooseFolder :: Env -> IO ()
chooseFolder env = chooseFolderWith env (post env . FolderChosen)

chooseFolderWith :: Env -> (FilePath -> IO ()) -> IO ()
chooseFolderWith env continue = onMain $ do
  window <- readIORef (envWindow env)
  dialog <- new Gtk.FileDialog [#title := "Choose a Folder"]
  Gtk.fileDialogSelectFolder dialog window
    (Nothing :: Maybe Gio.Cancellable) $ Just $ \_ result -> do
      -- Cancelling raises, which is the one outcome we expect and ignore.
      outcome <- try (Gtk.fileDialogSelectFolderFinish dialog result)
      case outcome :: Either SomeException Gio.File of
        Left _ -> pure ()
        Right file -> do
          path <- Gio.fileGetPath file
          forM_ path continue

-- | Put a dialog on screen and hand its answer on.
choose :: Env -> Adw.AlertDialog -> (Text -> IO ()) -> IO ()
choose env dialog continue = do
  window <- readIORef (envWindow env)
  Adw.alertDialogChoose dialog window (Nothing :: Maybe Gio.Cancellable) $ Just $
    \_ result -> do
      response <- Adw.alertDialogChooseFinish dialog result
      continue response

openPreferences :: Env -> Config -> IO ()
openPreferences env config = onMain $ do
  builder <- Gtk.builderNewFromFile (envUiDirectory env </> "preferences.ui")
  dialog <- object builder "preferences_dialog" Adw.PreferencesDialog
  commandRow <- object builder "external_editor_command" Adw.EntryRow
  overrideRow <- object builder "override_row" Adw.ActionRow
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

  _ <- on commandRow #changed $ do
    command <- T.unpack <$> Gtk.editableGetText commandRow
    post env (EditorCommandSet command)
  window <- readIORef (envWindow env)
  Adw.dialogPresent dialog window

showAbout :: Env -> IO ()
showAbout env = onMain $ do
  dialog <- new Adw.AboutDialog
    [ #applicationName := "Cellar"
    , #applicationIcon := "dev.enzuru.Cellar"
    , #version := "0.1.0"
    , #developerName := "A Haskell shell around a Guile kernel, in GTK4"
    , #comments := "A spreadsheet with no formula language. Every cell is a \
                   \Guile expression, and references like A1 are just variables \
                   \you can use in it."
    , #licenseType := Gtk.LicenseGpl30
    , #website := "https://www.gnu.org/software/guile/" ]
  window <- readIORef (envWindow env)
  Adw.dialogPresent dialog window

showShortcuts :: Env -> IO ()
showShortcuts env = onMain $ do
  dialog <- new Adw.AlertDialog
    [ #heading := "Keyboard Shortcuts"
    , #body := T.intercalate "\n"
        [ key <> " \8212 " <> what | (key, what) <- shortcuts ] ]
  Adw.alertDialogAddResponse dialog "close" "Close"
  window <- readIORef (envWindow env)
  Adw.dialogPresent dialog window

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

-- | Ask whether to stop a cell that is not going to finish.
askAboutTheKernel :: Env -> IO ()
askAboutTheKernel env = onMain $ do
  dialog <- new Adw.AlertDialog
    [ #heading := "A cell is taking a long time"
    , #body := T.intercalate "\n\n"
        [ "Cellar is still waiting for the kernel to finish working something \
          \out. A cell can be given an expression that never finishes \8212 \
          \(let loop () (loop)) \8212 and this is what that looks like."
        , "Stopping restarts the kernel and reloads the sheets as they stand \
          \on disk. The edit that caused it was never written, so stopping \
          \loses nothing that was saved." ] ]
  Adw.alertDialogAddResponse dialog "wait" "Keep Waiting"
  Adw.alertDialogAddResponse dialog "stop" "Stop the Cell"
  Adw.alertDialogSetResponseAppearance dialog "stop" Adw.ResponseAppearanceDestructive
  Adw.alertDialogSetDefaultResponse dialog (Just "wait")
  Adw.alertDialogSetCloseResponse dialog "wait"
  writeIORef (envStallDialog env) (Just dialog)
  choose env dialog $ \response -> do
    writeIORef (envStallDialog env) Nothing
    post env (if response == "stop" then Stopped else NeverMind)

-- | Take the dialog down, for a cell that finished while it was up.
neverMindTheKernel :: Env -> IO ()
neverMindTheKernel env = onMain $ do
  dialog <- readIORef (envStallDialog env)
  forM_ dialog $ \up -> do
    writeIORef (envStallDialog env) Nothing
    Adw.dialogForceClose up

--
-- The cell editor, which is a window of its own
--

openEditor :: Env -> TabId -> String -> Ref -> Maybe String -> IO ()
openEditor env tab sheetName r source = onMain $ do
  window <- readIORef (envWindow env)
  forM_ window $ \parent ->
    openCellEditor (envUiDirectory env) parent r source
      (\text answer -> do
         requestId <- call (envKernel env) "preview"
                        [Str sheetName, Str (refName r), Str text]
         modifyIORef' (envPreviews env) $ M.insert requestId $ \payload ->
           answer (previewOf payload))
      (\text -> post env (CellEdited tab r text))

previewOf :: Sexp -> Preview
previewOf payload = Preview
  { previewText = fromMaybe "" (lookupKey "display" payload >>= asString)
  , previewIsError = maybe False (const True)
      (lookupKey "error" payload >>= asString)
  }

-- | Hand an answer to the editor, if the editor is what asked.  Answers with
-- whether it was.
answerEditor :: Env -> Int -> Sexp -> IO Bool
answerEditor env requestId payload = do
  found <- atomicModifyIORef' (envPreviews env) $ \waiting ->
    (M.delete requestId waiting, M.lookup requestId waiting)
  case found of
    Nothing -> pure False
    Just answer -> onMain (answer payload) >> pure True

-- | The submenu of workbooks opened lately.  Built here rather than in the
-- Blueprint file because its length is not known until the preferences are
-- read, and an empty section is one GTK draws nothing for.
fillRecentMenu :: Env -> [FilePath] -> IO ()
fillRecentMenu env paths = onMain $ do
  let section = envRecentSection env
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
    Gio.menuAppendSection inner (Nothing :: Maybe Text) forgetting
    Gio.menuAppendSubmenu section (Just "Open _Recent") inner

-- | Open a cell in the program the desktop opens text files with.
openWithDesktop :: Env -> FilePath -> IO ()
openWithDesktop env path = onMain $ do
  window <- readIORef (envWindow env)
  file <- Gio.fileNewForPath path
  launcher <- Gtk.fileLauncherNew (Just file)
  Gtk.fileLauncherLaunch launcher window (Nothing :: Maybe Gio.Cancellable) $ Just $
    \_ result -> do
      outcome <- try (Gtk.fileLauncherLaunchFinish launcher result)
      case outcome :: Either SomeException () of
        Right () -> pure ()
        Left _ -> post env (Toast (T.pack ("Could not open " ++ takeFileName path)))

--
-- The folder on disk
--

-- | Watch these folders, and drop whatever was being watched before.
watchPathsFor :: Env -> [FilePath] -> IO ()
watchPathsFor env paths = do
  unwatchAll env
  watcher <- watchPaths paths (post env DiskChanged)
  writeIORef (envWatcher env) (Just watcher)

unwatchAll :: Env -> IO ()
unwatchAll env = do
  previous <- readIORef (envWatcher env)
  forM_ previous unwatch
  writeIORef (envWatcher env) Nothing

--
-- The Blueprint file
--

object :: GObject o => Gtk.Builder -> Text -> (ManagedPtr o -> o) -> IO o
object builder name constructor = do
  found <- Gtk.builderGetObject builder name
  case found of
    Nothing -> fail ("the .ui file has no " ++ T.unpack name)
    Just this -> unsafeCastTo constructor this

optionalObject :: GObject o => Gtk.Builder -> Text -> (ManagedPtr o -> o) -> IO (Maybe o)
optionalObject builder name constructor = do
  found <- Gtk.builderGetObject builder name
  case found of
    Nothing -> pure Nothing
    Just this -> Just <$> unsafeCastTo constructor this
