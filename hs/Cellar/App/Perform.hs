{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | How the window does what "Cellar.App.Update" asked for.
--
-- The update answers with values.  This is the only part of the window that
-- turns one into something happening, and the only part that holds the
-- 'Env' -- the kernel, the builder, the toasts, the gestures.
--
-- Nothing here decides anything.  Every effect either does exactly what it
-- says or, when the disk or the store refuses, answers with a 'Toast', which
-- the update already knows what to do with.  That is the whole of the policy
-- on this side of the line.
module Cellar.App.Perform
  ( perform
  , settleMicroseconds
  ) where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesPathExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName)
import System.Process (callProcess)

import qualified GI.GLib as GLib

import Cellar.App.Effect
import Cellar.App.Env
import Cellar.App.Event
import Cellar.Client (markReady, restartKernel)
import Cellar.Config
import Cellar.Editor (Preview (..))
import Cellar.External
import Cellar.Ref (Ref, refName)
import Cellar.Store

-- | How long a burst of the same request has to stop for before it is obeyed.
-- Long enough to swallow a drag, short enough that letting go feels immediate.
settleMicroseconds :: Int
settleMicroseconds = 150000

-- | Do one of the update's effects, and hand back whatever it answers with.
perform :: Env -> Effect -> IO (Maybe Event)
perform env = \case
  Emit event -> pure (Just event)

  --
  -- The kernel
  --

  Request requests -> nothing (asking env requests)
  RestartKernel -> nothing (restartKernel (envKernel env) >> forgetTags env)
  MarkReady -> nothing (markReady (envKernel env))

  --
  -- The window
  --

  Notify message -> nothing (notify env message)
  ShowPalette css -> nothing (showPalette env css)
  ShowDrag tab drag -> nothing (showDrag env tab drag)
  -- The grid takes the keyboard when a workbook opens.  There is no way to
  -- say in markup that a widget wants the focus, so this is a gap in the
  -- declarative layer rather than a thing Cellar wants to do by hand.
  FocusGrid -> pure Nothing
  FillRecent paths -> nothing (fillRecentMenu env paths)

  --
  -- The windows of their own
  --

  AskSheetName tab suggestion heading ->
    nothing (askSheetName env tab suggestion heading)
  AskNewWorkbook suggestion location copying ->
    nothing (askNewWorkbook env suggestion location copying)
  AskToDeleteSheet tab name -> nothing (askToDelete env tab name)
  ChooseFolder -> nothing (chooseFolder env)
  OpenPreferences config -> nothing (openPreferences env config)
  ShowShortcuts -> nothing (showShortcuts env)
  ShowAbout -> nothing (showAbout env)
  AskAboutKernel -> nothing (askAboutTheKernel env)
  NeverMindKernel -> nothing (neverMindTheKernel env)
  OpenCellEditor tab r source -> nothing (openEditor env tab r source)
  OpenCellFile folder r config -> openCellFile env folder r config
  ShowPreview mine text isError ->
    nothing (showPreview env mine (Preview text isError))

  --
  -- The folder on disk
  --

  SaveSheet folder sheet -> do
    outcome <- try (saveSheet folder sheet)
    pure $ case outcome :: Either SomeException () of
      Left _ -> Just (Toast "Could not save the sheet")
      Right () -> Nothing

  SaveCell folder name source -> nothing (quietly (saveCell folder name source))
  SaveConfig config -> nothing (quietly (saveConfig config))
  Watch open -> do
    paths <- workbookWatchPaths open
    pure (Just (Watching paths))
  SetActiveSheet open name -> nothing (quietly (setWorkbookActive open name))
  SetSheetOrder open names -> nothing (quietly (setWorkbookOrder open names))

  ReadWorkbookAt path how -> readWorkbookAt path how
  ReadSheetsOf open -> readSheetsOf open
  MakeWorkbook path wantsGit -> makeWorkbook env path wantsGit False (pure ())
  MakeScratch -> makeScratch
  CopyWorkbook path sheets showing wantsGit ->
    makeWorkbook env path wantsGit True (copyInto path sheets showing)

  AddSheetFolder open name -> stored (addWorkbookSheet open name) $
    \(moved, added) -> SheetAdded moved added
  RenameSheetFolder open tab old new ->
    stored (renameWorkbookSheet open old new) $
      \(moved, renamed) -> SheetRenamed moved tab old renamed
  RemoveSheetFolder open _ name ->
    stored (removeWorkbookSheet open name) (const (SheetRemoved open name))

-- | An effect with nothing to say afterwards.
nothing :: IO () -> IO (Maybe Event)
nothing action = action >> pure Nothing

-- | Something the store may refuse.  A refusal is a toast and nothing else.
stored :: forall a. IO a -> (a -> Event) -> IO (Maybe Event)
stored action said = do
  outcome <- try action
  pure $ case outcome :: Either StoreError a of
    Left (StoreError why) -> Just (Toast (T.pack why))
    Right got -> Just (said got)

quietly :: IO a -> IO ()
quietly action = do
  outcome <- try (void action)
  either (\(_ :: SomeException) -> pure ()) pure outcome



--
-- Reading the folder
--

-- | Read a workbook off the disk, and say what to do with it.
readWorkbookAt :: FilePath -> Opening -> IO (Maybe Event)
readWorkbookAt path how = resolveWorkbook path >>= \case
  Nothing -> pure (Just (WorkbookRefused path))
  Just open -> do
    names <- workbookSheetNames open
    showing <- workbookActiveSheet open
    sheets <- forM names $ \name ->
      (,) name <$> readSheetOrEmpty (workbookSheetDirectory open name)
    pure (Just (WorkbookRead open how sheets showing))

-- | Read every sheet of a workbook that is already open, for the watcher.
--
-- Whether this is a change at all is the update's to work out, since only the
-- update knows what the window is showing.  Answers with nothing when the
-- folder has gone: a workbook that is not there any more is not a change to
-- report, and the window keeps showing what it was showing.
readSheetsOf :: Workbook -> IO (Maybe Event)
readSheetsOf open = do
  stillThere <- isWorkbookDirectory (workbookRoot open)
  if not stillThere then pure Nothing else do
    names <- workbookSheetNames open
    sheets <- forM names $ \name -> do
      sheet <- readSheetOrEmpty (workbookSheetDirectory open name)
      pure (name, sheet)
    pure (Just (SheetsOnDisk sheets))

readSheetOrEmpty :: FilePath -> IO Sheet
readSheetOrEmpty folder = do
  isSheet <- isSheetDirectory folder
  if not isSheet then pure emptySheet else do
    outcome <- try (readSheet folder)
    pure (either (\(_ :: SomeException) -> emptySheet) id outcome)

emptySheet :: Sheet
emptySheet = Sheet [] 100 26 []

--
-- Making a workbook
--

-- | Make a workbook at this path, and say so.
makeWorkbook :: Env -> FilePath -> Bool -> Bool -> IO () -> IO (Maybe Event)
makeWorkbook env path wantsGit copying build = do
  outcome <- try build'
  case outcome :: Either SomeException () of
    Left _ -> pure (Just (Toast (T.pack ("Could not " ++ verb ++ takeFileName path))))
    Right () -> do
      when wantsGit (gitInit env path)
      notify env (T.pack (done ++ takeFileName path))
      pure (Just (Act (OpenRecentAt path)))
  where
    build' = if copying then build else createWorkbook path "Sheet 1"
    verb = if copying then "copy to " else "create "
    done = if copying then "Now editing " else "Created "

-- | Write these sheets into a workbook of their own.
copyInto :: FilePath -> [(String, Sheet)] -> Maybe String -> IO ()
copyInto directory sheets showing = case sheets of
  [] -> throwIO (StoreError "There is nothing to copy")
  ((firstName, _) : _) -> do
    createWorkbook directory firstName
    fresh <- resolveWorkbook directory >>= \case
      Just open -> pure open
      Nothing -> throwIO (StoreError ("Could not open " ++ directory))
    forM_ sheets $ \(name, _) ->
      unless (name == firstName) (void (addWorkbookSheet fresh name))
    forM_ sheets $ \(name, sheet) ->
      saveSheet (workbookSheetDirectory fresh name) sheet
    writeWorkbookIndex directory (map fst sheets) showing

-- | Make a git repository of the workbook.  Around the workbook rather than
-- around any one sheet, which is the whole reason a workbook exists.
gitInit :: Env -> FilePath -> IO ()
gitInit env directory = do
  outcome <- try (callProcess "git" ["init", "--quiet", directory])
  case outcome :: Either SomeException () of
    Left _ -> notify env "The folder was made, but git could not be run"
    Right () -> pure ()

-- | A workbook to think in, out of the way under the data directory.
makeScratch :: IO (Maybe Event)
makeScratch = do
  directory <- scratchLocation
  outcome <- try (createWorkbook directory "Sheet 1")
  pure $ case outcome :: Either SomeException () of
    Left _ -> Just (Toast "Could not make a scratch workbook")
    Right () -> Just (ScratchMade directory)

scratchLocation :: IO FilePath
scratchLocation = do
  home <- fromMaybe "." <$> lookupEnv "HOME"
  xdg <- lookupEnv "XDG_DATA_HOME"
  let base = fromMaybe (home </> ".local" </> "share") xdg
      scratches = base </> "cellar" </> "scratch"
  createDirectoryIfMissing True scratches
  -- Named for the moment it was started, so two of them never collide.
  stamp <- timestamp
  firstFree ((scratches </> stamp ++ ".cellar")
             : [ scratches </> (stamp ++ "-" ++ show n) ++ ".cellar" | n <- [1 :: Int ..] ])
  where
    firstFree [] = pure "."
    firstFree (candidate : more) = do
      taken <- doesPathExist candidate
      if taken then firstFree more else pure candidate

timestamp :: IO String
timestamp = GLib.dateTimeNewNowLocal >>= \case
  Nothing -> pure "sheet"
  Just moment -> do
    formatted <- GLib.dateTimeFormat moment "%Y-%m-%d-%H%M%S"
    pure (maybe "sheet" T.unpack formatted)

--
-- Handing a cell to another program
--

-- | An empty cell has no file, and no program can be handed a path that is not
-- there, so opening one is what brings its file into being.
openCellFile :: Env -> FilePath -> Ref -> Config -> IO (Maybe Event)
openCellFile env folder r config = do
  made <- try (touchCell folder (refName r))
  case made :: Either SomeException FilePath of
    Left _ -> pure (Just (Toast (T.pack ("Could not write a file for " ++ refName r))))
    Right file -> do
      command <- effectiveEditorCommand config
      case command of
        Nothing -> nothing (openWithDesktop env file)
        Just external -> do
          started <- openExternalEditor external folder r
          pure . Just . Toast $ case started of
            Just program -> T.pack ("Editing " ++ refName r
                                    ++ " in " ++ takeFileName program)
            Nothing -> "Could not start the editor in the preferences"
