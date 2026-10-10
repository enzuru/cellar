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

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesPathExist, getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName)

import qualified GI.GLib as GLib

import Cellar.App.Effect
import Cellar.App.Env
import Cellar.App.Event
import Cellar.App.State (TabId)
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
  OpenCellFile tab r source config -> openCellFile env tab r source config
  ShowPreview mine text isError ->
    nothing (showPreview env mine (Preview text isError))

  --
  -- The folder on disk
  --

  SaveWorkbook open book -> do
    outcome <- try (writeBook open book)
    case outcome :: Either SomeException () of
      Left _ -> pure (Just (Toast "Could not save the workbook"))
      Right () -> do
        -- What the file says now, so that the watcher can tell this write
        -- from somebody else's edit.
        writeIORef (envWritten env) (Just (T.pack (bookText book)))
        pure Nothing

  SaveConfig config -> nothing (quietly (saveConfig config))
  -- One file to watch now, which is the whole of it.
  Watch open -> pure (Just (Watching [workbookRoot open]))

  ReadWorkbookAt path how -> readWorkbookAt path how
  ReadWorkbookAgain open -> readWorkbookAgain env open
  MakeWorkbook path ->
    makeWorkbook env path False (void (createWorkbook path "Sheet 1"))
  MakeScratch -> makeScratch
  CopyWorkbook path book ->
    makeWorkbook env path True (writeBook (Workbook path) book)

-- | An effect with nothing to say afterwards.
nothing :: IO () -> IO (Maybe Event)
nothing action = action >> pure Nothing

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
    outcome <- try (readBook open)
    pure $ case outcome :: Either StoreError Book of
      Left _ -> Just (WorkbookRefused path)
      Right book -> Just (WorkbookRead open how (bookSheets book) (bookActive book))

-- | Read the open workbook again, because its file changed under us.
--
-- Answers with nothing at all when the file has gone: a workbook that is not
-- there any more is not a change to report, and the window keeps showing what
-- it was showing.
readWorkbookAgain :: Env -> Workbook -> IO (Maybe Event)
readWorkbookAgain env open = do
  outcome <- try (TIO.readFile (workbookRoot open))
  case outcome :: Either SomeException Text of
    Left _ -> pure Nothing
    Right text -> do
      ours <- readIORef (envWritten env)
      -- Our own write, which the window has already applied.  Reading it back
      -- would be at best wasted work and at worst a stale read landing on top
      -- of an edit that is still in flight.
      if Just text == ours then pure Nothing else
        pure $ case parseBook text of
          Left _ -> Nothing
          Right book -> Just (SheetsOnDisk (bookSheets book))

--
-- Making a workbook
--

-- | Make a workbook at this path, and say so.
makeWorkbook :: Env -> FilePath -> Bool -> IO () -> IO (Maybe Event)
makeWorkbook env path copying build = do
  outcome <- try build'
  case outcome :: Either SomeException () of
    Left _ -> pure (Just (Toast (T.pack ("Could not " ++ verb ++ takeFileName path))))
    Right () -> do
      notify env (T.pack (done ++ takeFileName path))
      pure (Just (Act (OpenRecentAt path)))
  where
    build' = build
    verb = if copying then "copy to " else "create "
    done = if copying then "Now editing " else "Created "

-- | A workbook to think in, out of the way under the data directory.
makeScratch :: IO (Maybe Event)
makeScratch = do
  directory <- scratchLocation
  outcome <- try (createWorkbook directory "Sheet 1")
  pure $ case outcome :: Either SomeException Workbook of
    Left _ -> Just (Toast "Could not make a scratch workbook")
    Right _ -> Just (ScratchMade directory)

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
-- | Hand a cell to the editor in the preferences.
--
-- A cell has no file of its own now that a workbook is one file, so it goes
-- out to a file of its own under the system's temporary directory.  Whatever
-- is in that file when the editor is done with it comes back as an ordinary
-- edit, so the workbook is written by the same path as every other edit.
--
-- The file is watched rather than waited on, because an editor may be a
-- window that was already open and may never exit.
openCellFile :: Env -> TabId -> Ref -> String -> Config -> IO (Maybe Event)
openCellFile env tab r source config = do
  made <- try $ do
    directory <- (</> "cellar") <$> getTemporaryDirectory
    createDirectoryIfMissing True directory
    let file = directory </> (refName r ++ ".scm")
    writeFile file (source ++ if null source || last source == '\n' then "" else "\n")
    pure file
  case made :: Either SomeException FilePath of
    Left _ -> pure (Just (Toast (T.pack ("Could not write a file for " ++ refName r))))
    Right file -> do
      watchCellFile env tab r file
      command <- effectiveEditorCommand config
      case command of
        Nothing -> nothing (openWithDesktop env file)
        Just external -> do
          started <- openExternalEditor external file
          pure . Just . Toast $ case started of
            Just program -> T.pack ("Editing " ++ refName r
                                    ++ " in " ++ takeFileName program)
            Nothing -> "Could not start the editor in the preferences"
