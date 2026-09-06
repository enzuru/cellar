{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Noticing that the workbook on disk has changed.
--
-- A workbook is a folder of ordinary files, so anything at all can change it:
-- an editor, a @git checkout@, another copy of Cellar.  This watches the folder
-- and says when something did.
--
-- It says only that, never what: the caller re-reads the workbook and compares.
-- That sounds wasteful and is not -- a workbook is a few dozen small files --
-- and it buys the property that makes this safe, which is that Cellar's own
-- writes are indistinguishable from anyone else's.  Our writes land, the
-- watcher fires, the caller finds the disk already says what it already said,
-- and nothing happens.  There is no need to remember which files we wrote, and
-- so no way to get that bookkeeping wrong.
--
-- What is watched is a list of paths rather than one folder, because a workbook
-- is several sheets and a sheet is a folder and a file.  A path that is not
-- there yet is watched as a file, which is how a workbook notices one being
-- created.
module Cellar.Watch
  ( Watcher
  , watchPaths
  , unwatch
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, void)
import Data.IORef
import Data.Maybe (catMaybes)
import Data.Word (Word32)

import Data.GI.Base
import qualified GI.GLib as GLib
import qualified GI.Gio as Gio

-- | How long to wait for a burst of changes to finish before believing it.
-- Writing a file is several events, and a @git checkout@ of a workbook is
-- several files; both should cost one re-read, not a dozen.
settleMilliseconds :: Word32
settleMilliseconds = 250

data Watcher = Watcher
  { watcherMonitors :: [Gio.FileMonitor]
  , watcherPending :: IORef (Maybe GLib.Source)
  }

-- | Watch every path and run the action once each time they settle.
--
-- Answers with a watcher even when nothing could be watched, which is not
-- fatal: it leaves the workbook working exactly as it did before, only without
-- noticing edits made behind its back.
watchPaths :: [FilePath] -> IO () -> IO Watcher
watchPaths paths onChange = do
  pending <- newIORef Nothing
  let watcher = Watcher [] pending
  monitors <- forM paths $ \path -> monitorPath path (settle watcher onChange)
  pure watcher { watcherMonitors = catMaybes monitors }

-- | Monitor one path, whether it is a folder of cells or a single file.
monitorPath :: FilePath -> IO () -> IO (Maybe Gio.FileMonitor)
monitorPath path onEvent = do
  outcome <- try $ do
    file <- Gio.fileNewForPath path
    kind <- Gio.fileQueryFileType file [Gio.FileQueryInfoFlagsNone]
      (Nothing :: Maybe Gio.Cancellable)
    monitor <- case kind of
      Gio.FileTypeDirectory ->
        Gio.fileMonitorDirectory file [Gio.FileMonitorFlagsNone]
          (Nothing :: Maybe Gio.Cancellable)
      _ -> Gio.fileMonitorFile file [Gio.FileMonitorFlagsNone]
             (Nothing :: Maybe Gio.Cancellable)
    -- The signal carries which file changed and how, and we want neither: the
    -- caller re-reads everything regardless, so one code path covers a created
    -- file, a written one and a deleted one alike.
    _ <- on monitor #changed $ \_ _ _ -> onEvent
    pure monitor
  pure $ case (outcome :: Either SomeException Gio.FileMonitor) of
    Left _ -> Nothing
    Right monitor -> Just monitor

-- | Restart the quiet period.  The action runs once the changes stop coming.
settle :: Watcher -> IO () -> IO ()
settle watcher onChange = do
  previous <- readIORef (watcherPending watcher)
  forM_ previous GLib.sourceDestroy
  source <- GLib.timeoutSourceNew settleMilliseconds
  void $ GLib.sourceSetCallback source $ do
    writeIORef (watcherPending watcher) Nothing
    onChange
    -- False: a one-shot timeout, not a heartbeat.
    pure False
  _ <- GLib.sourceAttach source (Nothing :: Maybe GLib.MainContext)
  writeIORef (watcherPending watcher) (Just source)

-- | Stop watching.  Safe to call twice.
unwatch :: Watcher -> IO ()
unwatch watcher = do
  pending <- readIORef (watcherPending watcher)
  forM_ pending GLib.sourceDestroy
  writeIORef (watcherPending watcher) Nothing
  forM_ (watcherMonitors watcher) $ \monitor ->
    void (try (Gio.fileMonitorCancel monitor) :: IO (Either SomeException Bool))
