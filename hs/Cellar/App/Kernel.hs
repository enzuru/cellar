{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Talking to the kernel.
--
-- Everything that crosses the pipe, and everything that happens when an answer
-- comes back: snapshots turning into views, an edit reaching a file, and the
-- watchdog that notices a cell which is never going to finish.
--
module Cellar.App.Kernel where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, void, when)
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.FilePath (takeFileName)

import Data.GI.Base
import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import Cellar.Client
import Cellar.External (openExternalEditor)
import Cellar.Config
import Cellar.Editor
import Cellar.Grid
import Cellar.Ref
import Cellar.Sexp
import Cellar.Store
import Cellar.View
import Cellar.App.Types

ask :: App -> String -> [Sexp] -> (Sexp -> IO ()) -> IO ()
ask app op arguments continue =
  call (appKernel app) op arguments continue (notify app . T.pack)

-- | Ask the kernel for nothing in particular, so that the first answer marks
-- the end of starting up rather than the end of somebody's first edit.

warmUp :: App -> IO ()
warmUp app = do
  writeIORef (appKernelAnswered app) False
  call (appKernel app) "ping" []
    (\_ -> do writeIORef (appKernelAnswered app) True
              -- Anything sent while it was starting has been waiting on the
              -- start rather than on itself.
              markReady (appKernel app))
    (\_ -> pure ())


installPump :: App -> IO ()
installPump app = do
  _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT pumpMilliseconds $ do
    void (pump (appKernel app))
    pure True
  _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 500 $ do
    mindTheKernel app
    pure True
  pure ()

-- | Notice a cell that is not going to finish.
--
-- There is no way to tell one apart from a cell that is merely slow -- that is
-- the halting problem, and Cellar is not going to solve it on a timer -- so
-- this does not decide anything.  It waits until a request has gone unanswered
-- for longer than anybody would expect, and then asks.

mindTheKernel :: App -> IO ()
mindTheKernel app = do
  alive <- kernelAlive (appKernel app)
  answered <- readIORef (appKernelAnswered app)
  waiting <- outstanding (appKernel app)
  asking <- readIORef (appAskingAboutKernel app)
  onPurpose <- readIORef (appWaitingOnPurpose app)
  isStalled <- stalled (appKernel app) patienceSeconds
  if not alive || not answered
    then pure ()
    else if waiting == 0
      then do
        neverMindTheKernel app
        writeIORef (appWaitingOnPurpose app) False
      else when (not asking && not onPurpose && isStalled) (askAboutTheKernel app)


askAboutTheKernel :: App -> IO ()
askAboutTheKernel app = do
  dialog <- new Adw.AlertDialog
    [ #heading := "A cell is taking a long time"
    , #body := T.intercalate "\n\n"
        [ "Cellar is still waiting for the kernel to finish working something \
          \out. A cell can be given an expression that never finishes — \
          \(let loop () (loop)) — and this is what that looks like."
        , "Stopping restarts the kernel and reloads the sheets as they stand \
          \on disk. The edit that caused it was never written, so stopping \
          \loses nothing but the edit itself." ] ]
  Adw.alertDialogAddResponse dialog "wait" "Keep Waiting"
  Adw.alertDialogAddResponse dialog "stop" "Stop It"
  Adw.alertDialogSetResponseAppearance dialog "stop" Adw.ResponseAppearanceDestructive
  Adw.alertDialogSetDefaultResponse dialog (Just "wait")
  Adw.alertDialogSetCloseResponse dialog "wait"
  _ <- on dialog #response $ \response -> do
    writeIORef (appAskingAboutKernel app) False
    writeIORef (appStallDialog app) Nothing
    if response == "stop"
      then restartTheKernel app
      else writeIORef (appWaitingOnPurpose app) True
  writeIORef (appAskingAboutKernel app) True
  writeIORef (appStallDialog app) (Just dialog)
  Adw.dialogPresent dialog (Just (appWindow app))

-- | Take the question back down.  The cell finished while we were asking about
-- it, and a dialog still on screen about a problem that has gone away is worse
-- than never having asked.

neverMindTheKernel :: App -> IO ()
neverMindTheKernel app = do
  dialog <- readIORef (appStallDialog app)
  forM_ dialog $ \d -> do
    writeIORef (appStallDialog app) Nothing
    writeIORef (appAskingAboutKernel app) False
    void (try (Adw.dialogClose d) :: IO (Either SomeException Bool))

-- | Kill the kernel and hand the sheets to a new one.
--
-- Everything the old kernel knew is gone, so every open sheet has to be given
-- again.  What is given is what the shell has -- which is what is on disk,
-- since the edit that hung it was never answered and so was never written.

restartTheKernel :: App -> IO ()
restartTheKernel app = do
  outcome <- try $ do
    restartKernel (appKernel app)
    writeIORef (appWaitingOnPurpose app) False
    warmUp app
    tabs <- readIORef (appTabs app)
    forM_ tabs (reopenInKernel app)
  case outcome :: Either SomeException () of
    Left _ -> notify app "Could not restart the kernel"
    Right () ->
      notify app "The kernel was restarted; the sheets are as they were on disk"


reopenInKernel :: App -> Tab -> IO ()
reopenInKernel app tab = do
  view <- gridCurrentView (tabGrid tab)
  sources <- readIORef (tabSources tab)
  ask app "open"
    [ Num (fromIntegral (tabId tab))
    , Num (fromIntegral (max defaultRows (viewRows view)))
    , Num (fromIntegral (max defaultColumns (viewColumns view)))
    , sourcesSexp sources ]
    (takeSnapshot app tab)


sourcesSexp :: [(String, String)] -> Sexp
sourcesSexp = list . map (\(name, source) -> Pair (Str name) (Str source))

-- | Take what the kernel just said about a sheet and put it on screen.
--
-- A snapshot carries sources only when the kernel rewrote them -- a move or an
-- insert -- because the shell already has the ones it sent.

takeSnapshot :: App -> Tab -> Sexp -> IO ()
takeSnapshot app tab payload = do
  forM_ (lookupKey "sources" payload) $ \sources ->
    writeIORef (tabSources tab) (readSources sources)
  sources <- readIORef (tabSources tab)
  gridSetView (tabGrid tab) (viewFromSnapshot payload sources)
  current <- currentTab app
  when (fmap tabId current == Just (tabId tab)) $
    gridActiveRef (tabGrid tab) >>= showSelection app


readSources :: Sexp -> [(String, String)]
readSources value = case toList value of
  Nothing -> []
  Just entries -> [ (name, source) | Pair (Str name) (Str source) <- entries ]

-- Tabs

setCell :: App -> Tab -> Ref -> String -> IO ()
setCell app tab r text = do
  let name = refName r
  ask app "set-cell"
    [Num (fromIntegral (tabId tab)), Str name, Str text]
    (\payload -> do
       let source = lookupKey "source" payload >>= asString
       modifyIORef' (tabSources tab) (setSource name source)
       persistCell app tab name source
       takeSnapshot app tab payload)


setSource :: String -> Maybe String -> [(String, String)] -> [(String, String)]
setSource name source sources =
  let without = filter ((/= name) . fst) sources
  in case source of
       Nothing -> without
       Just text -> without ++ [(name, text)]


-- | Open a cell in Cellar's own editor.  This is what the pencil and Enter do,
-- and all they do: choosing another program is the folder beside it, which is
-- a button of its own rather than a preference that changes what this one
-- means.
editCell :: App -> Tab -> Ref -> IO ()
editCell app tab r = do
  sources <- readIORef (tabSources tab)
  openCellEditor (appUiDirectory app) (appWindow app) r (lookup (refName r) sources)
    (\text continue ->
       ask app "preview"
         [Num (fromIntegral (tabId tab)), Str (refName r), Str text]
         (\payload -> continue Preview
            { previewText = fromMaybe "" (lookupKey "written" payload >>= asString)
            , previewIsError = maybe False asBool (lookupKey "error" payload)
            }))
    (\text -> setCell app tab r text)


-- | Hand a cell to another program: the command in the preferences when there
-- is one, and otherwise whatever the desktop opens text files with.
--
-- Nothing is read back either way.  The cell is a file in the sheet folder and
-- that folder is watched, so saving in the other program is what reaches the
-- grid -- with it still open, as often as you like.
openCellExternally :: App -> Tab -> Ref -> IO ()
openCellExternally app tab r = do
  config <- readIORef (appConfig app)
  command <- effectiveEditorCommand config
  directory <- tabDirectory app tab
  forM_ directory $ \path -> do
    -- An empty cell has no file, and no program can be handed a path that is
    -- not there, so opening one is what brings its file into being.
    made <- try (touchCell path (refName r))
    case made :: Either SomeException FilePath of
      Left _ -> notify app (T.pack ("Could not write a file for " ++ refName r))
      Right file -> case command of
        Just external -> do
          started <- openExternalEditor external path r
          case started of
            Just program ->
              notify app (T.pack ("Editing " ++ refName r ++ " in " ++ takeFileName program))
            Nothing -> notify app "Could not start the editor in the preferences"
        Nothing -> openInDefaultTextEditor app file


-- | Open a cell in the program the desktop opens text files with.
--
-- Text files, not Scheme files: asking for @text/x-scheme@ would land the cell
-- in whatever is registered for source code, which on a developer's machine is
-- an IDE, and the button says text editor.  When nothing is registered for
-- plain text either, GTK's file launcher takes over and puts the desktop's own
-- \"Open With\" chooser up rather than a toast saying no.
openInDefaultTextEditor :: App -> FilePath -> IO ()
openInDefaultTextEditor app file = do
  gioFile <- Gio.fileNewForPath file
  editor <- Gio.appInfoGetDefaultForType "text/plain" False
  case editor of
    Nothing -> askTheDesktop app file gioFile
    Just info -> do
      launched <- try (Gio.appInfoLaunch info [gioFile]
                         (Nothing :: Maybe Gio.AppLaunchContext))
      case launched :: Either SomeException () of
        Right () -> do
          name <- Gio.appInfoGetDisplayName info
          notify app (T.pack ("Opened " ++ takeFileName file ++ " in ") <> name)
        Left _ -> askTheDesktop app file gioFile


-- | The fallback: let GTK ask, which is the \"Open With\" chooser on a desktop
-- with nothing registered for the file, and the portal's version of it inside
-- a sandbox.
askTheDesktop :: App -> FilePath -> Gio.File -> IO ()
askTheDesktop app file gioFile = do
  launcher <- Gtk.fileLauncherNew (Just gioFile)
  Gtk.fileLauncherLaunch launcher (Just (appWindow app))
    (Nothing :: Maybe Gio.Cancellable) $ Just $ \_ result -> do
      -- A failure here is a dismissed chooser as often as it is a desktop with
      -- nothing to open the file with, so the toast says what happened rather
      -- than guessing why.
      outcome <- try (Gtk.fileLauncherLaunchFinish launcher result)
      case outcome :: Either SomeException () of
        Right () -> notify app (T.pack ("Opened " ++ takeFileName file))
        Left _ -> notify app (T.pack ("Nothing opened " ++ takeFileName file))


-- Catching up with the disk

-- | Watch the open workbook, and only it.

persistCell :: App -> Tab -> String -> Maybe String -> IO ()
persistCell app tab name source = do
  directory <- tabDirectory app tab
  forM_ directory $ \path -> reportFailure app ("save " ++ name) (saveCell path name source)
