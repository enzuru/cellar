{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The application: what starts, what it is called, and what the keys do.
--
-- Cellar's window is a function of one value.  This module makes the value,
-- the handles the window needs that a value cannot hold, and the threads that
-- bring it news -- the kernel, the clock, and the actions -- and hands the lot
-- to gi-gtk4-declarative's application loop, which draws the window and keeps
-- it in step.
module Cellar.App (runApp, applicationId, withApp) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (atomically)
import Control.Monad (forM_, forever, unless, void)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import Data.GI.Base
import qualified GI.Adw as Adw
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified GI.GtkSource as Source

import qualified GI.Gtk.Declarative.App.Simple as Simple
import GI.Gtk.Declarative.App.Simple (App (..), startInApplication)
import Pipes (Producer, liftIO, yield)
import Pipes.Concurrent (fromInput, send, spawn, unbounded)

import Cellar.App.Env
import Cellar.App.Event
import Cellar.App.State
import qualified Cellar.App.Update as Update
import Cellar.App.View
import Cellar.Client
import Cellar.Config
import Cellar.Grid.Gestures (gestureHandlers)
import Cellar.Grid.Model (GridHandlers (..))
import Cellar.Ref

applicationId :: Text
applicationId = "dev.enzuru.Cellar"

runApp :: [String] -> IO ()
runApp arguments = do
  application <- new Adw.Application [#applicationId := applicationId]
  _ <- on application #activate
         (void (startWindow application (listToMaybe arguments)))
  void (Gio.applicationRun application Nothing)

-- | Start the same window from a program rather than from a person.
--
-- The application is deliberately not unique, so that a test does not hand its
-- work to the Cellar you already have open.  The body is handed the way in:
-- post an event and the window answers it as though somebody had done it.
withApp :: ((Event -> IO ()) -> IO ()) -> IO ()
withApp body = do
  application <- new Adw.Application
    [ #applicationId := applicationId
    , #flags := [Gio.ApplicationFlagsNonUnique] ]
  _ <- on application #activate $ do
    poster <- startWindow application Nothing
    body poster
  void (Gio.applicationRun application (Just ["cellar-window-test"]))

-- | Build everything the window needs and set it going.  Answers with the way
-- to post an event to it.
startWindow :: Adw.Application -> Maybe FilePath -> IO (Event -> IO ())
startWindow application opening = do
  -- GtkSourceView registers its types here; without this the cell editor
  -- cannot be built from editor.ui.
  Source.init
  uiDirectory <- findUiDirectory
  -- The Blueprint file is down to the menus and the dialogs: the menus name
  -- application actions, which is what lets a keystroke and a menu item mean
  -- the same thing, and a dialog is asked once and answered once.  Neither is
  -- a function of the state, so neither is in the view.
  builder <- Gtk.builderNewFromFile (uiDirectory </> "cellar.ui")
  primaryMenu <- object builder "primary_menu" Gio.MenuModel

  (output, input) <- spawn unbounded
  let poster event = void (atomically (send output event))

  (program, arguments) <- kernelCommandLine
  kernel <- startKernel program arguments
  env <- newEnv kernel poster uiDirectory builder

  config <- loadConfig
  home <- fromMaybe "." <$> lookupEnv "HOME"
  installCss uiDirectory
  installIcons
  installActions env application
  fillRecentMenu env (recentWorkbooks config)

  let viewEnv = ViewEnv
        { viewPrimaryMenu = primaryMenu
        , viewGridHandlers = handlersFor env
        , viewTookWindow = \window -> do
            Gtk.windowSetIconName window (Just applicationId)
            tookWindow env window
        , viewTookToasts = tookToasts env
        }
      simple = App
        { view = windowView viewEnv
        , Simple.update = Update.update env
        , inputs = [fromInput input, kernelReplies env, clock]
        , initialState = newState config home
        }

  -- Ask the kernel for nothing in particular, so that the first answer marks
  -- the end of starting up rather than the end of somebody's first edit.
  asking env [("ping", [], Pinged)]
  forM_ opening $ \path -> poster (Act (OpenRecentAt path))

  -- The loop runs in a thread of its own, and holds the application while it
  -- gets going: an application quits as soon as `activate' returns holding no
  -- window, and the window is built on the main loop a moment later.
  loop <- startInApplication application simple
  void $ Async.async $ do
    _ <- Async.waitCatch loop
    stopKernel kernel
    unwatchAll env
  pure poster

-- | What the grid's widgets are handed to when they are built.  The gestures
-- for a sheet are made the first time one of its widgets is.
handlersFor :: Env -> TabId -> GridHandlers
handlersFor env tab = GridHandlers
  { onCellBuilt = \label -> withGestures (\hs -> onCellBuilt hs label)
  , onGutterBuilt = \label -> withGestures (\hs -> onGutterBuilt hs label)
  , onViewBuilt = \view' -> withGestures (\hs -> onViewBuilt hs view')
  }
  where withGestures use = gesturesFor env tab >>= use . gestureHandlers

--
-- What brings the window news
--

-- | Whatever the kernel says, as it says it.  The thread blocks on the pipe,
-- so nothing here is polled.
kernelReplies :: Env -> Producer Event IO ()
kernelReplies env = forever $ do
  replies <- liftIO (awaitReplies (envKernel env))
  forM_ replies $ \reply -> case reply of
    Answered requestId payload -> do
      -- The cell editor asks for previews of its own and is handed them
      -- directly; it is the one part of Cellar that is still a window of its
      -- own rather than part of this one.
      mine <- liftIO (answerEditor env requestId payload)
      unless mine $ do
        tag <- liftIO (tagOf env requestId)
        yield (KernelSaid tag payload)
    Refused requestId why -> do
      tag <- liftIO (tagOf env requestId)
      yield (KernelRefused tag why)

-- | Twice a second, so that a cell which will not finish is noticed.
clock :: Producer Event IO ()
clock = forever (liftIO (threadDelay 500000) >> yield Tick)

--
-- The actions
--

installActions :: Env -> Adw.Application -> IO ()
installActions env application = do
  let define name accelerators action = do
        simple <- Gio.simpleActionNew name Nothing
        _ <- on simple #activate (\_ -> post env (Act action))
        Gio.actionMapAddAction application simple
        unless (null accelerators) $
          Gtk.applicationSetAccelsForAction application ("app." <> name) accelerators

  define "new" ["<Control>n"] NewWorkbook
  define "new-scratch" ["<Control><Shift>n"] NewScratch
  define "open" ["<Control>o"] OpenWorkbook
  define "save" ["<Control>s"] SaveNothing
  define "copy-to" ["<Control><Shift>s"] CopyTo
  define "add-sheet" ["<Control>t"] AddSheet
  define "rename-sheet" ["<Control><Shift>r"] RenameSheet
  define "delete-sheet" ["<Control>w"] DeleteSheet
  define "next-sheet" ["<Control>Page_Down"] NextSheet
  define "previous-sheet" ["<Control>Page_Up"] PreviousSheet
  define "recalculate" ["<Control>r"] RecalculateSheet
  define "clear-cell" ["Delete"] ClearCell
  define "edit-cell" ["<Control>e"] EditCell
  -- The same letter, shifted: Ctrl+E edits the cell here, Ctrl+Shift+E edits
  -- it elsewhere.
  define "open-cell" ["<Control><Shift>e"] OpenCellElsewhere
  define "move-row-up" ["<Control><Shift>Up"] (MoveLine Row (-1))
  define "move-row-down" ["<Control><Shift>Down"] (MoveLine Row 1)
  define "move-column-left" ["<Control><Shift>Left"] (MoveLine Column (-1))
  define "move-column-right" ["<Control><Shift>Right"] (MoveLine Column 1)
  define "insert-row-before" ["<Control><Alt>Up"] (InsertLine Row True)
  define "insert-row-after" ["<Control><Alt>Down"] (InsertLine Row False)
  define "insert-column-before" ["<Control><Alt>Left"] (InsertLine Column True)
  define "insert-column-after" ["<Control><Alt>Right"] (InsertLine Column False)
  define "clear-recent" [] ClearRecent
  define "preferences" ["<Control>comma"] Preferences
  define "shortcuts" ["<Control>question"] Shortcuts
  define "about" [] About
  define "quit" ["<Control>q"] Quit

  -- A row on the start page and an item in the Open Recent submenu both fire
  -- this, with the workbook's folder as the target -- which is why it is
  -- defined by hand: it is the one action of Cellar's that takes one.
  openRecent <- Gio.simpleActionNew "open-recent" . Just =<< GLib.variantTypeNew "s"
  _ <- on openRecent #activate $ \parameter -> forM_ parameter $ \variant -> do
         wanted <- fromGVariant variant
         forM_ wanted $ \path -> post env (Act (OpenRecentAt (T.unpack path)))
  Gio.actionMapAddAction application openRecent

--
-- Where things are
--

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
