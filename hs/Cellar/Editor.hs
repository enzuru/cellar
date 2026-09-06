{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The cell editor.
--
-- Double-clicking a cell opens this: a GtkSourceView with Scheme highlighting
-- in an AdwDialog.  Whatever you type is the cell's source, and the Result line
-- underneath evaluates it as you type -- against the real sheet, but without
-- committing anything, so a half-written expression never corrupts the grid.
--
-- The evaluating is done by the kernel, so the result arrives some time after
-- the keystroke that asked for it.  Two things follow.  Typing is not asked
-- about until it pauses, because a request per keystroke would ask the kernel
-- to evaluate every prefix of what you are writing, nearly all of which are
-- half-written and none of which you wanted.  And an answer that arrives after
-- a newer one has been sent is thrown away, because a pipe makes no promise
-- about the order of things and the Result line showing an older answer than
-- the one already on screen would be worse than showing nothing.
module Cellar.Editor
  ( openCellEditor
  , Preview (..)
  ) where

import Control.Monad (forM_, void, when)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word32)
import qualified Data.Text as T

import Data.GI.Base
import qualified GI.Adw as Adw
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.GtkSource as Source

import Cellar.Ref

-- | What the kernel said about a half-written expression.
data Preview = Preview
  { previewText :: String
  , previewIsError :: Bool
  }

settleMilliseconds :: Word32
settleMilliseconds = 150

-- | Present the editor for a cell.
--
-- The preview action is handed the text and a procedure to give the answer to;
-- it is what talks to the kernel.  The apply action is called with the new
-- source when the edit is applied.
openCellEditor
  :: FilePath                                     -- ^ where the .ui files are
  -> Adw.ApplicationWindow
  -> Ref
  -> Maybe String                                 -- ^ the cell's current source
  -> (String -> (Preview -> IO ()) -> IO ())      -- ^ evaluate without keeping
  -> (String -> IO ())                            -- ^ apply
  -> IO ()
openCellEditor uiDirectory parent r source preview apply = do
  builder <- Gtk.builderNewFromFile (uiDirectory ++ "/editor.ui")
  dialog <- getObject builder "editor_dialog" Adw.Dialog
  title <- getObject builder "editor_title" Adw.WindowTitle
  view <- getObject builder "source_view" Source.View
  buffer <- getObject builder "source_buffer" Source.Buffer
  result <- getObject builder "result_label" Gtk.Label
  cancel <- getObject builder "cancel_button" Gtk.Button
  applyButton <- getObject builder "apply_button" Gtk.Button

  set title [ #title := T.pack ("Cell " ++ refName r)
            , #subtitle := maybe "This cell is empty"
                                 (const "Editing an existing cell") source ]
  applySyntaxHighlighting buffer
  Gtk.textBufferSetText buffer (T.pack (fromMaybe "" source)) (-1)

  -- Which request the Result line is waiting on, and whether one is already
  -- due to be sent.  Counters rather than flags, so that an answer can be
  -- recognised as stale.
  asked <- newIORef (0 :: Int)
  shown <- newIORef (0 :: Int)
  settling <- newIORef False

  let bufferText = do
        start <- Gtk.textBufferGetStartIter buffer
        end <- Gtk.textBufferGetEndIter buffer
        T.unpack <$> Gtk.textBufferGetText buffer start end False

      ask = do
        mine <- atomicModifyIORef' asked (\n -> (n + 1, n + 1))
        text <- bufferText
        if all (`elem` (" \t\n\r" :: String)) text
          then writeIORef shown mine >> showEmpty result
          else preview text $ \answer -> do
                 current <- readIORef shown
                 -- Older than what is already up: drop it.
                 when (mine > current) $ do
                   writeIORef shown mine
                   showAnswer result answer

      askSoon = do
        busy <- readIORef settling
        unless' busy $ do
          writeIORef settling True
          void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT settleMilliseconds $ do
            writeIORef settling False
            ask
            pure False

      commit = do
        text <- bufferText
        apply text
        void (Adw.dialogClose dialog)

  _ <- on buffer #changed askSoon
  _ <- on cancel #clicked (void (Adw.dialogClose dialog))
  _ <- on applyButton #clicked commit

  -- Ctrl+Return applies, so you never have to reach for the mouse.
  keys <- Gtk.eventControllerKeyNew
  _ <- on keys #keyPressed $ \keyval _ state ->
    if (keyval == Gdk.KEY_Return || keyval == Gdk.KEY_KP_Enter)
         && Gdk.ModifierTypeControlMask `elem` state
      then commit >> pure True
      else pure False
  Gtk.widgetAddController view keys

  ask
  Adw.dialogPresent dialog (Just parent)
  void (Gtk.widgetGrabFocus view)

unless' :: Bool -> IO () -> IO ()
unless' condition action = if condition then pure () else action

showEmpty :: Gtk.Label -> IO ()
showEmpty label = do
  Gtk.labelSetLabel label "empty"
  Gtk.widgetAddCssClass label "dim-label"
  Gtk.widgetRemoveCssClass label "error"

showAnswer :: Gtk.Label -> Preview -> IO ()
showAnswer label answer = do
  Gtk.labelSetLabel label (T.pack (previewText answer))
  Gtk.widgetRemoveCssClass label "dim-label"
  if previewIsError answer
    then Gtk.widgetAddCssClass label "error"
    else Gtk.widgetRemoveCssClass label "error"

-- | Scheme highlighting, in a style scheme that matches the current light or
-- dark preference.  Highlighting is a nicety; never let it stop the editor
-- from opening.
applySyntaxHighlighting :: Source.Buffer -> IO ()
applySyntaxHighlighting buffer = do
  languages <- Source.languageManagerGetDefault
  scheme <- Source.languageManagerGetLanguage languages "scheme"
  forM_ scheme $ \language -> Source.bufferSetLanguage buffer (Just language)
  manager <- Adw.styleManagerGetDefault
  dark <- Adw.styleManagerGetDark manager
  schemes <- Source.styleSchemeManagerGetDefault
  let names = if dark then ["Adwaita-dark", "solarized-dark"]
                      else ["Adwaita", "solarized-light"]
  found <- firstScheme schemes names
  forM_ found $ \style -> Source.bufferSetStyleScheme buffer (Just style)

firstScheme :: Source.StyleSchemeManager -> [Text] -> IO (Maybe Source.StyleScheme)
firstScheme _ [] = pure Nothing
firstScheme manager (name : more) = do
  found <- Source.styleSchemeManagerGetScheme manager name
  case found of
    Just scheme -> pure (Just scheme)
    Nothing -> firstScheme manager more

getObject
  :: (GObject o, TypedObject o)
  => Gtk.Builder -> Text -> (ManagedPtr o -> o) -> IO o
getObject builder name constructor = do
  found <- Gtk.builderGetObject builder name
  case found of
    Nothing -> error ("cellar: the UI file has no " ++ T.unpack name)
    Just object -> unsafeCastTo constructor object
