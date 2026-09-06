-- | Handing a cell to an external editor.
--
-- A cell is a file in the sheet folder, so there is nothing to hand over but
-- its path: the editor opens @cells/B2.scm@, and saving in the editor saves
-- the cell.  Cellar does not wait for the editor to exit and does not read
-- anything back -- the workbook folder is watched, so the grid catches up with
-- each save on its own, with the editor still open.
--
-- That is why an external editor no longer has to be told to wait.  An earlier
-- design copied the cell to a temporary file and read it back when the editor
-- exited, which meant @code@ or @gedit@ had to be run with @--wait@ or the edit
-- was lost.  Now the file is the cell.
--
-- Its own module, and free of GTK, so that it can be run against a stand-in
-- editor in a test rather than only by hand.
module Cellar.External (openExternalEditor) where

import Control.Exception (SomeException, try)
import Control.Monad (void)

import System.Process (spawnProcess)

import Cellar.Config (editorArgv)
import Cellar.Ref (Ref, refName)
import Cellar.Store (cellFilePath)

-- | Open a cell with a command.  Answers with the program that was started, or
-- 'Nothing' when it could not be -- nearly always a command that is not on
-- PATH, which the caller reports before falling back to the built-in editor
-- rather than leaving a cell that cannot be edited at all.
openExternalEditor :: String -> FilePath -> Ref -> IO (Maybe FilePath)
openExternalEditor command directory r =
  case editorArgv command (cellFilePath directory (refName r)) of
    [] -> pure Nothing
    (program : arguments) -> do
      outcome <- try (void (spawnProcess program arguments))
      pure $ case outcome :: Either SomeException () of
        Left _ -> Nothing
        Right () -> Just program
