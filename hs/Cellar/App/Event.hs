-- | Everything that can happen to the window.
--
-- One type for the lot: a key in the grid, an answer from the kernel, a
-- menu item, a folder that changed on disk.  They all arrive at
-- 'Cellar.App.Update.update', which is the only place that decides anything.
module Cellar.App.Event
  ( Event (..)
  , Opening (..)
  , Action (..)
  ) where

import Data.Text (Text)

import Cellar.App.State (Tag, TabId)
import Cellar.Grid.Model (GridEvent)
import Cellar.Ref (Axis, Ref)
import Cellar.Sexp (Sexp)
import Cellar.Store (Sheet, Workbook)

-- | What somebody asked for through a menu, a keystroke or a button that
-- carries an application action.
data Action
  = NewWorkbook
  | NewScratch
  | OpenWorkbook
  | SaveNothing
  | CopyTo
  | AddSheet
  | RenameSheet
  | DeleteSheet
  | NextSheet
  | PreviousSheet
  | RecalculateSheet
  | ClearCell
  | EditCell
  | OpenCellElsewhere
  | MoveLine Axis Int
  | InsertLine Axis Bool
    -- | Take away the row or column the active cell is on.
  | DeleteLine Axis
  | OpenRecentAt FilePath
  | ClearRecent
  | Preferences
  | Shortcuts
  | About
  | Quit
  deriving (Eq, Show)

-- | Why a workbook is being opened, which is what decides two things a
-- workbook cannot say for itself.
data Opening
  = AsUsual
    -- ^ Somebody asked for this workbook, so it joins the list of the ones
    -- opened lately, and it is not scratch.
  | AsScratch
    -- ^ Cellar made it to think in.  A scratch workbook is made by a keypress
    -- and thrown away as often as it is kept, and ten of those would be a list
    -- with none of the workbooks you meant in it.
  | AsBefore
    -- ^ It is already open and is being read again, so nothing about how it
    -- was opened changes.
  deriving (Eq, Show)

data Event
  -- The widgets
  = GridSaid TabId GridEvent
  | EditPressed
  | RecalculatePressed
  | TabSelected TabId
  | TabsReordered [TabId]
  | TabCloseAsked TabId
  | WindowClosing
  -- The gestures, which are not declarative
  | LineChosen TabId Axis Int
  | DragBegun TabId Axis Int
  | DragMoved TabId Int
  | DragDropped TabId Int
  | DragCancelled TabId
  -- The kernel
  | KernelSaid Tag Sexp
    -- ^ The kernel answered something, and this is what it was for.
  | KernelRefused Tag String
  | Stalled Bool
    -- ^ The kernel has been sitting on a request for longer than anybody would
    -- expect, or has stopped doing so.  Said only when it changes: a window
    -- with nothing wrong with it hears nothing.
  -- What was asked for
  | Act Action
  -- What came back from something the update asked for
  | NeverMind
    -- ^ That dialog was answered with "keep waiting".
  | Stopped
    -- ^ The kernel was stopped and started again, on purpose.
  | Toast Text
  | WorkbookRead Workbook Opening [(String, Sheet)] (Maybe String)
    -- ^ A workbook, how it is being opened, its sheets as they are on disk,
    -- and which one to show.
  | SheetsRead [(String, Sheet)] (Maybe String)
    -- ^ The sheets of the workbook already open, read again.
  | WorkbookRefused FilePath
  | Remembered FilePath
  | ScratchMade FilePath
    -- ^ A workbook to think in was made here.
  | SheetsOnDisk [(String, Sheet)]
    -- ^ The sheets as they now are on disk, and whether the set of them
    -- changed rather than only their contents.
  | DiskChanged
  | SheetNamed (Maybe TabId) String
    -- ^ A dialog asked for a name: for a tab to be renamed, or for a new one.
  | SheetAdded Workbook String
  | SheetRenamed Workbook TabId String String
  | SheetDeleted TabId String
  | TabKept TabId
    -- ^ A tab whose close button was pressed, and which stays after all.
  | EditorCommandSet String
  | WorkbookMade FilePath Bool Bool
    -- ^ A folder for a new workbook, whether to copy this one into it, and
    -- whether to make a Git repository of it.
  | FolderChosen FilePath
  | CellEdited TabId Ref String
    -- | A sheet's folder has gone from the disk.  The kernel still holds the
    -- sheet, and the sheets that are left may have been naming it.
  | SheetRemoved Workbook String
    -- | These folders are being watched for changes made behind our back.
    -- The window holds the list because what is watched is a fact about the
    -- workbook that is open, and a subscription that starts and stops with
    -- the state needs the state to say so.
  | Watching [FilePath]
    -- | The cell editor wants to know what this half-written expression comes
    -- to.  The number is the editor's own, and comes back with the answer.
  | PreviewWanted TabId Ref Int String
  deriving (Eq, Show)
