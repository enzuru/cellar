-- | Everything the window can do, as values.
--
-- "Cellar.App.Update" is a function from a state and an event to the next
-- state and a list of these.  It does no IO and holds no reference to
-- anything, so what the window does about an event can be read off the result
-- rather than inferred from the state that turns up afterwards.
--
-- Each of these says /what/, never /how/.  @Cellar.App.Perform@ says how, and
-- is the only part of the window that holds the 'Cellar.App.Env.Env'.
--
-- An effect that can fail answers with 'Cellar.App.Event.Toast', which the
-- update already knows what to do with.  That is the one thing the
-- interpreter decides for itself, and it decides nothing else: an effect
-- carries the folder to write to and the sheet to write, rather than a tab to
-- look up, so that there is no state on the other side to disagree with.
module Cellar.App.Effect
  ( Effect (..)
  , Doing (..)
  , Step (..)
  , stay
  , now
  ) where

import Data.Text (Text)

import Cellar.App.Event (Event, Opening)
import Cellar.App.State (State, Tag, TabId)
import Cellar.Config (Config)
import Cellar.Ref (Axis, Ref)
import Cellar.Op (Op)
import Cellar.Store (Book, Workbook)

data Effect
  = Emit Event
    -- ^ Hand this straight back to the loop.  For the cases where one event
    -- means another has happened, with nothing to do in between.

  --
  -- The kernel
  --
  | Request [(Op, Tag)]
    -- ^ Send these requests, each numbered and remembered under its tag.
  | RestartKernel
    -- ^ Stop whatever the kernel is doing, start it again, and forget every
    -- answer it still owed.
  | MarkReady
    -- ^ The kernel has answered something, so it is up.

  --
  -- The window
  --
  | Notify Text
    -- ^ A toast.
  | ShowPalette Text
    -- ^ Load this stylesheet, which is how a cell gets a colour.
  | ShowDrag TabId (Maybe (Axis, Int, Int))
    -- ^ Draw the line being dragged, which is classes on widgets: which axis
    -- it is on, where it started and where it would land.
  | FocusGrid
  | FillRecent [FilePath]
    -- ^ Rebuild the submenu of workbooks opened lately.

  --
  -- The windows of their own
  --
  | AskSheetName (Maybe TabId) String String
    -- ^ Which tab (none for a new sheet), the name to start from, the title.
  | AskNewWorkbook String FilePath Bool
    -- ^ The name to suggest, where to put it, and whether this is a copy.
  | AskToDeleteSheet TabId String
  | ChooseFolder
  | OpenPreferences Config
  | ShowShortcuts
  | ShowAbout
  | AskAboutKernel
  | NeverMindKernel
  | OpenCellEditor TabId Ref (Maybe String)
    -- ^ The tab, the cell, and what is in it.
  | OpenCellFile TabId Ref String Config
    -- ^ Hand a cell to the editor in the preferences.  A cell has no file of
    -- its own any more, so one is written somewhere temporary and read back
    -- when the editor is done with it: the tab, the cell, what is in it now,
    -- and where to find the editor.
  | ShowPreview Int String Bool
    -- ^ Hand the editor what its numbered question came to, and whether the
    -- answer is an error.  Nothing at all when no editor is up.
    -- ^ Bring the cell's file into being and hand it to the editor in the
    -- preferences, or to the desktop when there is none.

  --
  -- The file on disk
  --
  | SaveWorkbook Workbook Book
    -- ^ Write the whole workbook.  A workbook is one file, so there is no
    -- smaller thing to write: a cell, a column width and the order of the
    -- tabs all reach the disk the same way.
  | SaveConfig Config
  | Watch Workbook
    -- ^ Watch the workbook file for changes made behind our back.
  | ReadWorkbookAt FilePath Opening
    -- ^ Read a workbook off the disk and say what to do with it.
  | ReadWorkbookAgain Workbook
    -- ^ Read the open workbook again, because the file changed under us.
  | MakeWorkbook FilePath
    -- ^ Make a workbook here, holding one empty sheet.
  | MakeScratch
    -- ^ Make a workbook to think in, somewhere out of the way.
  | CopyWorkbook FilePath Book
    -- ^ Write this workbook out somewhere else, and open it there.
  deriving (Eq, Show)

-- | One thing to do, and when.
data Doing
  = Now Effect
    -- ^ Do this.
  | Settle Text Effect
    -- ^ Do this in a moment, unless another 'Settle' under the same name
    -- arrives first, in which case do that one instead.  For work that is
    -- asked for far faster than it is worth doing: a column dragged wider
    -- reports its width many times a second and every report asks for the
    -- sheet to be written.
  deriving (Eq, Show)

-- | What an event came to: the next state and what to do about it, or the end
-- of the program.
data Step = Step State [Doing] | Stop

-- | A state, and nothing to do about it.
stay :: State -> Step
stay state = Step state []

-- | A state, and one thing to do about it.
now :: State -> Effect -> Step
now state effect = Step state [Now effect]
