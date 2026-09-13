{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | The whole of the window, as one value.
--
-- Everything the window shows and everything it is in the middle of doing is
-- here: the workbook, the sheets and their grids, which page is up, what the
-- kernel owes an answer for.  Nothing in this module does anything -- there is
-- no IO in it at all -- so what the window does can be tested by handing a
-- state and an event to 'Cellar.App.Update' and looking at what comes back.
--
-- It replaced a record of twelve mutable references.  The reason for the
-- change is that those twelve could disagree with each other and with the
-- widgets, and a value cannot.
module Cellar.App.State
  ( -- * The state
    State (..)
  , Page (..)
  , Open (..)
  , Tab (..)
  , TabId (..)
  , newState
    -- * Asking after it
  , currentTab
  , tabById
  , tabNamed
  , stateTabs
  , stateWorkbook
  , stateScratch
  , tabOrder
  , tabPosition
  , subtitleOf
  , sheetShowing
  , sourceOf
    -- * Changing it
  , withOpen
  , withTab
  , withTabs
  , withCurrentTab
  , withWorkbook
  , opened
  , freshTab
  , addTab
  , forgetTab
  , selectTab
  , orderTabs
    -- * What the kernel owes
  , Tag (..)
  ) where

import qualified Data.Map.Strict as M
import Data.Foldable (find, toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Cellar.Config
import Cellar.Grid.Model
import Cellar.Ref
import Cellar.Store
import Cellar.View (View)

-- | Which of the window's two pages is up.
data Page = StartPage | SheetPage
  deriving (Eq, Show)

-- | The shell's own name for a sheet, handed out once and never reused.
--
-- Sheet names are unique within a workbook, but they change: renaming one is
-- an ordinary thing to do, and a name that is being changed is no use for
-- saying which sheet is meant.  The kernel is told the name, because that is
-- what cells use.
newtype TabId = TabId Int
  deriving newtype (Eq, Ord, Show)

-- | A sheet of the workbook, as the window holds it.
data Tab = Tab
  { tabId :: TabId
  , tabName :: String
    -- | The cell sources.  The shell owns these -- it is the side that reads
    -- and writes the files -- and the kernel owns what they come to.
  , tabSources :: [(String, String)]
  , tabGrid :: GridModel
  }

-- | What the kernel owes an answer for, and what to do with it.
--
-- A request is a number on the wire, and this says what that number was about.
-- It replaced a closure waiting in a map: a closure cannot be looked at, and
-- these can be compared, shown in a test, and thrown away when the kernel
-- restarts.
data Tag
  = Pinged
    -- ^ The first request of all, which says the kernel is up.
  | Snapshot TabId (Maybe Text)
    -- ^ Take the snapshot into this tab, and say this afterwards.
  | CellSet TabId String
    -- ^ A cell was given a source: keep what the kernel kept, write the cell's
    -- file, and take the snapshot that came with it.
  | Opened TabId Bool
    -- ^ A sheet was opened: take the snapshot and start in the corner.  The
    -- flag is for a workbook Cellar has just made, whose sheet files say
    -- nought by nought until the size on screen is written into them.
  | Reopened TabId Ref
    -- ^ A sheet was read again: take the snapshot and keep the cursor.
  | Closed
    -- ^ A sheet was closed: what is left is in the answer.
  | Renamed TabId
  | Ignored
    -- ^ An answer nobody is waiting for.
  deriving (Eq, Show)

-- | A workbook, with its sheets and the one showing.
--
-- The three are one value because they are true together: a workbook has at
-- least one sheet -- the window refuses to close the last -- and one of those
-- sheets is on screen.  Held apart, as a workbook and a list and a name, the
-- state could say things that never happen, and every reader had to allow for
-- them.
data Open = Open
  { openWorkbook :: Workbook
  , openTabs :: NonEmpty Tab
  , openCurrent :: TabId
    -- | A workbook Cellar made to think in, which is not offered as a recent
    -- one and says so under the title.
  , openScratch :: Bool
  }

data State = State
  { stateOpen :: Maybe Open
  , stateNextTab :: TabId
  , statePage :: Page
  , stateRecent :: [FilePath]
  , stateConfig :: Config
    -- | Where a new workbook goes, until somebody chooses somewhere else.
  , stateLocation :: FilePath
  , stateHome :: FilePath
    -- | Set once the kernel has answered anything at all.  Until then it is
    -- still starting, and the first real request would otherwise be timed as
    -- though a cell had gone wrong.
  , stateKernelAnswered :: Bool
  , stateWaitingOnPurpose :: Bool
  , stateAskingAboutKernel :: Bool
    -- | True while tabs are being built or torn down, which is when what
    -- looks like somebody switching sheets is nothing of the kind.  Nothing
    -- is written to disk during that.
  , stateLoading :: Bool
  , stateWatching :: [FilePath]
    -- | The colours the cells of this workbook have asked for, and the class
    -- each one is drawn under.  One palette for the window rather than one per
    -- sheet, because the names go into one stylesheet.
  , statePalette :: M.Map (Maybe String, Maybe String) Text
    -- | Set for a workbook Cellar made a moment ago, whose folders are empty
    -- until its sheets have been written out once.
  , stateFresh :: Bool
    -- | The answer to the last close button somebody pressed on a tab: which
    -- tab, and whether it goes.  A command rather than a fact, so the tab view
    -- acts on it once and it is put back to nothing.
  , stateCloseAnswer :: Maybe (Text, Bool)
  }

newState :: Config -> FilePath -> State
newState config home = State
  { stateOpen = Nothing
  , stateNextTab = TabId 1
  , statePage = StartPage
  , stateRecent = recentWorkbooks config
  , stateConfig = config
  , stateLocation = home
  , stateHome = home
  , stateKernelAnswered = False
  , stateWaitingOnPurpose = False
  , stateAskingAboutKernel = False
  , stateLoading = False
  , stateWatching = []
  , statePalette = M.empty
  , stateFresh = False
  , stateCloseAnswer = Nothing
  }

--
-- Asking after it
--

-- | The sheets of the workbook, in the order their tabs are in.
stateTabs :: State -> [Tab]
stateTabs = maybe [] (toList . openTabs) . stateOpen

stateWorkbook :: State -> Maybe Workbook
stateWorkbook = fmap openWorkbook . stateOpen

-- | Whether the workbook open is one Cellar made to think in.
stateScratch :: State -> Bool
stateScratch = maybe False openScratch . stateOpen

-- | The sheet on screen.  There is one whenever a workbook is open.
currentTab :: State -> Maybe Tab
currentTab state = do
  open <- stateOpen state
  find ((== openCurrent open) . tabId) (openTabs open)

tabById :: TabId -> State -> Maybe Tab
tabById wanted state = listToMaybe [ t | t <- stateTabs state, tabId t == wanted ]

tabNamed :: String -> State -> Maybe Tab
tabNamed name state = listToMaybe [ t | t <- stateTabs state, tabName t == name ]

-- | The sheets by name, in the order their tabs are in.
tabOrder :: State -> [String]
tabOrder = map tabName . stateTabs

tabPosition :: TabId -> State -> Maybe Int
tabPosition wanted state =
  listToMaybe [ n | (n, t) <- zip [0 ..] (stateTabs state), tabId t == wanted ]

-- | What the title says under "Cellar".
subtitleOf :: State -> Text
subtitleOf state = case stateOpen state of
  Nothing -> "No workbook open"
  Just open
    | openScratch open -> "Scratch"
    | otherwise -> T.pack (workbookName (openWorkbook open))

sheetShowing :: State -> Bool
sheetShowing state = statePage state == SheetPage

-- | What was typed into a cell of the sheet showing, if anything was.
sourceOf :: Ref -> State -> Maybe String
sourceOf r state = currentTab state >>= lookup (refName r) . tabSources

--
-- Changing it
--

-- | Change the workbook that is open, if one is.
withOpen :: (Open -> Open) -> State -> State
withOpen change state = state { stateOpen = change <$> stateOpen state }

-- | Change one sheet, by its name.
withTab :: TabId -> (Tab -> Tab) -> State -> State
withTab wanted change = withOpen $ \open -> open
  { openTabs = fmap (\t -> if tabId t == wanted then change t else t) (openTabs open) }

-- | Change every sheet of the workbook.
withTabs :: (Tab -> Tab) -> State -> State
withTabs change = withOpen $ \open -> open { openTabs = fmap change (openTabs open) }

-- | Say the workbook again, after a write has moved or renamed something in
-- it.  The sheets stay as they are.
withWorkbook :: Workbook -> State -> State
withWorkbook workbook = withOpen $ \open -> open { openWorkbook = workbook }

-- | Change the sheet that is showing, if one is.
withCurrentTab :: (Tab -> Tab) -> State -> State
withCurrentTab change state =
  maybe state (\t -> withTab (tabId t) change state) (currentTab state)

-- | Open this workbook, showing these sheets.  The sheet named is the one on
-- screen, and the first stands in when the name is not one of them.
opened :: Workbook -> NonEmpty Tab -> Maybe String -> Bool -> State -> State
opened workbook tabs showing scratch state = state
  { stateOpen = Just Open
      { openWorkbook = workbook
      , openTabs = tabs
      , openCurrent = maybe (tabId (NE.head tabs)) tabId
          (find ((== showing) . Just . tabName) tabs)
      , openScratch = scratch
      }
  }

-- | A sheet of this workbook, not yet part of it.
freshTab :: String -> View -> State -> (State, Tab)
freshTab name view state =
  ( state { stateNextTab = nextAfter (stateNextTab state) }
  , Tab { tabId = stateNextTab state
        , tabName = name
        , tabSources = []
        , tabGrid = newGridModel view
        } )

-- | The name the sheet after this one takes.
nextAfter :: TabId -> TabId
nextAfter (TabId n) = TabId (n + 1)

-- | Add a sheet at the end, and show it.
addTab :: Tab -> State -> State
addTab tab = withOpen $ \open -> open
  { openTabs = openTabs open <> (tab :| [])
  , openCurrent = tabId tab
  }

-- | Take a sheet away.  The last sheet of a workbook cannot go: a workbook
-- with no sheets is not a thing this module allows for, and the window refuses
-- to close one before it gets here.
forgetTab :: TabId -> State -> State
forgetTab wanted = withOpen $ \open ->
  case NE.nonEmpty [ t | t <- toList (openTabs open), tabId t /= wanted ] of
    Nothing -> open
    Just left -> open
      { openTabs = left
      , openCurrent = if openCurrent open == wanted
          then tabId (NE.head left) else openCurrent open
      }

selectTab :: TabId -> State -> State
selectTab wanted = withOpen $ \open ->
  if any ((== wanted) . tabId) (openTabs open)
    then open { openCurrent = wanted }
    else open

-- | Put the sheets in this order, naming them by identifier.  Anything the
-- order does not name keeps its place at the end, which is what a reorder of a
-- tab view that has just gained a page looks like.
orderTabs :: [TabId] -> State -> State
orderTabs wanted = withOpen $ \open ->
  case NE.nonEmpty (named open ++ rest open) of
    Nothing -> open
    Just ordered -> open { openTabs = ordered }
  where
    named open = [ t | identifier <- wanted
                 , t <- toList (openTabs open), tabId t == identifier ]
    rest open = [ t | t <- toList (openTabs open), tabId t `notElem` wanted ]
