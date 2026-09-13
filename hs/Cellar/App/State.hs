{-# LANGUAGE OverloadedStrings #-}

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
  , Tab (..)
  , TabId
  , newState
    -- * Asking after it
  , currentTab
  , tabById
  , tabNamed
  , tabOrder
  , tabPosition
  , subtitleOf
  , sheetShowing
  , sourceOf
    -- * Changing it
  , withTab
  , withCurrentTab
  , addTab
  , forgetTab
  , selectTab
  , orderTabs
    -- * What the kernel owes
  , Tag (..)
  ) where

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
type TabId = Int

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

data State = State
  { stateWorkbook :: Maybe Workbook
  , stateScratch :: Bool
  , stateTabs :: [Tab]
  , stateCurrent :: Maybe TabId
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
  { stateWorkbook = Nothing
  , stateScratch = False
  , stateTabs = []
  , stateCurrent = Nothing
  , stateNextTab = 1
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
  , stateFresh = False
  , stateCloseAnswer = Nothing
  }

--
-- Asking after it
--

currentTab :: State -> Maybe Tab
currentTab state = stateCurrent state >>= \wanted -> tabById wanted state

tabById :: TabId -> State -> Maybe Tab
tabById wanted state = listToMaybe [ t | t <- stateTabs state, tabId t == wanted ]

tabNamed :: String -> State -> Maybe Tab
tabNamed name state = listToMaybe [ t | t <- stateTabs state, tabName t == name ]

-- | The sheets in the order their tabs are in.
tabOrder :: State -> [String]
tabOrder = map tabName . stateTabs

tabPosition :: TabId -> State -> Maybe Int
tabPosition wanted state =
  listToMaybe [ n | (n, t) <- zip [0 ..] (stateTabs state), tabId t == wanted ]

-- | What the title says under "Cellar".
subtitleOf :: State -> Text
subtitleOf state = case (stateScratch state, stateWorkbook state) of
  (True, _) -> "Scratch"
  (_, Just open) -> T.pack (workbookName open)
  _ -> "No workbook open"

sheetShowing :: State -> Bool
sheetShowing state = statePage state == SheetPage

-- | What was typed into a cell of the sheet showing, if anything was.
sourceOf :: Ref -> State -> Maybe String
sourceOf r state = currentTab state >>= lookup (refName r) . tabSources

--
-- Changing it
--

-- | Change one tab, by its identifier.
withTab :: TabId -> (Tab -> Tab) -> State -> State
withTab wanted change state = state
  { stateTabs = [ if tabId t == wanted then change t else t | t <- stateTabs state ] }

-- | Change the tab that is showing, if one is.
withCurrentTab :: (Tab -> Tab) -> State -> State
withCurrentTab change state =
  maybe state (\t -> withTab (tabId t) change state) (currentTab state)

-- | Add a sheet at the end, and say which tab it became.
addTab :: String -> View -> State -> (State, Tab)
addTab name view state =
  let tab = Tab { tabId = stateNextTab state
                , tabName = name
                , tabSources = []
                , tabGrid = newGridModel view
                }
  in ( state { stateTabs = stateTabs state ++ [tab]
             , stateNextTab = stateNextTab state + 1
             }
     , tab )

forgetTab :: TabId -> State -> State
forgetTab wanted state = state
  { stateTabs = [ t | t <- stateTabs state, tabId t /= wanted ]
  , stateCurrent = case stateCurrent state of
      Just showing | showing == wanted -> tabId <$> listToMaybe remaining
      other -> other
  }
  where remaining = [ t | t <- stateTabs state, tabId t /= wanted ]

selectTab :: TabId -> State -> State
selectTab wanted state
  | any ((== wanted) . tabId) (stateTabs state) = state { stateCurrent = Just wanted }
  | otherwise = state

-- | Put the tabs in this order, naming them by identifier.  Anything the order
-- does not name keeps its place at the end, which is what a reorder of a tab
-- view that has just gained a page looks like.
orderTabs :: [TabId] -> State -> State
orderTabs wanted state = state { stateTabs = named ++ rest }
  where
    named = [ t | identifier <- wanted, t <- stateTabs state, tabId t == identifier ]
    rest = [ t | t <- stateTabs state, tabId t `notElem` wanted ]


