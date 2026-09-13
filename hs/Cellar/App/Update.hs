{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | What the window does about what happens to it.
--
-- One function, from a state and an event to the next state and whatever has
-- to be done in the world to keep up: a request to the kernel, a file written,
-- a dialog opened.  Anything that comes back from one of those comes back as
-- another event, so this is the only place anything is decided.
--
-- The shape of every case is the same.  Work out the new state, which is a
-- value; hand back whatever has to be done, which is an 'IO' action that posts
-- events of its own.  Neither half can be skipped and neither can be done
-- twice, which is the whole reason for the arrangement.
module Cellar.App.Update
  ( update
  , defaultRows
  , defaultColumns
  , firstSheetName
  , patienceSeconds
  , sheetOf
  , snapshotInto
  ) where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM, forM_, unless, void, when)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Traversable (mapAccumL)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName)
import System.Process (callProcess)

import qualified GI.GLib as GLib

import GI.Gtk.Declarative.App.Simple (Transition (..))

import Cellar.App.Env
import Cellar.App.Event
import Cellar.App.State
import Cellar.Client
import Cellar.Config
import Cellar.External
import Cellar.Grid.Model
import Cellar.Ref
import Cellar.Sexp
import Cellar.Store
import Cellar.View

-- | How much empty room a new sheet gets.  A question about what looks right
-- in a window, which is why it is settled here and not in the kernel.
defaultRows, defaultColumns :: Int
defaultRows = 100
defaultColumns = 26

-- | What the first sheet of a new workbook is called until it is renamed.
firstSheetName :: String
firstSheetName = "Sheet 1"

-- | How long a cell may take before Cellar assumes something has gone wrong
-- and offers to stop it.  Long enough that an honestly slow expression is not
-- interrupted, short enough that a cell that will never finish does not look
-- like the application hanging.
patienceSeconds :: Double
patienceSeconds = 10

--
-- Saying what to do
--

-- | A state, and nothing to do about it.
stay :: State -> Transition State Event
stay state = Transition state (pure Nothing)

-- | A state, and something to do about it.  Whatever it finds out, it posts.
after :: State -> IO () -> Transition State Event
after state action = Transition state (action >> pure Nothing)

update :: Env -> State -> Event -> Transition State Event
update env state = \case

  --
  -- The grid
  --

  GridSaid tab event -> case tabById tab state of
    Nothing -> stay state
    Just found ->
      let (model, outs) = gridEvent event (tabGrid found)
          moved = withTab tab (\t -> t { tabGrid = model }) state
      in after moved (mapM_ (carryOut env moved tab) outs)

  LineChosen tab axis index -> stay $ withTab tab
    (\t -> t { tabGrid = fromMaybe (tabGrid t) (selectLine axis index (tabGrid t)) })
    state

  DragBegun tab axis index -> dragging env tab (Just (axis, index, index)) state
  DragMoved tab index -> case dragOf tab state of
    Just (axis, from, _) -> dragging env tab (Just (axis, from, index)) state
    Nothing -> stay state
  DragCancelled tab -> dragging env tab Nothing state
  DragDropped tab landing -> case dragOf tab state of
    Nothing -> stay state
    Just (axis, from, _) ->
      let cleared = withTab tab (\t -> t { tabGrid = withDrag Nothing (tabGrid t) }) state
          moved = tabById tab cleared >>= \t -> moveLine axis from landing (tabGrid t)
      in case moved of
           Nothing -> after cleared (showDragOf env tab cleared)
           Just (model, command) ->
             let after' = withTab tab (\t -> t { tabGrid = model }) cleared
             in after after' $ do
                  showDragOf env tab after'
                  carryOut env after' tab (Ask command)

  --
  -- The bars and the tabs
  --

  EditPressed -> editing env state
  RecalculatePressed -> recalculating env state

  TabSelected tab ->
    let chosen = selectTab tab state
    in after chosen $ do
         unless (stateLoading chosen) $ forM_ (stateWorkbook chosen) $ \open ->
           forM_ (tabById tab chosen) $ \t ->
             quietly (setWorkbookActive open (tabName t))
         focusGrid env chosen

  TabsReordered order ->
    let ordered = orderTabs order state
    in after ordered $
         unless (stateLoading ordered) $ forM_ (stateWorkbook ordered) $ \open ->
           quietly (setWorkbookOrder open (tabOrder ordered))

  -- A tab is a sheet of the workbook rather than a view of one, so closing it
  -- is deleting it -- which is worth being asked about, and worth refusing
  -- when it would leave the workbook with nothing in it.
  TabCloseAsked tab -> case tabById tab state of
    Nothing -> stay state
    Just found
      | length (stateTabs state) <= 1 ->
          after (keeping tab state) $ do
            notify env "A workbook keeps at least one sheet"
      | otherwise -> after (state { stateCloseAnswer = Nothing }) $
          askToDelete env tab (tabName found)

  TabKept tab -> stay (keeping tab state)

  SheetDeleted tab name -> case stateWorkbook state of
    Nothing -> stay state
    Just open -> after (forgetTab tab (keeping tab state)) $ do
      outcome <- try (removeWorkbookSheet open name)
      case outcome :: Either StoreError [String] of
        Left (StoreError why) -> notify env (T.pack why)
        Right _ -> do
          -- The sheets that are left may have been naming this one; the kernel
          -- says what they come to now that they cannot.
          asking env [("close", [Str name], Closed)]
          watchAgain env open
          notify env (T.pack ("Deleted " ++ name))

  --
  -- The kernel
  --

  KernelSaid tag payload -> answered env state tag payload

  KernelRefused _ why -> after state (notify env (T.pack why))

  -- A cell that is not going to finish looks, from out here, exactly like one
  -- that is merely slow -- that is the halting problem, and Cellar is not
  -- going to solve it on a timer.  So this decides nothing: when the kernel
  -- has been sitting on something for longer than anybody would expect, it
  -- asks.
  Stalled True
    | stateKernelAnswered state
    , not (stateAskingAboutKernel state)
    , not (stateWaitingOnPurpose state) ->
        after state { stateAskingAboutKernel = True } (askAboutTheKernel env)
    | otherwise -> stay state

  Stalled False
    | stateAskingAboutKernel state || stateWaitingOnPurpose state ->
        after state { stateAskingAboutKernel = False
                    , stateWaitingOnPurpose = False }
              (neverMindTheKernel env)
    | otherwise -> stay state

  NeverMind -> stay state { stateAskingAboutKernel = False
                          , stateWaitingOnPurpose = True }

  -- Stopping restarts the kernel and reopens every sheet as it stands on disk.
  Stopped -> after state { stateAskingAboutKernel = False
                         , stateKernelAnswered = False } $ do
    restartKernel (envKernel env)
    forgetTags env
    asking env [("ping", [], Pinged)]
    post env DiskChanged
    notify env "Stopped the cell and restarted the kernel"

  --
  -- What somebody asked for
  --

  Act action -> acting env state action

  --
  -- The folder on disk
  --

  DiskChanged -> Transition state $ case stateWorkbook state of
    Nothing -> pure Nothing
    Just open -> do
      stillThere <- isWorkbookDirectory (workbookRoot open)
      if not stillThere then pure Nothing else do
        names <- workbookSheetNames open
        sheets <- forM names $ \name -> do
          sheet <- readSheetOrEmpty (workbookSheetDirectory open name)
          pure (name, sheet)
        pure (Just (SheetsOnDisk sheets (map fst sheets /= tabOrder state)))

  SheetsOnDisk sheets True -> after state $ do
    -- A sheet arrived or left -- somebody's commit, most likely.
    forM_ (stateWorkbook state) (watchAgain env)
    post env (SheetsRead sheets (tabName <$> currentTab state))
    notify env "Reloaded \8212 the sheets changed on disk"

  SheetsOnDisk sheets False ->
    let changed = [ (t, sheet)
                  | (name, sheet) <- sheets
                  , Just t <- [tabNamed name state]
                  , sheetCells sheet /= sortOn fst (tabSources t) ]
    in if null changed then stay state else
         let taken = foldr (\(t, sheet) s ->
                              withTab (tabId t) (\tab -> tab { tabSources = sheetCells sheet }) s)
                           state changed
         in after taken $ do
              asking env [ ( "open"
                           , openArguments (tabName t) sheet
                           , Reopened (tabId t) (modelActive (tabGrid t)) )
                         | (t, sheet) <- changed ]
              notify env "Reloaded \8212 the workbook changed on disk"

  --
  -- Opening, making and copying workbooks
  --

  -- A workbook with no sheets in it is not a workbook the window can show, so
  -- it is refused here rather than carried as a state nothing else allows for.
  WorkbookRead open how sheets showing -> case NE.nonEmpty sheets of
    Nothing -> after state (notify env (T.pack (workbookName open ++ " has no sheets")))
    Just some ->
      let (counted, tabs) = makeTabs some state
          scratch = case how of
            AsUsual -> False
            AsScratch -> True
            AsBefore -> maybe False openScratch (stateOpen state)
          loaded = (opened open tabs showing scratch counted)
            { statePage = SheetPage, stateLoading = False, stateFresh = False
            , stateCloseAnswer = Nothing }
      in after loaded $ do
           asking env [ ("open", openArguments (tabName t) sheet
                        , Opened (tabId t) (stateFresh state))
                      | (t, sheet) <- zip (NE.toList tabs) (map snd sheets) ]
           watchAgain env open
           when (how == AsUsual) (post env (Remembered (workbookRoot open)))
           focusGrid env loaded

  SheetsRead sheets showing -> case stateWorkbook state of
    Nothing -> stay state
    Just open -> Transition state (pure (Just (WorkbookRead open AsBefore sheets showing)))

  WorkbookRefused path -> after state $
    notify env (T.pack (takeFileName path ++ " is not a Cellar workbook"))

  Remembered path ->
    let updated = (stateConfig state) { recentWorkbooks = rememberRecent path (stateRecent state) }
    in after state { stateRecent = recentWorkbooks updated, stateConfig = updated } $ do
         quietly (saveConfig updated)
         fillRecentMenu env (recentWorkbooks updated)

  -- A workbook to think in is opened like any other, but it does not join the
  -- list of the ones opened lately.
  ScratchMade path -> Transition state (opening path AsScratch)

  WorkbookMade path copying wantsGit -> Transition state { stateFresh = True } $ do
    outcome <- try $ if copying
      then copyWorkbookTo state path
      else createWorkbook path firstSheetName
    case outcome :: Either SomeException () of
      Left _ -> do
        notify env (T.pack ("Could not " ++ (if copying then "copy to " else "create ")
                            ++ takeFileName path))
        pure Nothing
      Right () -> do
        when wantsGit (gitInit env path)
        notify env (T.pack ((if copying then "Now editing " else "Created ")
                            ++ takeFileName path))
        pure (Just (Act (OpenRecentAt path)))

  FolderChosen path -> after state (post env (Act (OpenRecentAt path)))

  --
  -- Sheets
  --

  SheetNamed Nothing name -> case stateWorkbook state of
    Nothing -> stay state
    Just open -> Transition state $ do
      outcome <- try (addWorkbookSheet open name)
      case outcome :: Either StoreError (Workbook, String) of
        Left (StoreError why) -> notify env (T.pack why) >> pure Nothing
        Right (moved, added) -> pure (Just (SheetAdded moved added))

  SheetNamed (Just tab) name -> case (stateWorkbook state, tabById tab state) of
    (Just open, Just found) -> Transition state $ do
      outcome <- try (renameWorkbookSheet open (tabName found) name)
      case outcome :: Either StoreError (Workbook, String) of
        Left (StoreError why) -> notify env (T.pack why) >> pure Nothing
        Right (moved, renamed) ->
          pure (Just (SheetRenamed moved tab (tabName found) renamed))
    _ -> stay state

  SheetAdded open name ->
    let (counted, tab) = freshTab name (emptyView defaultRows defaultColumns) state
        grown = addTab tab (withWorkbook open counted)
    in after grown $ do
         asking env [("open", openArguments name emptySheet, Opened (tabId tab) True)]
         watchAgain env open
         notify env (T.pack ("Added " ++ name))

  SheetRenamed open tab old new ->
    after (withTab tab (\t -> t { tabName = new }) (withWorkbook open state)) $ do
      -- Cells elsewhere say Summary!B2, so a sheet that changes its name
      -- changes what every one of them has to say.  The kernel rewrites them
      -- and hands back the sources of every sheet it touched.
      asking env [("rename", [Str old, Str new], Snapshot tab Nothing)]
      watchAgain env open

  --
  -- The cell editor
  --

  CellEdited tab r text -> case tabById tab state of
    Nothing -> stay state
    Just found -> after state $
      asking env [( "set-cell"
                  , [Str (tabName found), Str (refName r), Str text]
                  , CellSet tab (refName r) )]

  EditorCommandSet command ->
    let updated = (stateConfig state) { externalEditorCommand = command }
    in after state { stateConfig = updated } (quietly (saveConfig updated))

  Toast message -> after state (notify env message)

  WindowClosing -> Exit

--
-- The grid's questions
--

carryOut :: Env -> State -> TabId -> GridOut -> IO ()
carryOut env state tab = \case
  Edit r -> forM_ (tabById tab state) $ \found ->
    openEditor env tab (tabName found) r (lookup (refName r) (tabSources found))
  Ask Layout -> saveTab env state tab
  Ask (Clear r) -> forM_ (tabById tab state) $ \found ->
    asking env [( "set-cell"
                , [Str (tabName found), Str (refName r), Str ""]
                , CellSet tab (refName r) )]
  Ask (Move axis from to) -> forM_ (tabById tab state) $ \found ->
    asking env [( "move"
                , [Str (tabName found), Sym (axisName axis)
                  , Num (fromIntegral from), Num (fromIntegral to)]
                , Snapshot tab Nothing )]
  Ask (Insert axis at) -> forM_ (tabById tab state) $ \found ->
    asking env [( "insert"
                , [Str (tabName found), Sym (axisName axis), Num (fromIntegral at)]
                , Snapshot tab Nothing )]

dragOf :: TabId -> State -> Maybe (Axis, Int, Int)
dragOf tab state = tabById tab state >>= modelDrag . tabGrid

dragging :: Env -> TabId -> Maybe (Axis, Int, Int) -> State -> Transition State Event
dragging env tab drag state =
  let moved = withTab tab (\t -> t { tabGrid = withDrag drag (tabGrid t) }) state
  in after moved (showDragOf env tab moved)

showDragOf :: Env -> TabId -> State -> IO ()
showDragOf env tab state = forM_ (tabById tab state) (showDrag env tab . tabGrid)

--
-- What the kernel said
--

answered :: Env -> State -> Tag -> Sexp -> Transition State Event
answered env state tag payload = case tag of
  Ignored -> Transition state $ do
    -- The cell editor asks for previews of its own, and is handed them here.
    pure Nothing

  Pinged -> after state { stateKernelAnswered = True } (markReady (envKernel env))

  Opened tab fresh ->
    let taken = snapshotInto tab payload state
        started = withTab tab (\t -> t { tabGrid = fromMaybe (tabGrid t)
                                           (withActive (Ref 0 0) (tabGrid t)) }) taken
    in after started $ do
         showPaletteIfNew env state started
         -- A workbook Cellar made a moment ago has a sheet folder that says
         -- nothing about its size, and the sheet on screen is the ordinary
         -- 100 by 26.  Writing it out once is what makes the folder say what
         -- the window says.
         if fresh then saveTab env started tab else saveIfRewritten env started tab payload

  Reopened tab kept ->
    let taken = snapshotInto tab payload state
        back = withTab tab (\t -> t { tabGrid = fromMaybe (tabGrid t)
                                        (withActive kept (tabGrid t)) }) taken
    in after back $ do
         showPaletteIfNew env state back
         saveIfRewritten env back tab payload

  Snapshot tab said ->
    let taken = snapshotInto tab payload state
    in after taken $ do
         showPaletteIfNew env state taken
         saveIfRewritten env taken tab payload
         forM_ said (notify env)

  CellSet tab name ->
    let source = lookupKey "source" payload >>= asString
        kept = withTab tab (\t -> t { tabSources = setSource name source (tabSources t) })
                           state
        taken = snapshotInto tab payload kept
    in after taken $ do
         showPaletteIfNew env state taken
         forM_ (tabById tab taken) $ \found ->
           forM_ (sheetFolder taken found) $ \folder ->
             quietly (saveCell folder name source)
         saveIfRewritten env taken tab payload

  Closed -> after (othersFrom payload state) (pure ())

  Renamed tab -> after (snapshotInto tab payload state) (pure ())

-- | Take a snapshot into a tab, and into any other sheet the answer mentions.
--
-- A snapshot can bring colours the window has not seen before, and a colour is
-- drawn through a class in a stylesheet, so learning them is part of taking a
-- snapshot.  One palette for the window, handed down to every sheet, because
-- the names in it go into one stylesheet.
snapshotInto :: TabId -> Sexp -> State -> State
snapshotInto tab payload state = repainted
  where
    taken = othersFrom payload (into tab payload state)
    palette = foldl (\known t -> paletteFor (modelView (tabGrid t)) known)
                    (statePalette taken) (stateTabs taken)
    repainted
      | palette == statePalette taken = taken
      | otherwise = withTabs (\t -> t { tabGrid = withPalette palette (tabGrid t) })
                             taken { statePalette = palette }
    into which answer s = withTab which (\t ->
      let sources = maybe (tabSources t) readSources (lookupKey "sources" answer)
      in t { tabSources = sources
           , tabGrid = withView (viewFromSnapshot answer sources) (tabGrid t) }) s

-- | The part of an answer that is about the sheets nobody asked after.
othersFrom :: Sexp -> State -> State
othersFrom payload state = foldl into state others
  where
    others = fromMaybe [] (lookupKey "others" payload >>= toList)
    into s snapshot = case lookupKey "sheet" snapshot >>= asString of
      Nothing -> s
      Just name -> case tabNamed name s of
        Nothing -> s
        Just t -> withTab (tabId t) (\tab ->
          let sources = maybe (tabSources tab) readSources (lookupKey "sources" snapshot)
          in tab { tabSources = sources
                 , tabGrid = withView (viewFromSnapshot snapshot sources) (tabGrid tab) })
          s

-- | Write the colours out, when a snapshot brought one the window had not
-- seen.  The stylesheet is GTK's to hold, which is why this is the one part of
-- taking a snapshot that is not a change to a value.
showPaletteIfNew :: Env -> State -> State -> IO ()
showPaletteIfNew env before after' =
  when (statePalette after' /= statePalette before)
       (showPalette env (paletteCss (statePalette after')))

-- | An answer that rewrote cell sources is one the folder has to be told
-- about: moving a row or renaming a sheet changes what cells say, here and on
-- every sheet that named this one.
saveIfRewritten :: Env -> State -> TabId -> Sexp -> IO ()
saveIfRewritten env state tab payload =
  when (isJust (lookupKey "sources" payload)) (saveTab env state tab)

--
-- The folder
--

sheetFolder :: State -> Tab -> Maybe FilePath
sheetFolder state tab = do
  open <- stateWorkbook state
  pure (workbookSheetDirectory open (tabName tab))

-- | Write a sheet out: the size, the column widths, and a file for every cell
-- that holds anything.
saveTab :: Env -> State -> TabId -> IO ()
saveTab env state tab = forM_ (tabById tab state) $ \found ->
  forM_ (sheetFolder state found) $ \folder -> do
    let model = tabGrid found
        view = modelView model
    outcome <- try (saveSheet folder (Sheet (tabSources found)
                                            (viewRows view) (viewColumns view)
                                            (columnWidths model)))
    case outcome :: Either SomeException () of
      Left _ -> notify env "Could not save the sheet"
      Right () -> pure ()

watchAgain :: Env -> Workbook -> IO ()
watchAgain env open = workbookWatchPaths open >>= watchPathsFor env

quietly :: IO a -> IO ()
quietly action = do
  outcome <- try (void action)
  either (\(_ :: SomeException) -> pure ()) pure outcome

readSheetOrEmpty :: FilePath -> IO Sheet
readSheetOrEmpty folder = do
  isSheet <- isSheetDirectory folder
  if not isSheet then pure emptySheet else do
    outcome <- try (readSheet folder)
    pure (either (\(_ :: SomeException) -> emptySheet) id outcome)

emptySheet :: Sheet
emptySheet = Sheet [] defaultRows defaultColumns []

-- | What to tell the kernel when a sheet is opened.  A sheet is at least the
-- ordinary size on screen, even when it was saved smaller.
openArguments :: String -> Sheet -> [Sexp]
openArguments name sheet =
  [ Str name
  , Num (fromIntegral (max defaultRows (sheetRows sheet)))
  , Num (fromIntegral (max defaultColumns (sheetColumns sheet)))
  , sourcesSexp (sheetCells sheet) ]

sourcesSexp :: [(String, String)] -> Sexp
sourcesSexp sources = list [ Pair (Str name) (Str source) | (name, source) <- sources ]

readSources :: Sexp -> [(String, String)]
readSources value = case toList value of
  Nothing -> []
  Just entries -> [ (name, source) | Pair (Str name) (Str source) <- entries ]

setSource :: String -> Maybe String -> [(String, String)] -> [(String, String)]
setSource name source sources =
  let without = filter ((/= name) . fst) sources
  in case source of
       Nothing -> without
       Just text -> without ++ [(name, text)]

axisName :: Axis -> String
axisName = \case { Row -> "row"; Column -> "column" }

--
-- What somebody asked for
--

acting :: Env -> State -> Action -> Transition State Event
acting env state = \case
  NewWorkbook -> after state $
    askNewWorkbook env "workbook" (stateLocation state) False

  NewScratch -> Transition state $ do
    directory <- scratchLocation
    outcome <- try (createWorkbook directory firstSheetName)
    case outcome :: Either SomeException () of
      Left _ -> notify env "Could not make a scratch workbook" >> pure Nothing
      Right () -> pure (Just (ScratchMade directory))

  OpenWorkbook -> after state (chooseFolder env)

  -- There is nothing to save: the workbook on disk is already this one.
  -- Ctrl+S is too deep a reflex to leave doing nothing silently.
  SaveNothing -> onSheet $ after state $
    notify env "Cellar saves each cell as you edit it"

  CopyTo -> onSheet $ after state $
    askNewWorkbook env suggestion (stateLocation state) True

  AddSheet -> onSheet $ Transition state $ do
    let taken = tabOrder state
        name = nextSheetName taken (length taken + 1)
    askSheetName env Nothing name "Add Sheet"
    pure Nothing

  RenameSheet -> onSheet $ withSheet $ \tab -> after state $
    askSheetName env (Just (tabId tab)) (tabName tab) "Rename Sheet"

  DeleteSheet -> onSheet $ withSheet $ \tab ->
    update env state (TabCloseAsked (tabId tab))

  NextSheet -> onSheet (stepping 1)
  PreviousSheet -> onSheet (stepping (-1))

  RecalculateSheet -> recalculating env state

  ClearCell -> onSheet $ withSheet $ \tab ->
    after state (carryOut env state (tabId tab) (Ask (Clear (activeOf tab))))

  EditCell -> editing env state

  -- An empty cell has no file, and no program can be handed a path that is not
  -- there, so opening one is what brings its file into being.
  OpenCellElsewhere -> onSheet $ withSheet $ \tab -> after state $
    forM_ (sheetFolder state tab) $ \folder -> do
      let r = activeOf tab
      made <- try (touchCell folder (refName r))
      case made :: Either SomeException FilePath of
        Left _ -> notify env (T.pack ("Could not write a file for " ++ refName r))
        Right file -> do
          command <- effectiveEditorCommand (stateConfig state)
          case command of
            Nothing -> openWithDesktop env file
            Just external -> do
              started <- openExternalEditor external folder r
              case started of
                Just program -> notify env (T.pack ("Editing " ++ refName r
                                                    ++ " in " ++ takeFileName program))
                Nothing -> notify env "Could not start the editor in the preferences"

  MoveLine axis delta -> onSheet $ withSheet $ \tab ->
    let from = case axis of
          Row -> refRow (activeOf tab)
          Column -> refColumn (activeOf tab)
    in case moveLine axis from (from + delta) (tabGrid tab) of
         Nothing -> after state $ notify env $ case axis of
           Row -> "The row is already at the edge of the sheet"
           Column -> "The column is already at the edge of the sheet"
         Just (model, command) ->
           let moved = withTab (tabId tab) (\t -> t { tabGrid = model }) state
           in after moved (carryOut env moved (tabId tab) (Ask command))

  InsertLine axis before -> onSheet $ withSheet $ \tab ->
    let at = (case axis of
                Row -> refRow (activeOf tab)
                Column -> refColumn (activeOf tab)) + (if before then 0 else 1)
    in case insertLine axis at (tabGrid tab) of
         Nothing -> stay state
         Just (model, command) ->
           let grown = withTab (tabId tab) (\t -> t { tabGrid = model }) state
           in after grown (carryOut env grown (tabId tab) (Ask command))

  OpenRecentAt path -> Transition state (opening path AsUsual)

  ClearRecent ->
    let updated = (stateConfig state) { recentWorkbooks = [] }
    in after state { stateRecent = [], stateConfig = updated } $ do
         quietly (saveConfig updated)
         fillRecentMenu env []
         notify env "Cleared the recent workbooks"

  Quit -> Exit

  Preferences -> after state (openPreferences env (stateConfig state))
  Shortcuts -> after state (showShortcuts env)
  About -> after state (showAbout env)
  where
    -- An action that means nothing with no workbook on screen, and does
    -- nothing there.
    onSheet done = if sheetShowing state then done else stay state
    withSheet done = maybe (stay state) done (currentTab state)
    activeOf = modelActive . tabGrid
    suggestion = case (stateWorkbook state, stateScratch state) of
      (Just open, False) -> workbookName open
      _ -> "workbook"
    stepping delta = case (currentTab state >>= \t -> tabPosition (tabId t) state) of
      Nothing -> stay state
      Just position ->
        let next = position + delta
        in if next < 0 || next >= length (stateTabs state)
             then stay state
             else update env state (TabSelected (tabId (stateTabs state !! next)))

-- | Read a workbook off the disk, and say what to do with it.
opening :: FilePath -> Opening -> IO (Maybe Event)
opening path how = do
  resolved <- resolveWorkbook path
  case resolved of
    Nothing -> pure (Just (WorkbookRefused path))
    Just open -> do
      names <- workbookSheetNames open
      showing <- workbookActiveSheet open
      sheets <- forM names $ \name ->
        (,) name <$> readSheetOrEmpty (workbookSheetDirectory open name)
      pure (Just (WorkbookRead open how sheets showing))

-- | A name no sheet in the workbook has yet.
nextSheetName :: [String] -> Int -> String
nextSheetName taken n
  | candidate `elem` taken = nextSheetName taken (n + 1)
  | otherwise = candidate
  where candidate = "Sheet " ++ show n

editing :: Env -> State -> Transition State Event
editing env state = case currentTab state of
  Nothing -> stay state
  Just tab -> after state $
    carryOut env state (tabId tab) (Edit (modelActive (tabGrid tab)))

recalculating :: Env -> State -> Transition State Event
recalculating env state = case currentTab state of
  Nothing -> stay state
  Just tab -> after state $
    asking env [("recalculate", [Str (tabName tab)], Snapshot (tabId tab) (Just "Recalculated"))]

-- | A tab that was asked about and stays.  The answer is a command to the tab
-- view, which acts on it once.
keeping :: TabId -> State -> State
keeping tab state =
  state { stateCloseAnswer = Just (T.pack (show tab), False) }

-- | A sheet of the window for every sheet on disk, each holding what its
-- folder said.
makeTabs :: NonEmpty (String, Sheet) -> State -> (State, NonEmpty Tab)
makeTabs sheets state = mapAccumL one state sheets
  where
    one s (name, sheet) =
      let (next, tab) = freshTab name (emptyView (max defaultRows (sheetRows sheet))
                                                 (max defaultColumns (sheetColumns sheet)))
                                 s
      in ( next
         , tab { tabSources = sheetCells sheet
               , tabGrid = withWidths (sheetWidths sheet) (tabGrid tab) } )

focusGrid :: Env -> State -> IO ()
focusGrid _ _ = pure ()

-- | Write every sheet of this workbook into a folder of its own.
copyWorkbookTo :: State -> FilePath -> IO ()
copyWorkbookTo state directory = case stateTabs state of
  [] -> throwIO (StoreError "There is nothing to copy")
  (first : _) -> do
    createWorkbook directory (tabName first)
    fresh <- resolveWorkbook directory >>= \case
      Just open -> pure open
      Nothing -> throwIO (StoreError ("Could not open " ++ directory))
    forM_ (stateTabs state) $ \tab ->
      unless (tabName tab == tabName first) (void (addWorkbookSheet fresh (tabName tab)))
    forM_ (stateTabs state) $ \tab -> do
      let view = modelView (tabGrid tab)
      saveSheet (workbookSheetDirectory fresh (tabName tab))
                (Sheet (tabSources tab) (viewRows view) (viewColumns view)
                       (columnWidths (tabGrid tab)))
    writeWorkbookIndex directory (tabOrder state) (tabName <$> currentTab state)

-- | Make a git repository of the workbook.  Around the workbook rather than
-- around any one sheet, which is the whole reason a workbook exists.
gitInit :: Env -> FilePath -> IO ()
gitInit env directory = do
  outcome <- try (callProcess "git" ["init", "--quiet", directory])
  case outcome :: Either SomeException () of
    Left _ -> notify env "The folder was made, but git could not be run"
    Right () -> pure ()

-- | A workbook to think in, out of the way under the data directory.
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
timestamp = do
  now <- GLib.dateTimeNewNowLocal
  case now of
    Nothing -> pure "sheet"
    Just moment -> do
      formatted <- GLib.dateTimeFormat moment "%Y-%m-%d-%H%M%S"
      pure (maybe "sheet" T.unpack formatted)

sheetOf :: State -> TabId -> Maybe Tab
sheetOf state tab = tabById tab state
