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

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Traversable (mapAccumL)
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeFileName)


import Cellar.App.Effect
import Cellar.App.Event
import Cellar.App.State
import Cellar.Config
import Cellar.Grid.Model
import Cellar.Op (Op)
import qualified Cellar.Op as Op
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

-- | A state, and one thing to do about it that answers with an event.
after :: State -> Effect -> Step
after = now

-- | The name a sheet's layout is written under, so that dragging a column of
-- one sheet does not cancel the write of another.
layoutKey :: TabId -> Text
layoutKey (TabId n) = "layout:" <> T.pack (show n)

update :: State -> Event -> Step
update state = \case

  --
  -- The grid
  --

  GridSaid tab event -> case tabById tab state of
    Nothing -> stay state
    Just found ->
      let (model, outs) = gridEvent event (tabGrid found)
          moved = withTab tab (\t -> t { tabGrid = model }) state
      in Step moved (concatMap (carrying moved tab) outs)

  LineChosen tab axis index -> stay $ withTab tab
    (\t -> t { tabGrid = fromMaybe (tabGrid t) (selectLine axis index (tabGrid t)) })
    state

  DragBegun tab axis index -> dragging tab (Just (axis, index, index)) state
  DragMoved tab index -> case dragOf tab state of
    Just (axis, from, _) -> dragging tab (Just (axis, from, index)) state
    Nothing -> stay state
  DragCancelled tab -> dragging tab Nothing state
  DragDropped tab landing -> case dragOf tab state of
    Nothing -> stay state
    Just (axis, from, _) ->
      let cleared = withTab tab (\t -> t { tabGrid = withDrag Nothing (tabGrid t) }) state
          moved = tabById tab cleared >>= \t -> moveLine axis from landing (tabGrid t)
      in case moved of
           Nothing -> dragging tab Nothing cleared
           Just (model, command) ->
             let after' = withTab tab (\t -> t { tabGrid = model }) cleared
             in Step after' (Now (ShowDrag tab Nothing)
                             : carrying after' tab (Ask command))

  --
  -- The bars and the tabs
  --

  EditPressed -> editing state
  RecalculatePressed -> recalculating state

  TabSelected tab ->
    let chosen = selectTab tab state
        remember = [ SetActiveSheet open (tabName t)
                   | not (stateLoading chosen)
                   , open <- maybeToList (stateWorkbook chosen)
                   , t <- maybeToList (tabById tab chosen) ]
    in Step chosen (map Now (remember ++ [FocusGrid]))

  TabsReordered order ->
    let ordered = orderTabs order state
    in Step ordered [ Now (SetSheetOrder open (tabOrder ordered))
                    | not (stateLoading ordered)
                    , open <- maybeToList (stateWorkbook ordered) ]

  -- A tab is a sheet of the workbook rather than a view of one, so closing it
  -- is deleting it -- which is worth being asked about, and worth refusing
  -- when it would leave the workbook with nothing in it.
  TabCloseAsked tab -> case tabById tab state of
    Nothing -> stay state
    Just found
      | length (stateTabs state) <= 1 ->
          after (keeping tab state) (Notify "A workbook keeps at least one sheet")
      | otherwise -> after (state { stateCloseAnswer = Nothing })
          (AskToDeleteSheet tab (tabName found))

  TabKept tab -> stay (keeping tab state)

  SheetDeleted tab name -> case stateWorkbook state of
    Nothing -> stay state
    Just open ->
      after (forgetTab tab (keeping tab state)) (RemoveSheetFolder open tab name)

  -- The sheets that are left may have been naming the one that has gone, so
  -- the kernel says what they come to now that they cannot.
  SheetRemoved open name ->
    Step state (map Now [ Request [(Op.Close name, Closed)]
                        , Watch open
                        , Notify (T.pack ("Deleted " ++ name)) ])

  --
  -- The kernel
  --

  KernelSaid tag payload -> answered state tag payload

  KernelRefused _ why -> after state (Notify (T.pack why))

  -- A cell that is not going to finish looks, from out here, exactly like one
  -- that is merely slow -- that is the halting problem, and Cellar is not
  -- going to solve it on a timer.  So this decides nothing: when the kernel
  -- has been sitting on something for longer than anybody would expect, it
  -- asks.
  Stalled True
    | stateKernelAnswered state
    , not (stateAskingAboutKernel state)
    , not (stateWaitingOnPurpose state) ->
        after state { stateAskingAboutKernel = True } AskAboutKernel
    | otherwise -> stay state

  Stalled False
    | stateAskingAboutKernel state || stateWaitingOnPurpose state ->
        after state { stateAskingAboutKernel = False
                    , stateWaitingOnPurpose = False }
              NeverMindKernel
    | otherwise -> stay state

  NeverMind -> stay state { stateAskingAboutKernel = False
                          , stateWaitingOnPurpose = True }

  -- Stopping restarts the kernel and reopens every sheet as it stands on disk.
  Stopped -> Step state { stateAskingAboutKernel = False
                        , stateKernelAnswered = False }
    (map Now [ RestartKernel
             , Request [(Op.Ping, Pinged)]
             , Emit DiskChanged
             , Notify "Stopped the cell and restarted the kernel" ])

  --
  -- What somebody asked for
  --

  Act action -> acting state action

  --
  -- The folder on disk
  --

  DiskChanged ->
    Step state [ Now (ReadSheetsOf open) | open <- maybeToList (stateWorkbook state) ]

  -- A sheet arrived or left -- somebody's commit, most likely.
  SheetsOnDisk sheets
    | map fst sheets /= tabOrder state ->
        Step state (map Now
          (  [ Watch open | open <- maybeToList (stateWorkbook state) ]
          ++ [ Emit (SheetsRead sheets (tabName <$> currentTab state))
             , Notify "Reloaded \8212 the sheets changed on disk" ]))

  -- The same sheets, so only their cells can have changed.
  SheetsOnDisk sheets ->
    let changed = [ (t, sheet)
                  | (name, sheet) <- sheets
                  , Just t <- [tabNamed name state]
                  , sheetCells sheet /= sortOn fst (tabSources t) ]
    in if null changed then stay state else
         let taken = foldr (\(t, sheet) s ->
                              withTab (tabId t) (\tab -> tab { tabSources = sheetCells sheet }) s)
                           state changed
         in Step taken (map Now
              [ Request [ ( opening (tabName t) sheet
                          , Reopened (tabId t) (modelActive (tabGrid t)) )
                        | (t, sheet) <- changed ]
              , Notify "Reloaded \8212 the workbook changed on disk" ])

  --
  -- Opening, making and copying workbooks
  --

  -- A workbook with no sheets in it is not a workbook the window can show, so
  -- it is refused here rather than carried as a state nothing else allows for.
  WorkbookRead open how sheets showing -> case NE.nonEmpty sheets of
    Nothing -> after state (Notify (T.pack (workbookName open ++ " has no sheets")))
    Just some ->
      let (counted, tabs) = makeTabs some state
          scratch = case how of
            AsUsual -> False
            AsScratch -> True
            AsBefore -> maybe False openScratch (stateOpen state)
          loaded = (opened open tabs showing scratch counted)
            { statePage = SheetPage, stateLoading = False, stateFresh = False
            , stateCloseAnswer = Nothing }
      in Step loaded (map Now
           (  [ Request [ ( opening (tabName t) sheet
                          , Opened (tabId t) (stateFresh state) )
                        | (t, sheet) <- zip (NE.toList tabs) (map snd sheets) ]
              , Watch open ]
           ++ [ Emit (Remembered (workbookRoot open)) | how == AsUsual ]
           ++ [ FocusGrid ]))

  SheetsRead sheets showing -> case stateWorkbook state of
    Nothing -> stay state
    Just open -> after state (Emit (WorkbookRead open AsBefore sheets showing))

  WorkbookRefused path -> after state
    (Notify (T.pack (takeFileName path ++ " is not a Cellar workbook")))

  Remembered path ->
    let updated = (stateConfig state) { recentWorkbooks = rememberRecent path (stateRecent state) }
    in Step state { stateRecent = recentWorkbooks updated, stateConfig = updated }
         (map Now [SaveConfig updated, FillRecent (recentWorkbooks updated)])

  -- A workbook to think in is opened like any other, but it does not join the
  -- list of the ones opened lately.
  ScratchMade path -> after state (ReadWorkbookAt path AsScratch)

  WorkbookMade path copying wantsGit -> after state { stateFresh = True } $
    if copying
      then CopyWorkbook path [ (tabName t, sheetOfTab state t) | t <- stateTabs state ]
                             (tabName <$> currentTab state) wantsGit
      else MakeWorkbook path wantsGit

  FolderChosen path -> after state (Emit (Act (OpenRecentAt path)))

  --
  -- Sheets
  --

  SheetNamed Nothing name -> case stateWorkbook state of
    Nothing -> stay state
    Just open -> after state (AddSheetFolder open name)

  SheetNamed (Just tab) name -> case (stateWorkbook state, tabById tab state) of
    (Just open, Just found) ->
      after state (RenameSheetFolder open tab (tabName found) name)
    _ -> stay state

  SheetAdded open name ->
    let (counted, tab) = freshTab name (emptyView defaultRows defaultColumns) state
        grown = addTab tab (withWorkbook open counted)
    in Step grown (map Now
         [ Request [(opening name emptySheet, Opened (tabId tab) True)]
         , Watch open
         , Notify (T.pack ("Added " ++ name)) ])

  -- Cells elsewhere say Summary!B2, so a sheet that changes its name changes
  -- what every one of them has to say.  The kernel rewrites them and hands
  -- back the sources of every sheet it touched.
  SheetRenamed open tab old new ->
    Step (withTab tab (\t -> t { tabName = new }) (withWorkbook open state))
      (map Now [ Request [(Op.Rename old new, Snapshot tab Nothing)]
               , Watch open ])

  --
  -- The cell editor
  --

  CellEdited tab r text -> case tabById tab state of
    Nothing -> stay state
    Just found -> after state $
      Request [(Op.SetCell (tabName found) (refName r) text, CellSet tab (refName r))]

  EditorCommandSet command ->
    let updated = (stateConfig state) { externalEditorCommand = command }
    in after state { stateConfig = updated } (SaveConfig updated)

  -- What is watched follows from what is open, so the window holds the list
  -- and the loop starts and stops the watching to match.
  Watching paths -> stay state { stateWatching = paths }

  PreviewWanted tab r mine text -> Step state $ asked state tab $ \name ->
    (Op.Preview name (refName r) text, Previewing tab mine)

  Toast message -> after state (Notify message)

  WindowClosing -> Stop

--
-- The grid's questions
--

-- | What one of the grid's questions asks for.
--
-- All of them are the plain thing except the layout, which is written under a
-- name of its own so that a drag costs one write rather than one per frame.
-- Every whole-sheet write is the primary file and a file per cell, which is
-- four milliseconds on a sheet of two hundred, and GTK reports a column's
-- width on every step of a drag.
carrying :: State -> TabId -> GridOut -> [Doing]
carrying state tab out = case out of
  Edit r ->
    [ Now (OpenCellEditor tab r (lookup (refName r) (tabSources found)))
    | found <- maybeToList (tabById tab state) ]
  Ask Layout ->
    [ Settle (layoutKey tab) (SaveSheet folder sheet)
    | (folder, sheet) <- maybeToList (sheetToWrite state tab) ]
  Ask (Clear r) -> asked state tab $ \name ->
    (Op.SetCell name (refName r) "", CellSet tab (refName r))
  Ask (Move axis from to) -> asked state tab $ \name ->
    (Op.Move name axis from to, Snapshot tab Nothing)
  Ask (Insert axis at) -> asked state tab $ \name ->
    (Op.Insert name axis at, Snapshot tab Nothing)
  Ask (Delete axis at) -> asked state tab $ \name ->
    (Op.Delete name axis at, Snapshot tab Nothing)

-- | One request about a sheet, which is nothing at all when the tab has gone.
asked :: State -> TabId -> (String -> (Op, Tag)) -> [Doing]
asked state tab request =
  [ Now (Request [request (tabName found)]) | found <- maybeToList (tabById tab state) ]

dragOf :: TabId -> State -> Maybe (Axis, Int, Int)
dragOf tab state = tabById tab state >>= modelDrag . tabGrid

dragging :: TabId -> Maybe (Axis, Int, Int) -> State -> Step
dragging tab drag state =
  now (withTab tab (\t -> t { tabGrid = withDrag drag (tabGrid t) }) state)
      (ShowDrag tab drag)

--
-- What the kernel said
--

answered :: State -> Tag -> Sexp -> Step
answered state tag payload = case tag of
  -- The cell editor asks for previews of its own, and is handed them
  -- elsewhere.
  Ignored -> stay state

  Pinged -> after state { stateKernelAnswered = True } MarkReady

  -- A workbook Cellar made a moment ago has a sheet folder that says nothing
  -- about its size, and the sheet on screen is the ordinary 100 by 26.
  -- Writing it out once is what makes the folder say what the window says.
  Opened tab fresh ->
    let taken = snapshotInto tab payload state
        started = withTab tab (\t -> t { tabGrid = fromMaybe (tabGrid t)
                                           (withActive (Ref 0 0) (tabGrid t)) }) taken
    in Step started (map Now
         (  paletteIfNew state started
         ++ if fresh then writeSheet started tab else writeIfRewritten started tab payload))

  Reopened tab kept ->
    let taken = snapshotInto tab payload state
        back = withTab tab (\t -> t { tabGrid = fromMaybe (tabGrid t)
                                        (withActive kept (tabGrid t)) }) taken
    in Step back (map Now (paletteIfNew state back ++ writeIfRewritten back tab payload))

  Snapshot tab said ->
    let taken = snapshotInto tab payload state
    in Step taken (map Now
         (  paletteIfNew state taken
         ++ writeIfRewritten taken tab payload
         ++ map Notify (maybeToList said) ))

  CellSet tab name ->
    let source = lookupKey "source" payload >>= asString
        kept = withTab tab (\t -> t { tabSources = setSource name source (tabSources t) })
                           state
        taken = snapshotInto tab payload kept
    in Step taken (map Now
         (  paletteIfNew state taken
         ++ [ SaveCell folder name source
            | found <- maybeToList (tabById tab taken)
            , folder <- maybeToList (sheetFolder taken found) ]
         ++ writeIfRewritten taken tab payload ))

  -- The editor's own question, which changes nothing here.
  Previewing _ mine -> after state $ ShowPreview mine
    (fromMaybe "" (lookupKey "display" payload >>= asString))
    (maybe False asBool (lookupKey "error" payload))

  Closed -> stay (othersFrom payload state)

  Renamed tab -> stay (snapshotInto tab payload state)

-- | Write the colours out, when a snapshot brought one the window had not
-- seen.  The stylesheet is GTK's to hold, which is why this is the one part of
-- taking a snapshot that is not a change to a value.
paletteIfNew :: State -> State -> [Effect]
paletteIfNew before after' =
  [ ShowPalette (paletteCss (statePalette after'))
  | statePalette after' /= statePalette before ]

-- | An answer that rewrote cell sources is one the folder has to be told
-- about: moving a row or renaming a sheet changes what cells say, here and on
-- every sheet that named this one.
writeIfRewritten :: State -> TabId -> Sexp -> [Effect]
writeIfRewritten state tab payload =
  if isJust (lookupKey "sources" payload) then writeSheet state tab else []

-- | A whole sheet, as a folder to write into and what to write.
writeSheet :: State -> TabId -> [Effect]
writeSheet state tab = [ SaveSheet folder sheet
                       | (folder, sheet) <- maybeToList (sheetToWrite state tab) ]

sheetToWrite :: State -> TabId -> Maybe (FilePath, Sheet)
sheetToWrite state tab = do
  found <- tabById tab state
  folder <- sheetFolder state found
  pure (folder, sheetOfTab state found)

-- | A tab as the folder holds it.
sheetOfTab :: State -> Tab -> Sheet
sheetOfTab _ tab =
  let view = modelView (tabGrid tab)
  in Sheet (tabSources tab) (viewRows view) (viewColumns view)
           (columnWidths (tabGrid tab))

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

--
-- The folder
--

sheetFolder :: State -> Tab -> Maybe FilePath
sheetFolder state tab = do
  open <- stateWorkbook state
  pure (workbookSheetDirectory open (tabName tab))

emptySheet :: Sheet
emptySheet = Sheet [] defaultRows defaultColumns []

-- | Hand a sheet over to the kernel.  A sheet is at least the ordinary size on
-- screen, even when it was saved smaller.
opening :: String -> Sheet -> Op
opening name sheet = Op.Open name
  (max defaultRows (sheetRows sheet))
  (max defaultColumns (sheetColumns sheet))
  (sheetCells sheet)

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

--
-- What somebody asked for
--

acting :: State -> Action -> Step
acting state = \case
  NewWorkbook -> after state (AskNewWorkbook "workbook" (stateLocation state) False)

  NewScratch -> after state MakeScratch

  OpenWorkbook -> after state ChooseFolder

  -- There is nothing to save: the workbook on disk is already this one.
  -- Ctrl+S is too deep a reflex to leave doing nothing silently.
  SaveNothing -> onSheet $ after state (Notify "Cellar saves each cell as you edit it")

  CopyTo -> onSheet $ after state (AskNewWorkbook suggestion (stateLocation state) True)

  AddSheet -> onSheet $
    let taken = tabOrder state
    in after state (AskSheetName Nothing
                      (nextSheetName taken (length taken + 1)) "Add Sheet")

  RenameSheet -> onSheet $ withSheet $ \tab ->
    after state (AskSheetName (Just (tabId tab)) (tabName tab) "Rename Sheet")

  DeleteSheet -> onSheet $ withSheet $ \tab ->
    update state (TabCloseAsked (tabId tab))

  NextSheet -> onSheet (stepping 1)
  PreviousSheet -> onSheet (stepping (-1))

  RecalculateSheet -> recalculating state

  ClearCell -> onSheet $ withSheet $ \tab ->
    Step state (carrying state (tabId tab) (Ask (Clear (activeOf tab))))

  EditCell -> editing state

  -- An empty cell has no file, and no program can be handed a path that is not
  -- there, so opening one is what brings its file into being.
  OpenCellElsewhere -> onSheet $ withSheet $ \tab ->
    Step state [ Now (OpenCellFile folder (activeOf tab) (stateConfig state))
               | folder <- maybeToList (sheetFolder state tab) ]

  MoveLine axis delta -> onSheet $ withSheet $ \tab ->
    let from = along axis tab
    in case moveLine axis from (from + delta) (tabGrid tab) of
         Nothing -> after state $ Notify $ case axis of
           Row -> "The row is already at the edge of the sheet"
           Column -> "The column is already at the edge of the sheet"
         Just (model, command) -> asked' tab model command

  InsertLine axis before -> onSheet $ withSheet $ \tab ->
    let at = along axis tab + (if before then 0 else 1)
    in case insertLine axis at (tabGrid tab) of
         Nothing -> stay state
         Just (model, command) -> asked' tab model command

  DeleteLine axis -> onSheet $ withSheet $ \tab ->
    case deleteLine axis (along axis tab) (tabGrid tab) of
      Nothing -> after state $ Notify $ case axis of
        Row -> "A sheet has to keep one row"
        Column -> "A sheet has to keep one column"
      Just (model, command) -> asked' tab model command

  OpenRecentAt path -> after state (ReadWorkbookAt path AsUsual)

  ClearRecent ->
    let updated = (stateConfig state) { recentWorkbooks = [] }
    in Step state { stateRecent = [], stateConfig = updated }
         (map Now [ SaveConfig updated, FillRecent []
                  , Notify "Cleared the recent workbooks" ])

  Quit -> Stop

  Preferences -> after state (OpenPreferences (stateConfig state))
  Shortcuts -> after state ShowShortcuts
  About -> after state ShowAbout
  where
    -- An action that means nothing with no workbook on screen, and does
    -- nothing there.
    onSheet done = if sheetShowing state then done else stay state
    withSheet done = maybe (stay state) done (currentTab state)
    activeOf = modelActive . tabGrid
    along axis tab = case axis of
      Row -> refRow (activeOf tab)
      Column -> refColumn (activeOf tab)
    -- A line that moved, and the request that tells the kernel so.
    asked' tab model command =
      let changed = withTab (tabId tab) (\t -> t { tabGrid = model }) state
      in Step changed (carrying changed (tabId tab) (Ask command))
    suggestion = case (stateWorkbook state, stateScratch state) of
      (Just open, False) -> workbookName open
      _ -> "workbook"
    stepping delta = case (currentTab state >>= \t -> tabPosition (tabId t) state) of
      Nothing -> stay state
      Just position ->
        let next = position + delta
        in if next < 0 || next >= length (stateTabs state)
             then stay state
             else update state (TabSelected (tabId (stateTabs state !! next)))

-- | A name no sheet in the workbook has yet.
nextSheetName :: [String] -> Int -> String
nextSheetName taken n
  | candidate `elem` taken = nextSheetName taken (n + 1)
  | otherwise = candidate
  where candidate = "Sheet " ++ show n

editing :: State -> Step
editing state = case currentTab state of
  Nothing -> stay state
  Just tab -> Step state (carrying state (tabId tab) (Edit (modelActive (tabGrid tab))))

recalculating :: State -> Step
recalculating state = case currentTab state of
  Nothing -> stay state
  Just tab -> after state $
    Request [(Op.Recalculate (tabName tab), Snapshot (tabId tab) (Just "Recalculated"))]

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

sheetOf :: State -> TabId -> Maybe Tab
sheetOf state tab = tabById tab state
