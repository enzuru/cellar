{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The spreadsheet grid.
--
-- GTK4 has no spreadsheet widget, so this builds one out of GtkColumnView: one
-- GtkColumnViewColumn per spreadsheet column, each with a signal factory whose
-- callbacks close over the column's index.
--
-- The trick that keeps it simple is that the list model carries no data at
-- all.  It is a GtkStringList of row numbers, purely to give the view the right
-- number of rows; what goes in a cell is looked up at bind time from the
-- 'View'.  That means no custom GObject subclass and no C.
--
-- Nothing here evaluates anything or holds a sheet.  What it draws is a
-- 'View': strings, alignments and colours that the kernel worked out and sent
-- over.  Binding a cell is a map lookup and no more, which is what it has to
-- be -- binding happens on every scroll frame, and the thing that used to
-- answer these questions is now another process.  When the grid wants the
-- sheet changed it says so through 'Command' and waits to be handed a new view.
module Cellar.Grid
  ( Grid
  , Command (..)
  , newGrid
  , gridWidget
  , gridSetView
  , gridCurrentView
  , gridActiveRef
  , gridSetActive
  , gridRefresh
  , gridFocus
  , gridColumnWidths
  , gridSetColumnWidths
  , gridMoveLine
  , gridInsertLine
  ) where

import Control.Monad (filterM, forM, forM_, void, when)
import Data.IORef
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)

import Data.GI.Base
import Data.GI.Base.Overloading (IsDescendantOf)
import Foreign.Ptr (castPtr)
import qualified GI.Gdk as Gdk
import qualified GI.GObject as GObject
import qualified GI.Gio as Gio
import qualified GI.Graphene as Graphene
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import Cellar.Ref
import Cellar.View

-- | What the grid asks for when it wants the sheet changed.  It cannot change
-- one itself -- the sheet is in another process -- so it says what it wants and
-- waits for a new view.
data Command
  = Layout                 -- ^ A column was dragged wider.
  | Clear Ref              -- ^ Delete was pressed on a cell.
  | Move Axis Int Int      -- ^ A row or column should move.
  | Insert Axis Int        -- ^ A row or column should be opened.
  deriving (Eq, Show)

-- | A realised cell widget and the column it currently stands for.  The index
-- is a reference because inserting a column renumbers every column to its
-- right, and the widgets have to follow.
data CellWidget = CellWidget
  { cellItem :: Gtk.ColumnViewCell
  , cellLabel :: Gtk.Label
  , cellColumn :: IORef Int
  }

-- | A drag in progress.
--
-- GtkColumnView can reorder its own columns by dragging a header, but that
-- moves the view's columns and not the sheet behind them: the letters would
-- come out in the wrong order and A1 would no longer be the cell in the
-- corner.  So the view's own reordering stays off (@reorderable: false@ in
-- cellar.blp) and the drag is ours, ending in the same move the keyboard asks
-- for.
--
-- It is a GtkGestureDrag rather than GTK's drag-and-drop.  haskell-gi could
-- reach the real thing -- it marshals the @GValue@ and @GType@ that put it out
-- of Guile's reach -- but nothing is being transferred here: what is dragged
-- never leaves the process, so a gesture, which needs no content at all, is
-- still the better fit.
data Drag = Drag
  { dragAxis :: Axis
  , dragFrom :: Int
  , dragWidget :: Gtk.Widget
  , dragStartX :: Double
  , dragStartY :: Double
  , dragTarget :: Int
  }

data Grid = Grid
  { gridWidget :: Gtk.ColumnView
  , gridViewRef :: IORef View
  , gridActive :: IORef Ref
    -- | Every cell widget a factory has handed us.  Cell widgets are recycled
    -- as you scroll, so this stays proportional to what is on screen rather
    -- than to the size of the sheet.
  , gridCells :: IORef [CellWidget]
    -- | The GtkStringList standing in for the rows, and how many entries it
    -- has: the view grows a row by growing this.
  , gridRowList :: Gtk.StringList
  , gridRowCount :: IORef Int
    -- | The sheet's columns as index and widget, in column order.
  , gridColumnRefs :: IORef [(IORef Int, Gtk.ColumnViewColumn)]
  , gridOnSelect :: Ref -> IO ()
  , gridOnActivate :: Ref -> IO ()
  , gridOnCommand :: Command -> IO ()
    -- | The colours the stylesheet already knows about, and the provider
    -- holding them.
  , gridPalette :: IORef (M.Map (Maybe String, Maybe String) Text)
  , gridPaletteProvider :: Gtk.CssProvider
  , gridLineMenu :: Maybe Gio.MenuModel
    -- | The column header widgets, each paired with its column's index.
    --
    -- A pairing rather than an order: GtkColumnView adds an inserted column's
    -- header at the end of the header row whatever the column's position, so
    -- the header row is not in column order.  See 'installHeaderDrag'.
  , gridTitles :: IORef [(Gtk.Widget, IORef Int)]
  , gridDrag :: IORef (Maybe Drag)
  }

defaultColumnWidth, gutterWidth :: Int
defaultColumnWidth = 104
gutterWidth = 60

-- | What @gtk_column_view_cell_get_position@ returns for an unbound cell.
invalidPosition :: Word32
invalidPosition = 0xffffffff

-- | Turn a GtkColumnView into a grid showing a view.
newGrid
  :: Gtk.ColumnView
  -> View
  -> Maybe Gio.MenuModel
  -> (Ref -> IO ())     -- ^ the active cell changed
  -> (Ref -> IO ())     -- ^ a cell was activated (double-click, Enter)
  -> (Command -> IO ())
  -> IO Grid
newGrid widget view lineMenu onSelect onActivate onCommand = do
  rows <- Gtk.stringListNew (Just (map (T.pack . show) [0 .. viewRows view - 1]))
  provider <- Gtk.cssProviderNew
  display <- Gdk.displayGetDefault
  forM_ display $ \d ->
    Gtk.styleContextAddProviderForDisplay d provider 700
  grid <- Grid widget
    <$> newIORef view
    <*> newIORef (Ref 0 0)
    <*> newIORef []
    <*> pure rows
    <*> newIORef (viewRows view)
    <*> newIORef []
    <*> pure onSelect
    <*> pure onActivate
    <*> pure onCommand
    <*> newIORef M.empty
    <*> pure provider
    <*> pure lineMenu
    <*> newIORef []
    <*> newIORef Nothing
  selection <- Gtk.noSelectionNew (Just rows)
  Gtk.columnViewSetModel widget (Just selection)
  -- A narrow leading column of row numbers, standing in for the row headers
  -- GtkColumnView does not have.
  gutterIndex <- newIORef (-1)
  gutter <- makeColumn grid gutterIndex "" gutterWidth
  Gtk.columnViewAppendColumn widget gutter
  forM_ [0 .. viewColumns view - 1] (addColumn grid)
  installKeyHandling grid
  -- The header widgets may not exist yet; if they do not, realising will make
  -- them and this runs again.
  installHeaderDrag grid
  _ <- on widget #realize (installHeaderDrag grid)
  pure grid

-- | One spreadsheet column.
makeColumn :: Grid -> IORef Int -> Text -> Int -> IO Gtk.ColumnViewColumn
makeColumn grid index title width = do
  factory <- Gtk.signalListItemFactoryNew
  -- The factory hands over a plain GObject; since GTK 4.12 what it actually is
  -- for a column view is a GtkColumnViewCell, which is the thing that knows
  -- which row it has been recycled onto.
  _ <- on factory #setup $ \item ->
    unsafeCastTo Gtk.ColumnViewCell item >>= setupCell grid index
  _ <- on factory #bind $ \item ->
    unsafeCastTo Gtk.ColumnViewCell item >>= bindCell grid index
  column <- Gtk.columnViewColumnNew (Just title) (Just factory)
  Gtk.columnViewColumnSetFixedWidth column (fromIntegral width)
  n <- readIORef index
  Gtk.columnViewColumnSetResizable column (n >= 0)
  -- Connected after the width above is set, so building a column is not itself
  -- a change.  Dragging a header's edge is GTK's own gesture, which Cellar
  -- deliberately stays out of; this property is where it shows up.
  _ <- on column (PropertyNotify #fixedWidth) $ \_ -> gridOnCommand grid Layout
  pure column

-- Cell widgets

setupCell :: Grid -> IORef Int -> Gtk.ColumnViewCell -> IO ()
setupCell grid index cell = do
  column <- readIORef index
  label <- new Gtk.Label
    [ #hexpand := True
    , #xalign := if column < 0 then 0.5 else 0.0
    , #ellipsize := Pango.EllipsizeModeEnd
    , #singleLineMode := True
    ]
  Gtk.widgetAddCssClass label (if column < 0 then "cellar-gutter" else "cellar-cell")
  Gtk.columnViewCellSetChild cell (Just label)
  if column < 0
    then do
      -- The gutter is the row's handle: it is the one part of a row that holds
      -- no data, so a click on it can only mean "this row".
      Gtk.widgetSetCursorFromName label (Just "grab")
      installRowDrag grid cell label
      installLineClick grid label Row (rowUnder grid cell)
    else do
      gesture <- Gtk.gestureClickNew
      _ <- on gesture #pressed $ \presses _ _ ->
        cellPressed grid index cell (fromIntegral presses)
      Gtk.widgetAddController label gesture
      installLineClick grid label Column (columnUnder index)
  modifyIORef' (gridCells grid) (CellWidget cell label index :)

bindCell :: Grid -> IORef Int -> Gtk.ColumnViewCell -> IO ()
bindCell grid index cell = do
  child <- Gtk.columnViewCellGetChild cell
  forM_ child $ \widget -> do
    label <- unsafeCastTo Gtk.Label widget
    position <- Gtk.columnViewCellGetPosition cell
    live <- livePosition grid position
    column <- readIORef index
    when live $ paintCell grid label column (fromIntegral position)

livePosition :: Grid -> Word32 -> IO Bool
livePosition grid position = do
  view <- readIORef (gridViewRef grid)
  pure (position /= invalidPosition && fromIntegral position < viewRows view)

rowUnder :: Grid -> Gtk.ColumnViewCell -> IO (Maybe Int)
rowUnder grid cell = do
  position <- Gtk.columnViewCellGetPosition cell
  live <- livePosition grid position
  pure (if live then Just (fromIntegral position) else Nothing)

columnUnder :: IORef Int -> IO (Maybe Int)
columnUnder index = do
  column <- readIORef index
  pure (if column >= 0 then Just column else Nothing)

-- | Draw one cell.
--
-- Every line of this reads from the view and nothing else.  That is what makes
-- scrolling free: the answers were computed once, by another process, and
-- painting them again costs no word to anybody.
paintCell :: Grid -> Gtk.Label -> Int -> Int -> IO ()
paintCell grid label column row
  | column < 0 = Gtk.labelSetLabel label (T.pack (show (row + 1)))
  | otherwise = do
      view <- readIORef (gridViewRef grid)
      active <- readIORef (gridActive grid)
      let r = Ref row column
      Gtk.labelSetLabel label (T.pack (displayAt view r))
      -- Numbers right-align, everything else left-aligns, as in any sheet.
      Gtk.labelSetXalign label (if numberAt view r then 1.0 else 0.0)
      case errorAt view r of
        Just why -> do
          Gtk.widgetAddCssClass label "cellar-error"
          Gtk.widgetSetTooltipText label (Just (T.pack why))
        Nothing -> do
          Gtk.widgetRemoveCssClass label "cellar-error"
          Gtk.widgetSetTooltipText label (T.pack <$> sourceAt view r)
      if r == active
        then Gtk.widgetAddCssClass label "cellar-active"
        else Gtk.widgetRemoveCssClass label "cellar-active"
      paintStyle grid label (styleAt view r)

-- | A cell's own colours.
--
-- A cell can ask to be drawn in any colour it likes, and GTK has no way to set
-- one on a widget except through the stylesheet.  So each distinct pair of
-- colours becomes a class, the classes are collected into one provider, and
-- the provider is rewritten when a colour turns up that it has not seen.
paintStyle :: Grid -> Gtk.Label -> (Maybe String, Maybe String) -> IO ()
paintStyle grid label style = do
  palette <- readIORef (gridPalette grid)
  forM_ (M.elems palette) $ \name -> Gtk.widgetRemoveCssClass label name
  case style of
    (Nothing, Nothing) -> pure ()
    _ -> do
      name <- styleClass grid style
      Gtk.widgetAddCssClass label name

styleClass :: Grid -> (Maybe String, Maybe String) -> IO Text
styleClass grid style = do
  palette <- readIORef (gridPalette grid)
  case M.lookup style palette of
    Just name -> pure name
    Nothing -> do
      let name = T.pack ("cellar-style-" ++ show (M.size palette))
          palette' = M.insert style name palette
      writeIORef (gridPalette grid) palette'
      Gtk.cssProviderLoadFromString (gridPaletteProvider grid) (paletteCss palette')
      pure name

paletteCss :: M.Map (Maybe String, Maybe String) Text -> Text
paletteCss palette = T.concat (map rule (M.toList palette))
  where
    rule ((color, background), name) = T.concat
      [ ".", name, " { "
      , maybe "" (\c -> T.pack ("color: " ++ c ++ "; ")) color
      , maybe "" (\c -> T.pack ("background-color: " ++ c ++ "; ")) background
      , "}\n" ]

-- Repainting

-- | Take a new view.  This is the only way anything the grid draws ever
-- changes: a snapshot comes back from the kernel, the shell builds a view of
-- it, and hands it here.
gridSetView :: Grid -> View -> IO ()
gridSetView grid view = do
  writeIORef (gridViewRef grid) view
  gridSyncSize grid
  gridRefresh grid

gridCurrentView :: Grid -> IO View
gridCurrentView = readIORef . gridViewRef

-- | Repaint every realised cell.
gridRefresh :: Grid -> IO ()
gridRefresh grid = do
  -- A column added since the last refresh has a header widget by now, which it
  -- had not when it was inserted -- GtkColumnView builds those on the next
  -- layout pass -- so this is where it is given its drag gesture.
  installHeaderDrag grid
  cells <- readIORef (gridCells grid)
  forM_ cells $ \cell -> do
    position <- Gtk.columnViewCellGetPosition (cellItem cell)
    live <- livePosition grid position
    when live $ do
      column <- readIORef (cellColumn cell)
      paintCell grid (cellLabel cell) column (fromIntegral position)

-- | Grow the widget to the size the view reports.  Sheets only ever grow, so
-- there is nothing here to take away.
gridSyncSize :: Grid -> IO ()
gridSyncSize grid = do
  view <- readIORef (gridViewRef grid)
  let growRows = do
        count <- readIORef (gridRowCount grid)
        when (count < viewRows view) $ addRow grid >> growRows
      growColumns = do
        columns <- readIORef (gridColumnRefs grid)
        when (length columns < viewColumns view) $ do
          addColumn grid (length columns)
          growColumns
  growRows
  growColumns

addRow :: Grid -> IO ()
addRow grid = do
  count <- readIORef (gridRowCount grid)
  Gtk.stringListAppend (gridRowList grid) (T.pack (show count))
  writeIORef (gridRowCount grid) (count + 1)

-- | One more column, inserted at a position.  Every column from there
-- rightwards then answers to a new index and a new letter.
addColumn :: Grid -> Int -> IO ()
addColumn grid at = do
  index <- newIORef at
  column <- makeColumn grid index (T.pack (columnName at)) defaultColumnWidth
  columns <- readIORef (gridColumnRefs grid)
  let (toTheLeft, toTheRight) = splitAt at columns
  writeIORef (gridColumnRefs grid) (toTheLeft ++ [(index, column)] ++ toTheRight)
  Gtk.columnViewInsertColumn (gridWidget grid) (fromIntegral (at + 1)) column
  reletterColumns grid

-- | Put every column's index and heading back in step with its position.
reletterColumns :: Grid -> IO ()
reletterColumns grid = do
  columns <- readIORef (gridColumnRefs grid)
  forM_ (zip [0 ..] columns) $ \(position, (index, column)) -> do
    writeIORef index position
    Gtk.columnViewColumnSetTitle column (Just (T.pack (columnName position)))

-- Selection

gridActiveRef :: Grid -> IO Ref
gridActiveRef = readIORef . gridActive

gridSetActive :: Grid -> Ref -> IO ()
gridSetActive grid r = do
  view <- readIORef (gridViewRef grid)
  when (viewHolds view r) $ do
    writeIORef (gridActive grid) r
    gridRefresh grid
    gridOnSelect grid r

cellPressed :: Grid -> IORef Int -> Gtk.ColumnViewCell -> Int -> IO ()
cellPressed grid index cell presses = do
  position <- Gtk.columnViewCellGetPosition cell
  live <- livePosition grid position
  when live $ do
    column <- readIORef index
    let r = Ref (fromIntegral position) column
    gridSetActive grid r
    -- The whole point of the app: a second click opens the editor.
    when (presses >= 2) $ gridOnActivate grid r

moveActive :: Grid -> Int -> Int -> IO ()
moveActive grid rowDelta columnDelta = do
  view <- readIORef (gridViewRef grid)
  Ref row column <- readIORef (gridActive grid)
  let row' = clamp (row + rowDelta) 0 (viewRows view - 1)
      column' = clamp (column + columnDelta) 0 (viewColumns view - 1)
  gridSetActive grid (Ref row' column')
  scrollToRow grid row'

clamp :: Int -> Int -> Int -> Int
clamp n low high = max low (min high n)

scrollToRow :: Grid -> Int -> IO ()
scrollToRow grid row =
  Gtk.columnViewScrollTo (gridWidget grid) (fromIntegral row)
    (Nothing :: Maybe Gtk.ColumnViewColumn) [] (Nothing :: Maybe Gtk.ScrollInfo)

gridFocus :: Grid -> IO ()
gridFocus grid = void (Gtk.widgetGrabFocus (gridWidget grid))

-- Column widths

gridColumnWidths :: Grid -> IO [(Int, Int)]
gridColumnWidths grid = do
  columns <- readIORef (gridColumnRefs grid)
  widths <- forM columns $ \(index, column) -> do
    position <- readIORef index
    width <- Gtk.columnViewColumnGetFixedWidth column
    pure (position, fromIntegral width)
  -- Only the ones that were changed from the default are worth writing down.
  pure [ entry | entry@(_, width) <- widths
       , width > 0, width /= defaultColumnWidth ]

gridSetColumnWidths :: Grid -> [(Int, Int)] -> IO ()
gridSetColumnWidths grid widths = do
  columns <- readIORef (gridColumnRefs grid)
  forM_ columns $ \(index, column) -> do
    position <- readIORef index
    forM_ (lookup position widths) $ \width ->
      Gtk.columnViewColumnSetFixedWidth column (fromIntegral width)

-- Moving and inserting

-- | Ask for the active cell's row or column to move, and take the active cell
-- with it.  Answers whether the move is one that can be made.
--
-- The active cell moves here and now rather than when the answer comes back.
-- The arithmetic for where it lands needs nothing but the two indices, so
-- there is no reason to make the selection wait on a round trip -- and every
-- reason not to, since a selection that lags a keystroke is the kind of thing
-- that makes an application feel slow.
gridMoveLine :: Grid -> Axis -> Int -> IO Bool
gridMoveLine grid axis delta = do
  active <- readIORef (gridActive grid)
  let from = case axis of
        Row -> refRow active
        Column -> refColumn active
  applyMove grid axis from (from + delta)

-- | Ask for a line to move from one index to another, wherever the request
-- came from -- an arrow key or a dropped drag.
applyMove :: Grid -> Axis -> Int -> Int -> IO Bool
applyMove grid axis from to = do
  view <- readIORef (gridViewRef grid)
  active <- readIORef (gridActive grid)
  let limit = case axis of
        Row -> viewRows view
        Column -> viewColumns view
  if from < 0 || from >= limit || to < 0 || to >= limit || from == to
    then pure False
    else do
      writeIORef (gridActive grid) (refAfterMove active axis from to)
      readIORef (gridActive grid) >>= gridOnSelect grid
      gridOnCommand grid (Move axis from to)
      readIORef (gridActive grid) >>= scrollToRow grid . refRow
      pure True

-- | Ask for an empty row or column at the active cell.  The active cell stays
-- on the cell it was on, so an insert above it carries it down.
gridInsertLine :: Grid -> Axis -> Bool -> IO Bool
gridInsertLine grid axis before = do
  view <- readIORef (gridViewRef grid)
  active <- readIORef (gridActive grid)
  let at = (case axis of
              Row -> refRow active
              Column -> refColumn active)
           + (if before then 0 else 1)
      limit = case axis of
        Row -> viewRows view
        Column -> viewColumns view
  if at < 0 || at > limit
    then pure False
    else do
      -- The widget grows now: a column has to be inserted at its own index for
      -- the letters along the top to come out right, and growing at the end --
      -- which is all `gridSyncSize` can do when the snapshot lands -- would put
      -- it in the wrong place.
      case axis of
        Row -> addRow grid
        Column -> addColumn grid at
      writeIORef (gridActive grid) (refAfterInsert active axis at)
      readIORef (gridActive grid) >>= gridOnSelect grid
      gridOnCommand grid (Insert axis at)
      readIORef (gridActive grid) >>= scrollToRow grid . refRow
      pure True

-- The context menu on a row number or a column heading

installLineClick :: Grid -> Gtk.Label -> Axis -> IO (Maybe Int) -> IO ()
installLineClick grid label axis locate = do
  widget <- Gtk.toWidget label
  installLineClickOn grid widget axis locate

installLineClickOn :: Grid -> Gtk.Widget -> Axis -> IO (Maybe Int) -> IO ()
installLineClickOn grid label axis locate = forM_ (gridLineMenu grid) $ \model -> do
  handed <- Gtk.gestureClickNew
  gesture <- retain Gtk.GestureClick handed
  Gtk.gestureSingleSetButton gesture 3
  _ <- on gesture #pressed $ \_ x y -> do
    found <- locate
    forM_ found $ \index -> do
      void (Gtk.gestureSetState gesture Gtk.EventSequenceStateClaimed)
      selectLine grid axis index
      popover <- Gtk.popoverMenuNewFromModel (Just model)
      Gtk.popoverSetHasArrow popover False
      Gtk.widgetSetParent popover label
      rectangle <- Gdk.newZeroRectangle
      Gdk.setRectangleX rectangle (round x)
      Gdk.setRectangleY rectangle (round y)
      Gdk.setRectangleWidth rectangle 1
      Gdk.setRectangleHeight rectangle 1
      Gtk.popoverSetPointingTo popover (Just rectangle)
      Gtk.popoverPopup popover
      pure ()
  Gtk.widgetAddController label handed

-- | Put the active cell on a row or column, keeping the other half of the
-- reference where it is.
selectLine :: Grid -> Axis -> Int -> IO ()
selectLine grid axis index = do
  Ref row column <- readIORef (gridActive grid)
  gridSetActive grid $ case axis of
    Row -> Ref index column
    Column -> Ref row index

-- Dragging a row or a column

-- | Wide enough to keep clear of GTK's column resize handles, which live at
-- the edges of the same header widget the drag gesture is on.
resizeMargin :: Double
resizeMargin = 8

installRowDrag :: Grid -> Gtk.ColumnViewCell -> Gtk.Label -> IO ()
installRowDrag grid cell label = do
  handed <- Gtk.gestureDragNew
  gesture <- retain Gtk.GestureDrag handed
  _ <- on gesture #dragBegin $ \x y -> do
    found <- rowUnder grid cell
    case found of
      Just position -> do
        _ <- Gtk.gestureSetState gesture Gtk.EventSequenceStateClaimed
        widget <- Gtk.toWidget label
        beginDrag grid Row position widget x y
      Nothing -> void (Gtk.gestureSetState gesture Gtk.EventSequenceStateDenied)
  _ <- on gesture #dragUpdate (updateDrag grid)
  _ <- on gesture #dragEnd (finishDrag grid)
  _ <- on gesture #cancel (\_ -> cancelDrag grid)
  Gtk.widgetAddController label handed

-- | Give each column header a drag gesture, and pair it with its column.
--
-- The headers are GtkColumnView's own widgets and there is no API that hands
-- them over -- a column has a title string, not a header factory -- so they
-- are reached by walking the view: its first child is the header row, whose
-- children are the column titles, the leading one belonging to the row gutter.
--
-- Which header belongs to which column cannot be read off that walk, because
-- GtkColumnView appends an inserted column's header to the end of the header
-- row even though the column itself went in somewhere in the middle.  What can
-- be relied on is that a header appears exactly once: so the headers already
-- paired keep their columns, and a header seen for the first time takes the
-- column that has not got one yet.  Headers come and go -- there are none
-- until the view is realised, and inserting a column adds one -- so this runs
-- more than once.
installHeaderDrag :: Grid -> IO ()
installHeaderDrag grid = do
  header <- Gtk.widgetGetFirstChild (gridWidget grid)
  forM_ header $ \row -> do
    gutter <- Gtk.widgetGetFirstChild row
    forM_ gutter $ \first -> do
      widgets <- siblingsAfter first
      known <- readIORef (gridTitles grid)
      columns <- readIORef (gridColumnRefs grid)
      kept <- filterM (\(widget, _) -> anyM (sameWidget widget) widgets) known
      let takenIndexes = map snd kept
          free = [ index | (index, _) <- columns, index `notElem` takenIndexes ]
      paired <- pairUp grid widgets kept free
      writeIORef (gridTitles grid) paired

-- | Walk the widgets across the header row, keeping the pairings that survive
-- and giving each new header the next column that has not got one.
pairUp
  :: Grid -> [Gtk.Widget] -> [(Gtk.Widget, IORef Int)] -> [IORef Int]
  -> IO [(Gtk.Widget, IORef Int)]
pairUp _ [] _ _ = pure []
pairUp grid (widget : more) kept free = do
  already <- findM (\(known, _) -> sameWidget known widget) kept
  case already of
    Just entry -> (entry :) <$> pairUp grid more kept free
    Nothing -> case free of
      [] -> pure []
      (index : rest) -> do
        installColumnDrag grid widget index
        ((widget, index) :) <$> pairUp grid more kept rest

installColumnDrag :: Grid -> Gtk.Widget -> IORef Int -> IO ()
installColumnDrag grid title index = do
  handed <- Gtk.gestureDragNew
  gesture <- retain Gtk.GestureDrag handed
  -- Capture, not bubble.  A header has gestures of GTK's own -- one of them
  -- claims the sequence as soon as the pointer moves, and a bubble-phase
  -- gesture here sees the press and then nothing at all.
  Gtk.eventControllerSetPropagationPhase gesture Gtk.PropagationPhaseCapture
  _ <- on gesture #dragBegin $ \x y -> do
    width <- fromIntegral <$> Gtk.widgetGetWidth title
    -- Near either edge the user is resizing the column, which is GTK's gesture
    -- on this same widget.  Stay out of its way.
    if x > resizeMargin && x < width - resizeMargin
      then do
        _ <- Gtk.gestureSetState gesture Gtk.EventSequenceStateClaimed
        column <- readIORef index
        beginDrag grid Column column title x y
      else void (Gtk.gestureSetState gesture Gtk.EventSequenceStateDenied)
  _ <- on gesture #dragUpdate (updateDrag grid)
  _ <- on gesture #dragEnd (finishDrag grid)
  _ <- on gesture #cancel (\_ -> cancelDrag grid)
  Gtk.widgetAddController title handed
  installLineClickOn grid title Column (Just <$> readIORef index)

beginDrag :: Grid -> Axis -> Int -> Gtk.Widget -> Double -> Double -> IO ()
beginDrag grid axis from widget x y = do
  writeIORef (gridDrag grid) (Just (Drag axis from widget x y from))
  showDrag grid

updateDrag :: Grid -> Double -> Double -> IO ()
updateDrag grid offsetX offsetY = do
  current <- readIORef (gridDrag grid)
  forM_ current $ \drag -> do
    target <- dragTargetAt grid drag offsetX offsetY
    forM_ target $ \landing -> do
      writeIORef (gridDrag grid) (Just drag { dragTarget = landing })
      showDrag grid

finishDrag :: Grid -> Double -> Double -> IO ()
finishDrag grid offsetX offsetY = do
  current <- readIORef (gridDrag grid)
  forM_ current $ \drag -> do
    target <- dragTargetAt grid drag offsetX offsetY
    let landing = fromMaybe (dragTarget drag) target
    writeIORef (gridDrag grid) Nothing
    showDrag grid
    void (applyMove grid (dragAxis drag) (dragFrom drag) landing)

cancelDrag :: Grid -> IO ()
cancelDrag grid = do
  writeIORef (gridDrag grid) Nothing
  showDrag grid

-- | The row or column the pointer is over, so far into the drag.
dragTargetAt :: Grid -> Drag -> Double -> Double -> IO (Maybe Int)
dragTargetAt grid drag offsetX offsetY = do
  let x = dragStartX drag + offsetX
      y = dragStartY drag + offsetY
  case dragAxis drag of
    Row -> rowAt grid (dragWidget drag) x y
    Column -> columnAt grid (dragWidget drag) x

rowAt :: Grid -> Gtk.Widget -> Double -> Double -> IO (Maybe Int)
rowAt grid widget x y = do
  point <- translatePoint widget (gridWidget grid) x y
  case point of
    Nothing -> pure Nothing
    Just (viewX, viewY) -> do
      found <- widgetAt grid viewX viewY
      case found of
        Nothing -> pure Nothing
        Just cell -> do
          position <- Gtk.columnViewCellGetPosition (cellItem cell)
          live <- livePosition grid position
          pure (if live then Just (fromIntegral position) else Nothing)

-- | The cell widget under a point in the view's coordinates.
--
-- @gtk_widget_pick@ answers with whatever widget is deepest at that point,
-- which is the cell widget GtkColumnView wraps our label in, so this looks at
-- the child of the answer as well as at the answer itself, and then walks up.
widgetAt :: Grid -> Double -> Double -> IO (Maybe CellWidget)
widgetAt grid x y = do
  picked <- Gtk.widgetPick (gridWidget grid) x y []
  go picked (0 :: Int)
  where
    go Nothing _ = pure Nothing
    go (Just widget) depth
      | depth >= 3 = pure Nothing
      | otherwise = do
          here <- entryFor grid widget
          case here of
            Just entry -> pure (Just entry)
            Nothing -> do
              child <- Gtk.widgetGetFirstChild widget
              below <- case child of
                Nothing -> pure Nothing
                Just c -> entryFor grid c
              case below of
                Just entry -> pure (Just entry)
                Nothing -> do
                  parent <- Gtk.widgetGetParent widget
                  go parent (depth + 1)

entryFor :: Grid -> Gtk.Widget -> IO (Maybe CellWidget)
entryFor grid widget = do
  cells <- readIORef (gridCells grid)
  findM (\cell -> do
           label <- Gtk.toWidget (cellLabel cell)
           sameWidget label widget)
        cells

-- | The column whose header contains a point.  Past the last header this is
-- the last column, and before the first, the first: a drag that overshoots
-- means the end of the sheet, not nothing at all.
columnAt :: Grid -> Gtk.Widget -> Double -> IO (Maybe Int)
columnAt grid widget x = do
  entries <- headersAcross grid
  case entries of
    [] -> pure Nothing
    ((firstHeader, _) : _) -> do
      header <- Gtk.widgetGetParent firstHeader
      case header of
        Nothing -> pure Nothing
        Just row -> do
          point <- translateBetween widget row x 0
          case point of
            Nothing -> pure Nothing
            Just (headerX, _) -> walk row headerX entries Nothing
  where
    walk _ _ [] previous = traverse (readIORef . snd) previous
    walk row headerX (entry@(title, index) : more) previous = do
      origin <- translateBetween title row 0 0
      case origin of
        Nothing -> pure Nothing
        Just (left, _) -> do
          width <- fromIntegral <$> Gtk.widgetGetWidth title
          if headerX < left
            then Just <$> readIORef (snd (fromMaybe entry previous))
            else if headerX < left + width
              then Just <$> readIORef index
              else walk row headerX more (Just entry)

-- | The headers in the order they lie across the screen, which is the order
-- the columns are in and not the order the header row holds them.
headersAcross :: Grid -> IO [(Gtk.Widget, IORef Int)]
headersAcross grid = do
  entries <- readIORef (gridTitles grid)
  case entries of
    [] -> pure []
    ((firstHeader, _) : _) -> do
      header <- Gtk.widgetGetParent firstHeader
      case header of
        Nothing -> pure entries
        Just row -> do
          placed <- forM entries $ \entry@(title, _) -> do
            origin <- translateBetween title row 0 0
            pure (maybe (1 / 0) fst origin, entry)
          pure (map snd (sortOn fst placed))

-- | A point given in one widget's coordinates, in another's instead.
translatePoint :: Gtk.Widget -> Gtk.ColumnView -> Double -> Double
               -> IO (Maybe (Double, Double))
translatePoint widget target x y = do
  destination <- Gtk.toWidget target
  translateBetween widget destination x y

translateBetween :: Gtk.Widget -> Gtk.Widget -> Double -> Double
                 -> IO (Maybe (Double, Double))
translateBetween widget target x y = do
  -- widgetComputePoint rather than the older widgetTranslateCoordinates, which
  -- GTK deprecated in 4.12.  It wants a graphene point in and gives one back,
  -- which is why the Guile version used the old call: G-Golf marshals neither.
  from <- Graphene.newZeroPoint
  Graphene.setPointX from (realToFrac x)
  Graphene.setPointY from (realToFrac y)
  (ok, to) <- Gtk.widgetComputePoint widget target from
  if not ok then pure Nothing else do
    tx <- Graphene.getPointX to
    ty <- Graphene.getPointY to
    pure (Just (realToFrac tx, realToFrac ty))

-- What a drag looks like while it lasts

-- | Dim the line being dragged and light up the one it would land on.
showDrag :: Grid -> IO ()
showDrag grid = do
  drag <- readIORef (gridDrag grid)
  cells <- readIORef (gridCells grid)
  titles <- readIORef (gridTitles grid)
  let isRow = fmap dragAxis drag == Just Row
  forM_ cells $ \cell -> do
    position <- Gtk.columnViewCellGetPosition (cellItem cell)
    live <- livePosition grid position
    column <- readIORef (cellColumn cell)
    -- Which row or column this cell is in, on the axis being dragged.  The
    -- gutter is column -1, so it is never a column target, but it is part of
    -- every row.
    let index = case drag of
          Just _ | live -> Just (if isRow then fromIntegral position else column)
          _ -> Nothing
    widget <- Gtk.toWidget (cellLabel cell)
    paintDrag widget drag index
  forM_ titles $ \(title, index) -> do
    column <- readIORef index
    paintDrag title drag (if fmap dragAxis drag == Just Column
                            then Just column else Nothing)

paintDrag :: Gtk.Widget -> Maybe Drag -> Maybe Int -> IO ()
paintDrag widget drag index = do
  let source = case (drag, index) of
        (Just d, Just i) -> i == dragFrom d
        _ -> False
      target = case (drag, index) of
        (Just d, Just i) -> i == dragTarget d && dragTarget d /= dragFrom d
        _ -> False
  setCssClass widget "cellar-drag-source" source
  setCssClass widget "cellar-drag-target" target

setCssClass :: Gtk.Widget -> Text -> Bool -> IO ()
setCssClass widget name wanted
  | wanted = Gtk.widgetAddCssClass widget name
  | otherwise = Gtk.widgetRemoveCssClass widget name

-- | Keep a reference of our own to a controller before handing it to a widget.
--
-- @gtk_widget_add_controller@ takes ownership, so haskell-gi disowns the value
-- passed to it.  Every gesture below then goes on to call @gestureSetState@
-- from inside its own callbacks -- which would be reading a pointer we no
-- longer hold, and which haskell-gi warns about at runtime as "accessing a
-- disowned pointer".  It happens to work while the widget is alive, and would
-- stop working the moment it was not.  Taking a reference first is what makes
-- the captured handle ours for as long as the closure lives.
retain :: (GObject a, TypedObject a, IsDescendantOf GObject.Object a)
       => (ManagedPtr a -> a) -> a -> IO a
retain constructor object = GObject.objectRef object >>= unsafeCastTo constructor

-- Widget identity
--
-- Two handles on the same GObject are two Haskell values, so identity has to
-- be asked of the pointer underneath rather than of the wrapper.

sameWidget :: Gtk.Widget -> Gtk.Widget -> IO Bool
sameWidget a b =
  withManagedPtr a $ \pa -> withManagedPtr b $ \pb ->
    pure (castPtr pa == castPtr pb)

siblingsAfter :: Gtk.Widget -> IO [Gtk.Widget]
siblingsAfter widget = do
  next <- Gtk.widgetGetNextSibling widget
  case next of
    Nothing -> pure []
    Just sibling -> (sibling :) <$> siblingsAfter sibling

findM :: (a -> IO Bool) -> [a] -> IO (Maybe a)
findM _ [] = pure Nothing
findM predicate (x : xs) = do
  matched <- predicate x
  if matched then pure (Just x) else findM predicate xs

anyM :: (a -> IO Bool) -> [a] -> IO Bool
anyM predicate xs = maybe False (const True) <$> findM predicate xs

-- Keyboard

installKeyHandling :: Grid -> IO ()
installKeyHandling grid = do
  controller <- Gtk.eventControllerKeyNew
  Gtk.eventControllerSetPropagationPhase controller Gtk.PropagationPhaseCapture
  _ <- on controller #keyPressed $ \keyval _ state -> keyPressed grid keyval state
  Gtk.widgetAddController (gridWidget grid) controller

keyPressed :: Grid -> Word32 -> [Gdk.ModifierType] -> IO Bool
keyPressed grid keyval _state
  | keyval == Gdk.KEY_Left = moveActive grid 0 (-1) >> pure True
  | keyval == Gdk.KEY_Right = moveActive grid 0 1 >> pure True
  | keyval == Gdk.KEY_Up = moveActive grid (-1) 0 >> pure True
  | keyval == Gdk.KEY_Down = moveActive grid 1 0 >> pure True
  | keyval == Gdk.KEY_Page_Up = moveActive grid (-10) 0 >> pure True
  | keyval == Gdk.KEY_Page_Down = moveActive grid 10 0 >> pure True
  | keyval == Gdk.KEY_Home = moveActive grid 0 (-1000) >> pure True
  | keyval == Gdk.KEY_End = moveActive grid 0 1000 >> pure True
  | keyval == Gdk.KEY_Tab = moveActive grid 0 1 >> pure True
  | keyval == Gdk.KEY_ISO_Left_Tab = moveActive grid 0 (-1) >> pure True
  | keyval == Gdk.KEY_Return || keyval == Gdk.KEY_KP_Enter = do
      readIORef (gridActive grid) >>= gridOnActivate grid
      pure True
  | keyval == Gdk.KEY_Delete || keyval == Gdk.KEY_BackSpace = do
      r <- readIORef (gridActive grid)
      gridOnCommand grid (Clear r)
      pure True
  | otherwise = pure False
