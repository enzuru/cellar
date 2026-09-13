{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedLists #-}

-- | The grid, as a value and a function.
--
-- What the grid draws is a 'GridModel', and how it draws is 'gridView': a pure
-- function from that model to declarative markup.  What a person does to it
-- arrives as a 'GridEvent', and 'gridEvent' says what the model becomes and
-- what the grid wants done about it.  Nothing here touches a widget, opens a
-- file or talks to the kernel, so all of it can be tested without a display.
--
-- "Cellar.Grid" is the other half: the widgets, the gestures GTK gives no
-- declarative way to reach, and the reference that holds the model between
-- events.  When the window as a whole becomes a function of its state, this
-- module is the part of the grid that goes into it.
module Cellar.Grid.Model
  ( -- * The model
    GridModel (..)
  , ColumnId (..)
  , newGridModel
    -- * What the grid asks for
  , Command (..)
  , GridOut (..)
    -- * What happens to it
  , GridEvent (..)
  , gridEvent
  , handledKey
    -- * Changes from outside
  , withView
  , withPalette
  , paletteFor
  , withActive
  , withWidths
  , withDrag
  , columnWidths
  , selectLine
  , moveLine
  , insertLine
  , moveItem
  , scrollTo
  , positionOfColumn
    -- * What is on screen
  , GridHandlers (..)
  , gridView
  , paletteCss
  , gutterName
  , defaultColumnWidth
  ) where

import Data.Int (Int32)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32)

import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import GI.Gtk.Declarative
import qualified GI.Gtk.Declarative.ModelView.ColumnView as CV

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

-- | What the grid wants done, which is not always about the sheet.
data GridOut
  = Ask Command            -- ^ Change the sheet, and hand back a new view.
  | Edit Ref               -- ^ Open the editor on this cell.
  deriving (Eq, Show)

-- | What number a column's name is, for handing out the next one.  Nothing
-- else has any business doing arithmetic on one.
number :: ColumnId -> Int
number (ColumnId n) = n

-- | A column's name to itself, handed out once and never reused.
--
-- The letters along the top are positions, and an insert moves every letter to
-- its right along; the identifier underneath does not move.  That is what lets
-- the library keep a column's widget, and its width, when a column is inserted
-- to the left of it -- it matches one render against the next by this.
newtype ColumnId = ColumnId Int
  deriving newtype (Eq, Ord, Show)

-- | Everything the grid draws, in one value.
data GridModel = GridModel
  { modelView :: View
    -- | How many rows the widget shows.  The view says how many the sheet has;
    -- this can be one ahead of it, between asking for a row and being handed
    -- the snapshot that has it.
  , modelRows :: Int
  , modelColumns :: [ColumnId]
    -- ^ The sheet's columns, in the order they are drawn.
  , modelNextColumn :: ColumnId
    -- ^ The identifier the next column to appear will take.
  , modelActive :: Ref
  , modelWidths :: M.Map ColumnId Int
    -- | The colours the stylesheet knows about.  A cell can ask to be drawn in
    -- any colour it likes, and GTK has no way to set one on a widget except
    -- through the stylesheet, so each distinct pair becomes a class.
  , modelPalette :: M.Map (Maybe String, Maybe String) Text
  , modelDrag :: Maybe (Axis, Int, Int)
    -- ^ The axis being dragged, where it started, and where it would land.
  , modelScroll :: Maybe Word
  }
  deriving (Eq)

-- | A grid showing this view, with the first cell active.
newGridModel :: View -> GridModel
newGridModel view = GridModel
  { modelView = view
  , modelRows = viewRows view
  , modelColumns = map ColumnId [0 .. viewColumns view - 1]
  , modelNextColumn = ColumnId (viewColumns view)
  , modelActive = Ref 0 0
  , modelWidths = M.empty
  , modelPalette = M.empty
  , modelDrag = Nothing
  , modelScroll = Nothing
  }

-- | What the markup says happened.
data GridEvent
  = Pressed Ref Int32      -- ^ A cell was clicked, and how many times.
  | Resized ColumnId Int32
  | KeyDown Word32
  deriving (Eq, Show)

--
-- What happens to the model
--

-- | What an event makes of the model, and what the grid wants done about it.
gridEvent :: GridEvent -> GridModel -> (GridModel, [GridOut])
gridEvent event model = case event of
  Pressed r presses ->
    -- The whole point of the app: a second click opens the editor.
    ( fromMaybe model (withActive r model)
    , [Edit r | presses >= 2 && viewHolds (modelView model) r]
    )
  -- A column that was dragged wider is already the width it was dragged to, so
  -- this is written down rather than drawn.  The layout is worth saving.
  Resized identifier width ->
    ( model { modelWidths = M.insert identifier (fromIntegral width) (modelWidths model) }
    , [Ask Layout]
    )
  KeyDown keyval -> keyDown keyval model

keyDown :: Word32 -> GridModel -> (GridModel, [GridOut])
keyDown keyval model
  | keyval == Gdk.KEY_Left = moveActive 0 (-1)
  | keyval == Gdk.KEY_Right = moveActive 0 1
  | keyval == Gdk.KEY_Up = moveActive (-1) 0
  | keyval == Gdk.KEY_Down = moveActive 1 0
  | keyval == Gdk.KEY_Page_Up = moveActive (-10) 0
  | keyval == Gdk.KEY_Page_Down = moveActive 10 0
  | keyval == Gdk.KEY_Home = moveActive 0 (-1000)
  | keyval == Gdk.KEY_End = moveActive 0 1000
  | keyval == Gdk.KEY_Tab = moveActive 0 1
  | keyval == Gdk.KEY_ISO_Left_Tab = moveActive 0 (-1)
  | keyval == Gdk.KEY_Return || keyval == Gdk.KEY_KP_Enter =
      (model, [Edit (modelActive model)])
  | keyval == Gdk.KEY_Delete || keyval == Gdk.KEY_BackSpace =
      (model, [Ask (Clear (modelActive model))])
  | otherwise = (model, [])
  where
    moveActive rowDelta columnDelta =
      let view = modelView model
          Ref row column = modelActive model
          row' = clamp (row + rowDelta) 0 (viewRows view - 1)
          column' = clamp (column + columnDelta) 0 (viewColumns view - 1)
          moved = fromMaybe model (withActive (Ref row' column') model)
      in (scrollTo row' moved, [])

-- | Whether a key is one the grid answers.  A key it answers is one it stops,
-- because the column view would otherwise move its own cursor as well.
handledKey :: Word32 -> Bool
handledKey keyval = keyval `elem`
  ([ Gdk.KEY_Left, Gdk.KEY_Right, Gdk.KEY_Up, Gdk.KEY_Down
   , Gdk.KEY_Page_Up, Gdk.KEY_Page_Down, Gdk.KEY_Home, Gdk.KEY_End
   , Gdk.KEY_Tab, Gdk.KEY_ISO_Left_Tab
   , Gdk.KEY_Return, Gdk.KEY_KP_Enter
   , Gdk.KEY_Delete, Gdk.KEY_BackSpace
   ] :: [Word32])

clamp :: Int -> Int -> Int -> Int
clamp n low high = max low (min high n)

--
-- Changes from outside
--

-- | Take a new view.  This is the only way anything the grid draws ever
-- changes: a snapshot comes back from the kernel, the shell builds a view of
-- it, and hands it here.
--
-- Sheets only ever grow here: a row or column asked for a moment ago is
-- already on screen, and the snapshot that confirms it must not take it away
-- again.
withView :: View -> GridModel -> GridModel
withView view model = model
  { modelView = view
  , modelRows = max (modelRows model) (viewRows view)
  , modelColumns = modelColumns model ++ fresh
  , modelNextColumn = ColumnId (number (modelNextColumn model) + length fresh)
  }
  where
    fresh = [ ColumnId n
            | n <- take (viewColumns view - length (modelColumns model))
                        [number (modelNextColumn model) ..] ]

-- | Put the active cell somewhere.  'Nothing' when that is not a cell of this
-- sheet, which is what a click on a column that the kernel has not caught up
-- with looks like.
withActive :: Ref -> GridModel -> Maybe GridModel
withActive r model
  | viewHolds (modelView model) r = Just model { modelActive = r }
  | otherwise = Nothing

-- | Put the active cell on a row or column, keeping the other half of the
-- reference where it is.
selectLine :: Axis -> Int -> GridModel -> Maybe GridModel
selectLine axis index model =
  let Ref row column = modelActive model
  in withActive (case axis of
                   Row -> Ref index column
                   Column -> Ref row index) model

-- | Bring a row into sight.  The library scrolls when the value it is given
-- differs from the one before, so a grid that stays where it is stays put.
scrollTo :: Int -> GridModel -> GridModel
scrollTo row model = model { modelScroll = Just (fromIntegral row) }

-- | What the drag looks like, or that there is none.
withDrag :: Maybe (Axis, Int, Int) -> GridModel -> GridModel
withDrag drag model = model { modelDrag = drag }

-- | The widths a sheet was saved with, by position.
withWidths :: [(Int, Int)] -> GridModel -> GridModel
withWidths widths model =
  model { modelWidths = foldl put (modelWidths model) widths }
  where
    put current (position, width) = case drop position (modelColumns model) of
      (identifier : _) -> M.insert identifier width current
      [] -> current

-- | The widths worth writing down, by position.  Only the ones that were
-- changed from the default are.
columnWidths :: GridModel -> [(Int, Int)]
columnWidths model =
  [ (position, width)
  | (position, identifier) <- zip [0 ..] (modelColumns model)
  , Just width <- [M.lookup identifier (modelWidths model)]
  , width > 0, width /= defaultColumnWidth ]

-- | Where a column is now.  Its identifier does not move; its position does.
positionOfColumn :: ColumnId -> GridModel -> Maybe Int
positionOfColumn identifier model =
  lookup identifier (zip (modelColumns model) [0 ..])

-- | Ask for a line to move from one index to another, wherever the request
-- came from -- an arrow key or a dropped drag.  'Nothing' when the move is not
-- one that can be made.
--
-- The active cell moves here and now rather than when the answer comes back.
-- The arithmetic for where it lands needs nothing but the two indices, so
-- there is no reason to make the selection wait on a round trip -- and every
-- reason not to, since a selection that lags a keystroke is the kind of thing
-- that makes an application feel slow.
moveLine :: Axis -> Int -> Int -> GridModel -> Maybe (GridModel, Command)
moveLine axis from to model
  | from < 0 || from >= limit || to < 0 || to >= limit || from == to = Nothing
  | otherwise = Just
      ( scrollTo (refRow active) model
          { modelActive = active
          -- A column keeps its widget and its width by keeping its identifier,
          -- so moving one is moving its identifier along the list.
          , modelColumns = case axis of
              Row -> modelColumns model
              Column -> moveItem from to (modelColumns model)
          }
      , Move axis from to
      )
  where
    active = refAfterMove (modelActive model) axis from to
    limit = case axis of
      Row -> viewRows (modelView model)
      Column -> viewColumns (modelView model)

-- | Ask for an empty row or column at an index.  The active cell stays on the
-- cell it was on, so an insert above it carries it down.
--
-- The widget grows now rather than when the snapshot lands: a column has to
-- appear at its own index for the letters along the top to come out right, and
-- an insert is the one thing that cannot be done by growing at the end.
insertLine :: Axis -> Int -> GridModel -> Maybe (GridModel, Command)
insertLine axis at model
  | at < 0 || at > limit = Nothing
  | otherwise = Just
      ( scrollTo (refRow moved) model
          { modelRows = modelRows model + (case axis of { Row -> 1; Column -> 0 })
          , modelColumns = case axis of
              Row -> modelColumns model
              Column ->
                let (toTheLeft, toTheRight) = splitAt at (modelColumns model)
                in toTheLeft ++ [modelNextColumn model] ++ toTheRight
          , modelNextColumn = ColumnId (number (modelNextColumn model)
              + (case axis of { Row -> 0; Column -> 1 }))
          , modelActive = moved
          }
      , Insert axis at
      )
  where
    moved = refAfterInsert (modelActive model) axis at
    limit = case axis of
      Row -> viewRows (modelView model)
      Column -> viewColumns (modelView model)

moveItem :: Int -> Int -> [a] -> [a]
moveItem from to xs
  | from < 0 || to < 0 || from >= length xs || to >= length xs = xs
  | otherwise =
      let item = xs !! from
          without = take from xs ++ drop (from + 1) xs
      in take to without ++ [item] ++ drop to without

--
-- The palette
--

-- | Which colours a sheet asks for, added to the ones already known.
--
-- The names are handed out in the order the colours are met, so the same
-- palette has to be used by every sheet of a window: two sheets each counting
-- from zero would mean two different colours under one name, and one
-- stylesheet cannot say both.  The window keeps it and hands it down with
-- 'withPalette'.
paletteFor :: View -> M.Map (Maybe String, Maybe String) Text
           -> M.Map (Maybe String, Maybe String) Text
paletteFor view = \palette -> foldl add palette styles
  where
    styles = [ (cellColor cell, cellBackground cell)
             | cell <- M.elems (viewCells view)
             , isJust (cellColor cell) || isJust (cellBackground cell) ]
    add palette style
      | M.member style palette = palette
      | otherwise =
          M.insert style (T.pack ("cellar-style-" ++ show (M.size palette))) palette

-- | Draw with these colours.
withPalette :: M.Map (Maybe String, Maybe String) Text -> GridModel -> GridModel
withPalette palette model = model { modelPalette = palette }

-- | The stylesheet a palette comes to.
paletteCss :: M.Map (Maybe String, Maybe String) Text -> Text
paletteCss palette = T.concat (map rule (M.toList palette))
  where
    rule ((color, background), name) = T.concat
      [ ".", name, " { "
      , maybe "" (\c -> T.pack ("color: " ++ c ++ "; ")) color
      , maybe "" (\c -> T.pack ("background-color: " ++ c ++ "; ")) background
      , "}\n" ]

--
-- What is on screen
--

-- | One cell, as it is drawn.
--
-- The row a cell belongs to is what the library is handed, and what it
-- compares one render against the next to decide whether that row has to be
-- drawn again.  So a cell holds what it says rather than where to look it up:
-- two equal rows have to mean two rows that look the same, or a row that
-- changed would be left as it was.
data CellDraw = CellDraw
  { cellRef :: Ref
  , cellLabel :: Text
  , cellNumeric :: Bool
  , cellTooltip :: Text
  , cellStyles :: [Text]
  }
  deriving (Eq)

-- | One row: the number in the gutter, and a cell for each column.
data RowDraw = RowDraw
  { rowGutter :: CellDraw
  , rowCells :: V.Vector CellDraw
  }
  deriving (Eq)

-- | What the markup cannot say for itself.
--
-- A gesture that claims an event sequence needs the gesture object in its own
-- handler, which a declarative controller does not hand over, and a column
-- header is GtkColumnView's own widget.  So the widgets that need either are
-- handed to these when they are built.
data GridHandlers = GridHandlers
  { onCellBuilt :: Gtk.Label -> IO ()
  , onGutterBuilt :: Gtk.Label -> IO ()
  , onViewBuilt :: Gtk.ColumnView -> IO ()
  }

defaultColumnWidth, gutterWidth :: Int
defaultColumnWidth = 104
gutterWidth = 60

-- | The key the gutter column goes under.  The sheet's own columns are
-- numbers, so it cannot be one.
gutterKey :: Text
gutterKey = "gutter"

-- | The grid, as markup.
gridView :: GridHandlers -> GridModel -> Widget GridEvent
gridView handlers model =
  bin Gtk.ScrolledWindow [#hexpand := True, #vexpand := True] columnView'
  where
    columnView' = CV.columnView
      [ #showRowSeparators := True
      , #showColumnSeparators := True
      , #reorderable := False
      , #hexpand := True
      , #vexpand := True
      , classes ["data-table"]
      -- Capture, not bubble: GtkColumnView answers the arrow keys itself by
      -- moving its own cursor, and a handler that sees them afterwards is
      -- answering a question already settled.
      , onController capturingKeys #keyPressed
          (\keyval _ _ -> (handledKey keyval, KeyDown keyval))
      , afterCreated (onViewBuilt handlers)
      ]
      (CV.defaultColumnViewParams columns)
        { CV.rows = V.generate (modelRows model) (drawRow model)
        , CV.scrollTo = modelScroll model
        -- Draw the rows that changed and leave the rest alone.  A row holds
        -- everything its cells say, so two equal rows are two rows that look
        -- the same, and moving the selection from one cell to the next draws
        -- two rows rather than every row on screen.
        , CV.rowUnchanged = Just (==)
        -- What is selected in a spreadsheet is a cell, and this draws that
        -- itself.  A selected row would be GTK highlighting the whole width of
        -- the sheet because somebody clicked one cell of it.
        , CV.selectionMode = CV.SelectNothing
        }

    columns = V.fromList (gutter : map sheetColumn (zip [0 ..] (modelColumns model)))

    gutter = (CV.column gutterKey "" (gutterCell handlers))
      { CV.columnResizable = False
      , CV.columnFixedWidth = Just (fromIntegral gutterWidth)
      }

    sheetColumn (position, identifier) =
      (CV.column (T.pack (show identifier))
                 (T.pack (columnName position))
                 (sheetCell handlers position))
        { CV.columnFixedWidth = Just . fromIntegral $
            fromMaybe defaultColumnWidth (M.lookup identifier (modelWidths model))
        , CV.onResized = Just (Resized identifier)
        }

-- | What a row says, worked out once and compared as a value.
--
-- Every line of this reads from the model and nothing else.  That is what
-- makes scrolling free: the answers were computed once, by another process,
-- and drawing them again asks nobody anything.
drawRow :: GridModel -> Int -> RowDraw
drawRow model row = RowDraw
  { rowGutter = CellDraw
      { cellRef = Ref row (-1)
      , cellLabel = T.pack (show (row + 1))
      , cellNumeric = False
      , cellTooltip = ""
      , cellStyles = "cellar-gutter" : dragClasses model Row row
      }
  , rowCells = V.generate (length (modelColumns model)) cell
  }
  where
    view = modelView model
    -- One lookup per cell, not six.  Everything a cell says comes out of the
    -- same entry, and finding it means building the reference's name and
    -- walking a map, which is worth doing once when a screen holds hundreds of
    -- cells and the whole row is worked out on every patch.
    cell position =
      let r = Ref row position
          found = cellAt view r
          failure = found >>= cellError
      in CellDraw
           { cellRef = r
           , cellLabel = T.pack (maybe "" cellDisplay found)
           , cellNumeric = maybe False cellIsNumber found
           , cellTooltip = T.pack $ case failure of
               Just why -> why
               Nothing -> fromMaybe "" (found >>= cellSource)
           , cellStyles = concat
               ([ ["cellar-cell"]
                , ["cellar-active" | r == modelActive model]
                , ["cellar-error" | isJust failure]
                , [ name
                  | Just found' <- [found]
                  , Just name <- [M.lookup (cellColor found', cellBackground found')
                                           (modelPalette model)] ]
                , dragClasses model Row row
                , dragClasses model Column position
                ] :: [[Text]])
           }

-- | One cell of the sheet.
sheetCell :: GridHandlers -> Int -> RowDraw -> Widget GridEvent
sheetCell handlers position row = case rowCells row V.!? position of
  Just drawn -> cellLabelWidget handlers drawn
  -- A column the row has no cell for, which is a column inserted since this
  -- row was worked out.  The row is drawn again a moment later.
  Nothing -> widget Gtk.Label []

-- | One row number, in the gutter.
--
-- The gutter is the row's handle: it is the one part of a row that holds no
-- data, so a click on it can only mean "this row".
gutterCell :: GridHandlers -> RowDraw -> Widget GridEvent
gutterCell handlers row = widget Gtk.Label
  [ #hexpand := True
  , #xalign := 0.5
  , #ellipsize := Pango.EllipsizeModeEnd
  , #singleLineMode := True
  , #label := cellLabel (rowGutter row)
  , #name := T.pack (gutterName (refRow (cellRef (rowGutter row))))
  , #hasTooltip := False
  , #tooltipText := ""
  , classes (cellStyles (rowGutter row))
  , afterCreated (onGutterBuilt handlers)
  ]

cellLabelWidget :: GridHandlers -> CellDraw -> Widget GridEvent
cellLabelWidget handlers drawn = widget Gtk.Label
  [ #hexpand := True
  , #xalign := if cellNumeric drawn then 1.0 else 0.0
  , #ellipsize := Pango.EllipsizeModeEnd
  , #singleLineMode := True
  , #label := cellLabel drawn
  -- The cell a recycled widget currently stands for, which is how the gestures
  -- know what they are on.  A widget is bound to another row as you scroll,
  -- and this follows it.
  , #name := T.pack (refName (cellRef drawn))
  , #hasTooltip := not (T.null (cellTooltip drawn))
  , #tooltipText := cellTooltip drawn
  , classes (cellStyles drawn)
  , onClickPressed (\presses _ _ -> Pressed (cellRef drawn) presses)
  , afterCreated (onCellBuilt handlers)
  ]

-- | Dim the line being dragged, and light up the one it would land on.
dragClasses :: GridModel -> Axis -> Int -> [Text]
dragClasses model axis index = case modelDrag model of
  Just (dragging, from, target)
    | dragging == axis ->
        ["cellar-drag-source" | index == from]
          ++ ["cellar-drag-target" | index == target && target /= from]
  _ -> []

capturingKeys :: IO Gtk.EventControllerKey
capturingKeys = do
  controller <- Gtk.eventControllerKeyNew
  Gtk.eventControllerSetPropagationPhase controller Gtk.PropagationPhaseCapture
  pure controller

-- | Which row a gutter widget is standing for, as its name says it.
gutterName :: Int -> String
gutterName row = "gutter-" ++ show row
