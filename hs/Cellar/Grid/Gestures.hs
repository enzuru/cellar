{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The parts of the grid GTK gives no declarative way to reach.
--
-- Three things need a gesture object inside their own handler, which a
-- declarative controller does not hand over: the drag that moves a row, the
-- drag that moves a column, and the right-click that opens the line menu.  A
-- column header needs one more thing besides -- it is GtkColumnView's own
-- widget, built on a layout pass, and a column has a title string rather than
-- a header factory, so the only way to reach one is to walk the view.
--
-- Nothing here holds any part of the grid's model.  What a gesture works out
-- from the widgets -- which row the pointer is over, which column a header
-- belongs to -- it says, and whoever is listening decides what it means.
module Cellar.Grid.Gestures
  ( Gestures
  , GridGesture (..)
  , newGestures
  , gestureHandlers
  , gestureDragShown
  ) where

import Control.Monad (forM, forM_, void, when)
import Data.IORef
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Data.GI.Base (GObject, ManagedPtr, unsafeCastTo)
import qualified Data.GI.Base as GI
import Data.GI.Base.Overloading (IsDescendantOf)
import qualified GI.Gdk as Gdk
import qualified GI.GObject as GObject
import qualified GI.Gio as Gio
import qualified GI.Graphene as Graphene
import qualified GI.Gtk as Gtk

import Cellar.Grid.Model (GridHandlers (..))
import Cellar.Ref

-- | What a gesture noticed.  Every one of these is a fact about the widgets,
-- with no opinion about what the sheet should do next.
data GridGesture
  = LinePicked Axis Int         -- ^ A right-click on a row number or a heading.
  | DragBegan Axis Int          -- ^ A drag started on the line at this index.
  | DragMovedTo Int             -- ^ It is now over this one.
  | DragDroppedOn Int           -- ^ And it was let go over this one.
  | DragGaveUp
  deriving (Eq, Show)

-- | A drag in progress, as the widgets see it.
--
-- GtkColumnView can reorder its own columns by dragging a header, but that
-- moves the view's columns and not the sheet behind them: the letters would
-- come out in the wrong order and A1 would no longer be the cell in the
-- corner.  So the view's own reordering stays off and the drag is ours.
data Drag = Drag
  { dragAxis :: Axis
  , dragWidget :: Gtk.Widget
  , dragStartX :: Double
  , dragStartY :: Double
  , dragTarget :: Int
  }

-- | The widget side of one grid.
data Gestures = Gestures
  { gestureMenu :: Maybe Gio.MenuModel
  , gesturePost :: GridGesture -> IO ()
  , gestureView :: IORef (Maybe Gtk.ColumnView)
    -- | The header row, once it has been given its gestures.
    --
    -- The row, and not the headers in it.  A header is GtkColumnView's own
    -- widget and it builds new ones whenever the columns change -- which
    -- moving a column does, every time -- so a gesture on a header lasts until
    -- the first drag and no longer.  The row outlives them all, and which
    -- header a pointer is over is a question about where it is.
  , gestureRow :: IORef (Maybe Gtk.Widget)
  , gestureDrag :: IORef (Maybe Drag)
  }

-- | The widget side of a grid, which says what it noticed through this.
newGestures :: Maybe Gio.MenuModel -> (GridGesture -> IO ()) -> IO Gestures
newGestures menu post =
  Gestures menu post <$> newIORef Nothing <*> newIORef Nothing <*> newIORef Nothing

-- | What to do with each widget the grid's markup builds.
gestureHandlers :: Gestures -> GridHandlers
gestureHandlers gestures = GridHandlers
  { onCellBuilt = installCellHandles gestures
  , onGutterBuilt = installGutterHandles gestures
  , onViewBuilt = takeColumnView gestures
  }

-- | Show the drag on the column headers, which are not ours to draw.  The
-- cells draw themselves, from the model.
gestureDragShown :: Gestures -> Maybe (Axis, Int, Int) -> IO ()
gestureDragShown gestures drag = do
  entries <- headersAcross gestures
  forM_ (zip [0 ..] entries) $ \(position, title) -> do
    let index = case drag of
          Just (Column, _, _) -> Just (position :: Int)
          _ -> Nothing
        source = case (drag, index) of
          (Just (_, from, _), Just i) -> i == from
          _ -> False
        target = case (drag, index) of
          (Just (_, from, to), Just i) -> i == to && to /= from
          _ -> False
    setCssClass title "cellar-drag-source" source
    setCssClass title "cellar-drag-target" target

takeColumnView :: Gestures -> Gtk.ColumnView -> IO ()
takeColumnView gestures view = do
  writeIORef (gestureView gestures) (Just view)
  wireHeaderRow gestures
  -- The header row does not exist until the view is realised.
  void (GI.on view #realize (wireHeaderRow gestures))

--
-- The cells
--

installCellHandles :: Gestures -> Gtk.Label -> IO ()
installCellHandles gestures label = do
  widget' <- Gtk.toWidget label
  installLineClickOn gestures widget' Column (fmap (fmap refColumn) (refOf widget'))

installGutterHandles :: Gestures -> Gtk.Label -> IO ()
installGutterHandles gestures label = do
  widget' <- Gtk.toWidget label
  Gtk.widgetSetCursorFromName label (Just "grab")
  installRowDrag gestures widget'
  installLineClickOn gestures widget' Row (rowOf widget')

-- | Which cell a recycled widget is standing for, read back from its name.
refOf :: Gtk.Widget -> IO (Maybe Ref)
refOf widget' = parseRef . T.unpack <$> Gtk.widgetGetName widget'

rowOf :: Gtk.Widget -> IO (Maybe Int)
rowOf widget' = do
  name <- T.unpack <$> Gtk.widgetGetName widget'
  pure $ case splitAt (length ("gutter-" :: String)) name of
    ("gutter-", digits) | all (`elem` ("0123456789" :: String)) digits
                        , not (null digits) -> Just (read digits)
    _ -> Nothing

-- The context menu on a row number or a column heading

-- | The line menu, on a cell, a row number or the header row.
--
-- What line it is about is either known when the gesture is installed -- a
-- cell knows which row and column it stands for -- or worked out from where
-- the pointer is, which is what the header row does, since one gesture there
-- covers every column.
installLineClickOn :: Gestures -> Gtk.Widget -> Axis -> IO (Maybe Int) -> IO ()
installLineClickOn gestures label axis locate = forM_ (gestureMenu gestures) $ \model -> do
  handed <- Gtk.gestureClickNew
  gesture <- retain Gtk.GestureClick handed
  Gtk.gestureSingleSetButton gesture 3
  -- Capture, not bubble, because of where this ends up: a column heading is a
  -- GtkButton of GTK's own with a right-click gesture already on it, for the
  -- header menu a column can carry.  A gesture on the row that waits its turn
  -- never hears the press at all.
  Gtk.eventControllerSetPropagationPhase gesture Gtk.PropagationPhaseCapture
  _ <- GI.on gesture #pressed $ \_ x y -> do
    told <- locate
    found <- case told of
      Just index -> pure (Just index)
      Nothing -> fmap (\(position, _, _) -> position) <$> headerUnder gestures x
    forM_ found $ \index -> do
      void (Gtk.gestureSetState gesture Gtk.EventSequenceStateClaimed)
      gesturePost gestures (LinePicked axis index)
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
  Gtk.widgetAddController label handed

--
-- Dragging a row or a column
--

-- | Wide enough to keep clear of GTK's column resize handles, which live at
-- the edges of the same header widget the drag gesture is on.
resizeMargin :: Double
resizeMargin = 8

installRowDrag :: Gestures -> Gtk.Widget -> IO ()
installRowDrag gestures label = do
  handed <- Gtk.gestureDragNew
  gesture <- retain Gtk.GestureDrag handed
  _ <- GI.on gesture #dragBegin $ \x y -> do
    found <- rowOf label
    case found of
      Just position -> do
        _ <- Gtk.gestureSetState gesture Gtk.EventSequenceStateClaimed
        beginDrag gestures Row position label x y
      Nothing -> void (Gtk.gestureSetState gesture Gtk.EventSequenceStateDenied)
  _ <- GI.on gesture #dragUpdate (updateDrag gestures)
  _ <- GI.on gesture #dragEnd (finishDrag gestures)
  _ <- GI.on gesture #cancel (\_ -> cancelDrag gestures)
  Gtk.widgetAddController label handed

-- | Give the header row its gestures, once.
--
-- The headers are GtkColumnView's own widgets and there is no API that hands
-- them over -- a column has a title string, not a header factory -- so they
-- are reached by walking the view: its first child is the header row, whose
-- children are the column titles, the leading one belonging to the row gutter.
--
-- What is wired here is the row, because the headers do not last: the library
-- takes every column out and puts it back when their order changes, and GTK
-- builds fresh headers when it does.  A gesture on the row sees every press,
-- whichever header is under it and however many times they have been rebuilt.
wireHeaderRow :: Gestures -> IO ()
wireHeaderRow gestures = do
  already <- readIORef (gestureRow gestures)
  case already of
    Just _ -> pure ()
    Nothing -> do
      view <- readIORef (gestureView gestures)
      forM_ view $ \columnView' -> do
        header <- Gtk.widgetGetFirstChild columnView'
        forM_ header $ \row -> do
          writeIORef (gestureRow gestures) (Just row)
          installColumnDrag gestures row
          installLineClickOn gestures row Column (pure Nothing)

installColumnDrag :: Gestures -> Gtk.Widget -> IO ()
installColumnDrag gestures row = do
  handed <- Gtk.gestureDragNew
  gesture <- retain Gtk.GestureDrag handed
  -- Capture, not bubble.  A header has gestures of GTK's own -- one of them
  -- claims the sequence as soon as the pointer moves, and a bubble-phase
  -- gesture here sees the press and then nothing at all.
  Gtk.eventControllerSetPropagationPhase gesture Gtk.PropagationPhaseCapture
  _ <- GI.on gesture #dragBegin $ \x y -> do
    found <- headerUnder gestures x
    case found of
      Just (position, left, width)
        -- Near either edge the user is resizing the column, which is GTK's
        -- gesture on this same widget.  Stay out of its way.
        | x - left > resizeMargin && x - left < width - resizeMargin -> do
            _ <- Gtk.gestureSetState gesture Gtk.EventSequenceStateClaimed
            beginDrag gestures Column position row x y
      _ -> void (Gtk.gestureSetState gesture Gtk.EventSequenceStateDenied)
  _ <- GI.on gesture #dragUpdate (updateDrag gestures)
  _ <- GI.on gesture #dragEnd (finishDrag gestures)
  _ <- GI.on gesture #cancel (\_ -> cancelDrag gestures)
  Gtk.widgetAddController row handed

-- | The column whose header is at this point across the header row, with
-- where that header starts and how wide it is.
headerUnder :: Gestures -> Double -> IO (Maybe (Int, Double, Double))
headerUnder gestures x = do
  entries <- headersAcross gestures
  case entries of
    [] -> pure Nothing
    (firstHeader : _) -> do
      header <- Gtk.widgetGetParent firstHeader
      case header of
        Nothing -> pure Nothing
        Just row -> walk row (zip [0 ..] entries)
  where
    walk _ [] = pure Nothing
    walk row ((position, title) : more) = do
      origin <- translateBetween title row 0 0
      case origin of
        Nothing -> pure Nothing
        Just (left, _) -> do
          width <- fromIntegral <$> Gtk.widgetGetWidth title
          if x >= left && x < left + width
            then pure (Just (position, left, width))
            else walk row more

beginDrag :: Gestures -> Axis -> Int -> Gtk.Widget -> Double -> Double -> IO ()
beginDrag gestures axis from widget' x y = do
  writeIORef (gestureDrag gestures) (Just (Drag axis widget' x y from))
  gesturePost gestures (DragBegan axis from)

updateDrag :: Gestures -> Double -> Double -> IO ()
updateDrag gestures offsetX offsetY = do
  current <- readIORef (gestureDrag gestures)
  forM_ current $ \drag -> do
    target <- dragTargetAt gestures drag offsetX offsetY
    forM_ target $ \landing -> when (landing /= dragTarget drag) $ do
      writeIORef (gestureDrag gestures) (Just drag { dragTarget = landing })
      gesturePost gestures (DragMovedTo landing)

finishDrag :: Gestures -> Double -> Double -> IO ()
finishDrag gestures offsetX offsetY = do
  current <- readIORef (gestureDrag gestures)
  forM_ current $ \drag -> do
    target <- dragTargetAt gestures drag offsetX offsetY
    let landing = fromMaybe (dragTarget drag) target
    writeIORef (gestureDrag gestures) Nothing
    gesturePost gestures (DragDroppedOn landing)

cancelDrag :: Gestures -> IO ()
cancelDrag gestures = do
  writeIORef (gestureDrag gestures) Nothing
  gesturePost gestures DragGaveUp

-- | The row or column the pointer is over, so far into the drag.
dragTargetAt :: Gestures -> Drag -> Double -> Double -> IO (Maybe Int)
dragTargetAt gestures drag offsetX offsetY = do
  let x = dragStartX drag + offsetX
      y = dragStartY drag + offsetY
  case dragAxis drag of
    Row -> rowAt gestures (dragWidget drag) x y
    Column -> columnAt gestures (dragWidget drag) x

rowAt :: Gestures -> Gtk.Widget -> Double -> Double -> IO (Maybe Int)
rowAt gestures widget' x y = do
  view <- readIORef (gestureView gestures)
  case view of
    Nothing -> pure Nothing
    Just columnView' -> do
      destination <- Gtk.toWidget columnView'
      point <- translateBetween widget' destination x y
      case point of
        Nothing -> pure Nothing
        Just (viewX, viewY) -> do
          picked <- Gtk.widgetPick columnView' viewX viewY []
          cellRowAt picked 0

-- | The row of the cell under a point.
--
-- @gtk_widget_pick@ answers with whatever widget is deepest at that point,
-- which is the cell widget GtkColumnView wraps our label in, so this looks at
-- the child of the answer as well as at the answer itself, and then walks up.
-- What it is looking for is a widget whose name is a cell of ours.
cellRowAt :: Maybe Gtk.Widget -> Int -> IO (Maybe Int)
cellRowAt Nothing _ = pure Nothing
cellRowAt (Just widget') depth
  | depth >= 3 = pure Nothing
  | otherwise = do
      here <- indexOfCell widget'
      case here of
        Just row -> pure (Just row)
        Nothing -> do
          child <- Gtk.widgetGetFirstChild widget'
          below <- maybe (pure Nothing) indexOfCell child
          case below of
            Just row -> pure (Just row)
            Nothing -> do
              parent <- Gtk.widgetGetParent widget'
              cellRowAt parent (depth + 1)

indexOfCell :: Gtk.Widget -> IO (Maybe Int)
indexOfCell widget' = do
  asCell <- refOf widget'
  case asCell of
    Just r -> pure (Just (refRow r))
    Nothing -> rowOf widget'

-- | The column whose header contains a point.  Past the last header this is
-- the last column, and before the first, the first: a drag that overshoots
-- means the end of the sheet, not nothing at all.
columnAt :: Gestures -> Gtk.Widget -> Double -> IO (Maybe Int)
columnAt gestures widget' x = do
  entries <- headersAcross gestures
  case entries of
    [] -> pure Nothing
    (firstHeader : _) -> do
      header <- Gtk.widgetGetParent firstHeader
      case header of
        Nothing -> pure Nothing
        Just row -> do
          point <- translateBetween widget' row x 0
          case point of
            Nothing -> pure Nothing
            Just (headerX, _) -> walk row headerX (zip [0 ..] entries) Nothing
  where
    walk _ _ [] previous = pure previous
    walk row headerX ((position, title) : more) previous = do
      origin <- translateBetween title row 0 0
      case origin of
        Nothing -> pure Nothing
        Just (left, _) -> do
          width <- fromIntegral <$> Gtk.widgetGetWidth title
          if headerX < left
            then pure (Just (fromMaybe position previous))
            else if headerX < left + width
              then pure (Just position)
              else walk row headerX more (Just position)

-- | The headers in the order they lie across the screen, which is the order
-- the columns are in and not the order the header row holds them.
headersAcross :: Gestures -> IO [Gtk.Widget]
headersAcross gestures = do
  row <- readIORef (gestureRow gestures)
  case row of
    Nothing -> pure []
    Just header -> do
      -- The leading child is the gutter's heading, which stands for no column.
      gutter <- Gtk.widgetGetFirstChild header
      case gutter of
        Nothing -> pure []
        Just first -> do
          entries <- siblingsAfter first
          placed <- forM entries $ \title -> do
            origin <- translateBetween title header 0 0
            pure (maybe (1 / 0) fst origin, title)
          pure (map snd (sortOn fst placed))

-- | A point given in one widget's coordinates, in another's instead.
translateBetween :: Gtk.Widget -> Gtk.Widget -> Double -> Double
                 -> IO (Maybe (Double, Double))
translateBetween widget' target x y = do
  -- widgetComputePoint rather than the older widgetTranslateCoordinates, which
  -- GTK deprecated in 4.12.
  from <- Graphene.newZeroPoint
  Graphene.setPointX from (realToFrac x)
  Graphene.setPointY from (realToFrac y)
  (ok, to) <- Gtk.widgetComputePoint widget' target from
  if not ok then pure Nothing else do
    tx <- Graphene.getPointX to
    ty <- Graphene.getPointY to
    pure (Just (realToFrac tx, realToFrac ty))

setCssClass :: Gtk.Widget -> Text -> Bool -> IO ()
setCssClass widget' name wanted
  | wanted = Gtk.widgetAddCssClass widget' name
  | otherwise = Gtk.widgetRemoveCssClass widget' name

-- | Keep a reference of our own to a controller before handing it to a widget.
--
-- @gtk_widget_add_controller@ takes ownership, so haskell-gi disowns the value
-- passed to it.  Every gesture above then goes on to call @gestureSetState@
-- from inside its own callbacks -- which would be reading a pointer we no
-- longer hold, and which haskell-gi warns about at runtime as "accessing a
-- disowned pointer".
retain :: (GObject a, IsDescendantOf GObject.Object a)
       => (ManagedPtr a -> a) -> a -> IO a
retain constructor object = GObject.objectRef object >>= unsafeCastTo constructor

-- | Every widget after this one among its siblings.
siblingsAfter :: Gtk.Widget -> IO [Gtk.Widget]
siblingsAfter widget' = do
  next <- Gtk.widgetGetNextSibling widget'
  case next of
    Nothing -> pure []
    Just sibling -> (sibling :) <$> siblingsAfter sibling
