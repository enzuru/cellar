-- | A sheet as the window sees it.
--
-- A view is everything needed to draw a sheet and nothing that could compute
-- one: how big it is, and for each cell that has anything to show, the string
-- to put there, which way to align it, what colours to use, the error message
-- if it is one, and the source for the tooltip.
--
-- This is what stands where the sheet used to.  The grid cannot ask what a
-- cell comes to, because the thing that knows is in another process; so the
-- kernel sends the answers for the whole sheet at once and the grid reads them
-- out of here.  A scroll, a resize or a window being uncovered costs no talking
-- at all, and the last good view survives its kernel: if the kernel is wedged
-- on a cell that will not finish, the grid still has this and goes on drawing.
--
-- The two halves of a view come from two places and are put together here.
-- The kernel supplies what a cell comes to; the shell supplies what was typed
-- to make it, because the shell is the side that reads and writes the files.
module Cellar.View
  ( View (..)
  , Cell (..)
  , emptyView
  , viewFromSnapshot
  , viewHolds
  , cellAt
  , displayAt
  , numberAt
  , styleAt
  , errorAt
  , sourceAt
  ) where

import qualified Data.Map.Strict as M

import Cellar.Ref
import Cellar.Sexp

-- | One cell, ready to draw.
data Cell = Cell
  { cellDisplay :: String
    -- | Numbers are right-aligned and everything else is left-aligned.  The
    -- kernel decides this, because it is the last thing the shell would
    -- otherwise need a value for.
  , cellIsNumber :: Bool
  , cellColor :: Maybe String
  , cellBackground :: Maybe String
    -- | Why the cell is an error, and the tooltip it gets; 'Nothing' when it
    -- is not one.
  , cellError :: Maybe String
    -- | What was typed to make it.  The shell's, not the kernel's.
  , cellSource :: Maybe String
  } deriving (Eq, Show)

data View = View
  { viewRows :: Int
  , viewColumns :: Int
    -- | Only cells that have something to show.  A sheet is mostly empty --
    -- that is what a spreadsheet is -- so this is small even when the sheet is
    -- not.
  , viewCells :: M.Map String Cell
  } deriving (Eq, Show)

-- | A view of the right size with nothing in it.  What a tab shows before its
-- first snapshot has come back.
emptyView :: Int -> Int -> View
emptyView rows columns = View rows columns M.empty

-- | The view a kernel snapshot describes, with the shell's own sources laid
-- alongside it.
--
-- The kernel does not send sources back with every snapshot, and should not:
-- it was given them and they have not changed, so shipping them again with
-- every keystroke would be paying to be told what we already said.
viewFromSnapshot :: Sexp -> [(String, String)] -> View
viewFromSnapshot payload sources = View
  { viewRows = intOr 0 "rows"
  , viewColumns = intOr 0 "columns"
  , viewCells = M.fromList
      [ (name, cell) | Just entries <- [lookupKey "cells" payload >>= toList]
                     , entry <- entries
                     , Just (name, cell) <- [readCell entry] ]
  }
  where
    intOr fallback key = maybe fallback id (lookupKey key payload >>= asInt)

    -- (name display number? colour background error)
    readCell entry = case toList entry of
      Just [Str name, Str display, number, color, background, failure] ->
        Just (name, Cell
          { cellDisplay = display
          , cellIsNumber = asBool number
          , cellColor = asString color
          , cellBackground = asString background
          , cellError = asString failure
          , cellSource = lookup name sources
          })
      _ -> Nothing

-- | Is this reference inside the sheet at all?  Answered from the size the
-- last snapshot reported, which is the shell's only notion of how big a sheet
-- is.
viewHolds :: View -> Ref -> Bool
viewHolds view (Ref row column) =
  row >= 0 && row < viewRows view && column >= 0 && column < viewColumns view

cellAt :: View -> Ref -> Maybe Cell
cellAt view r = M.lookup (refName r) (viewCells view)

-- | The string to draw.  Empty for a cell with nothing in it, which is most of
-- them.
displayAt :: View -> Ref -> String
displayAt view r = maybe "" cellDisplay (cellAt view r)

numberAt :: View -> Ref -> Bool
numberAt view r = maybe False cellIsNumber (cellAt view r)

-- | The colour and background a cell asks to be drawn in.  Either may be
-- absent on its own.
styleAt :: View -> Ref -> (Maybe String, Maybe String)
styleAt view r = case cellAt view r of
  Nothing -> (Nothing, Nothing)
  Just cell -> (cellColor cell, cellBackground cell)

errorAt :: View -> Ref -> Maybe String
errorAt view r = cellAt view r >>= cellError

sourceAt :: View -> Ref -> Maybe String
sourceAt view r = cellAt view r >>= cellSource
