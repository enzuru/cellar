-- | Cell references, and where they land when the sheet is rearranged.
--
-- A reference is a row and a column, both 0-based.  Users see them as @A1@,
-- where the column is a letter and the row is 1-based.
--
-- This is the one thing both halves of Cellar know about.  The kernel needs
-- references because it evaluates cells that name each other; the shell needs
-- them because a cell's file is named for the cell, because the grid draws
-- column headers, and because the active cell has to follow the row it is
-- sitting on when that row moves.
--
-- The Guile side has the same module, and has to: this is the vocabulary the
-- two of them share.  Nothing here evaluates anything or knows what a sheet
-- is, which is why it can sit under both.
module Cellar.Ref
  ( Ref (..)
  , Axis (..)
  , columnName
  , parseColumn
  , refName
  , parseRef
  , shiftIndex
  , refAfterMove
  , shiftIndexForInsert
  , refAfterInsert
  ) where

import Data.Char (isDigit, ord, chr)

-- | A cell, by position.  Both fields are 0-based.
data Ref = Ref { refRow :: !Int, refColumn :: !Int }
  deriving (Eq, Ord, Show)

-- | Which way a row or column moves.  A great deal of Cellar is the same
-- operation along one axis or the other, and saying so once is what keeps
-- @moveRow@ and @moveColumn@ from being two copies of the same reasoning.
data Axis = Row | Column
  deriving (Eq, Show)

-- | A 0-based column index as spreadsheet letters: 0 becomes @A@, 26 @AA@.
columnName :: Int -> String
columnName = go []
  where
    go acc n =
      let letter = chr (ord 'A' + n `mod` 26)
          rest = n `div` 26
      in if rest == 0 then letter : acc else go (letter : acc) (rest - 1)

-- | The inverse.  'Nothing' if the text is not all @A@-@Z@.
parseColumn :: String -> Maybe Int
parseColumn "" = Nothing
parseColumn s
  | all (\c -> c >= 'A' && c <= 'Z') s =
      Just (foldl' step 0 s - 1)
  | otherwise = Nothing
  where step acc c = acc * 26 + 1 + (ord c - ord 'A')

-- | How a reference is written: @A1@.
refName :: Ref -> String
refName (Ref row column) = columnName column ++ show (row + 1)

-- | Read @A1@ back.  'Nothing' if it is not a reference at all.
parseRef :: String -> Maybe Ref
parseRef s =
  case break isDigit s of
    ([], _) -> Nothing
    (_, []) -> Nothing
    (letters, digits)
      | not (all isDigit digits) -> Nothing
      | otherwise -> do
          column <- parseColumn letters
          row <- readRow digits
          if row >= 1 then Just (Ref (row - 1) column) else Nothing
  where
    readRow d = case reads d :: [(Int, String)] of
      [(n, "")] -> Just n
      _ -> Nothing

-- Where a reference lands when the sheet is rearranged.
--
-- Both halves need this arithmetic and neither owns it.  The kernel rewrites
-- the references inside cell sources when a row moves; the shell keeps the
-- active cell on the same cell across the same move, without waiting to be
-- told where it went.

-- | Where index @i@ lands when the item at @from@ is moved to @to@ and the
-- indices in between slide over by one.
shiftIndex :: Int -> Int -> Int -> Int
shiftIndex i from to
  | i == from = to
  | from < i && i <= to = i - 1
  | to <= i && i < from = i + 1
  | otherwise = i

-- | Where a reference lands when @from@ is moved to @to@ along an axis.
refAfterMove :: Ref -> Axis -> Int -> Int -> Ref
refAfterMove (Ref row column) axis from to = case axis of
  Row -> Ref (shiftIndex row from to) column
  Column -> Ref row (shiftIndex column from to)

-- | Where index @i@ lands when a new line is opened at @at@.
shiftIndexForInsert :: Int -> Int -> Int
shiftIndexForInsert i at = if i >= at then i + 1 else i

-- | Where a reference lands when a line is inserted along an axis.
refAfterInsert :: Ref -> Axis -> Int -> Ref
refAfterInsert (Ref row column) axis at = case axis of
  Row -> Ref (shiftIndexForInsert row at) column
  Column -> Ref row (shiftIndexForInsert column at)
