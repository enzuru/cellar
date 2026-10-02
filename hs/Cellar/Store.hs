-- | Workbooks and sheets on disk.
--
-- A sheet is a directory, not a file.  Every cell that holds anything is one
-- small file of Guile source under @cells/@, named for the cell, and a primary
-- file at the top holds what is true of the sheet rather than of any one cell.
--
-- A workbook is a directory of those -- what a tab bar shows, and what a git
-- repository holds one of:
--
-- >  budget.cellar/
-- >    workbook.scm     which sheets there are, and in what order
-- >    sheets/
-- >      Summary/
-- >        sheet.scm    the size of the sheet, and the column widths
-- >        cells/
-- >          A1.scm     Qty
-- >          D6.scm     (sum (range 'D2 'D4))
--
-- Nothing here evaluates anything, and nothing here knows what a sheet's cells
-- come to.  It deals in cell names and source text.  That is what let this
-- module move to the Haskell side of the pipe while the evaluator stayed in
-- Guile, and it is why the types below are all data and no behaviour.
module Cellar.Store
  ( -- * A workbook, which is one file
    Book (..)
  , bookFormat
  , bookText
  , parseBook
  , Workbook (..)
  , resolveWorkbook
  , workbookName
  , workbookFileName
  , createWorkbook
  , readBook
  , writeBook
    -- * Sheets, which are inside it
  , Sheet (..)
  , validSheetName
  , uniqueSheetName
    -- * Errors
  , StoreError (..)
  ) where

import Control.Exception (Exception, throwIO, try, SomeException)
import Control.Monad (when)
import Data.Char (isDigit)
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (fromMaybe)
import System.Directory
import qualified Data.Text.IO as TIO
import System.FilePath (takeFileName, takeDirectory, takeExtension)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (IOMode (..), hSetEncoding, utf8, withFile, hPutStr)

import Cellar.Sexp

-- | What a workbook file is called.
workbookExtension :: String
workbookExtension = ".cellar"

defaultSheetName :: String
defaultSheetName = "Sheet 1"

newtype StoreError = StoreError String
  deriving (Show)

instance Exception StoreError

refuse :: String -> IO a
refuse = throwIO . StoreError

-- | Everything a sheet's folder holds: its cells as name and source text, the
-- size it was saved at, and its column widths.
data Sheet = Sheet
  { sheetCells :: [(String, String)]
  , sheetRows :: Int
  , sheetColumns :: Int
  , sheetWidths :: [(Int, Int)]
  } deriving (Eq, Show)

--
-- A workbook, as one file
--

-- | Everything one workbook file holds: its sheets in the order their tabs
-- come in, each with its cells, and which of them was showing.
data Book = Book
  { bookSheets :: [(String, Sheet)]
  , bookActive :: Maybe String
  } deriving (Eq, Show)

bookFormat :: Integer
bookFormat = 3

-- | A whole workbook as the text of one file.
--
-- One cell to a line, and the cells in a fixed order.  That is what keeps an
-- edit to a single cell a single line of diff, and what lets two people who
-- edited different cells merge without being asked about it.  It is most of
-- what the folder of one file per cell was for.
bookText :: Book -> String
bookText book =
  ";; A Cellar workbook: every sheet, and every cell of each.\n"
    ++ "((format . " ++ show bookFormat ++ ")\n"
    ++ " (active . " ++ quoted (fromMaybe "" (bookActive book)) ++ ")\n"
    ++ " (sheets"
    ++ concatMap sheetForm (bookSheets book)
    ++ "))\n"
  where
    sheetForm (name, sheet) =
      "\n  (" ++ quoted name ++ "\n"
        ++ "   (rows . " ++ show (sheetRows sheet) ++ ")\n"
        ++ "   (columns . " ++ show (sheetColumns sheet) ++ ")\n"
        ++ "   (widths" ++ concatMap width (sortOn fst (sheetWidths sheet)) ++ ")\n"
        ++ "   (cells" ++ concatMap cell (sortOn fst (sheetCells sheet)) ++ "))"
    width (column, pixels) = " (" ++ show column ++ " . " ++ show pixels ++ ")"
    cell (name, source) = "\n    (" ++ quoted name ++ " . " ++ quoted source ++ ")"

quoted :: String -> String
quoted = T.unpack . writeSexp . Str

-- | Read a workbook file back.  Answers with what is wrong rather than
-- throwing, because the caller is a window.
parseBook :: Text -> Either String Book
parseBook text = do
  value <- parseSexp text
  pure Book
    { bookActive = case lookupKey "active" value >>= asString of
        Just "" -> Nothing
        other -> other
    , bookSheets =
        [ sheet
        | Just forms <- [lookupKey "sheets" value >>= toList]
        , Just sheet <- map sheetOf forms ]
    }
  where
    -- A sheet is its name and then an alist of what is true of it.
    sheetOf (Pair (Str name) rest) = Just (name, Sheet
      { sheetCells = [ (n, source)
                     | Just entries <- [lookupKey "cells" rest >>= toList]
                     , Pair (Str n) (Str source) <- entries ]
      , sheetRows = fromMaybe 0 (lookupKey "rows" rest >>= asInt)
      , sheetColumns = fromMaybe 0 (lookupKey "columns" rest >>= asInt)
      , sheetWidths = [ (fromIntegral column, fromIntegral pixels)
                      | Just entries <- [lookupKey "widths" rest >>= toList]
                      , Pair (Num column) (Num pixels) <- entries ]
      })
    sheetOf _ = Nothing

-- | Read a workbook off the disk.
readBook :: Workbook -> IO Book
readBook workbook = do
  text <- TIO.readFile (workbookRoot workbook)
  case parseBook text of
    Left why -> refuse (workbookRoot workbook ++ " is not a Cellar workbook: " ++ why)
    Right book -> pure book

-- | Write a workbook out.  Nothing happens when the file already says this,
-- so a save that changes nothing does not touch the folder's modification
-- time and does not wake the watcher.
writeBook :: Workbook -> Book -> IO ()
writeBook workbook book = writeIfChanged (workbookRoot workbook) (bookText book)

-- Paths

-- | Write text to a path, unless the path already holds exactly that.
--
-- Worth the read it costs.  Cellar rewrites every cell of a sheet for a single
-- moved row, and most of those files are not changing; leaving them alone
-- keeps their mtimes still, keeps @git status@ honest about what was edited,
-- and -- since the workbook folder is watched -- stops Cellar waking itself up
-- over its own writes.
writeIfChanged :: FilePath -> String -> IO ()
writeIfChanged path text = do
  current <- try (readUtf8 path) :: IO (Either SomeException Text)
  case current of
    Right existing | existing == T.pack text -> pure ()
    _ -> withFile path WriteMode $ \handle -> do
           hSetEncoding handle utf8
           hPutStr handle text

-- | A file's contents as text, decoded as UTF-8 whatever the locale says.
readUtf8 :: FilePath -> IO Text
readUtf8 path = withFile path ReadMode $ \handle -> do
  hSetEncoding handle utf8
  contents <- TIO.hGetContents handle
  T.length contents `seq` pure contents

-- Reading

trim :: String -> String
trim = dropWhile isSpace' . reverse . dropWhile isSpace' . reverse
  where isSpace' c = c `elem` (" \t\n\r" :: String)

-- Workbooks

-- | An open workbook: where it is, and how its sheets are laid out inside it.
--
-- Resolved once, when the workbook is opened, and then answered from.  The
-- alternative -- which this replaced -- was to take a 'FilePath' everywhere
-- and work the layout out again on each call, which meant a @doesFileExist@
-- per sheet per snapshot and put the older format in an @if@ rather than in
-- the type.
-- | A workbook: the file it is kept in.
newtype Workbook = Workbook { workbookRoot :: FilePath }
  deriving (Eq, Show)

-- | Where a workbook keeps its sheets.
-- | Work out the shape of the workbook a path names, or answer 'Nothing'
-- when it names none.
--
-- This is the only function that looks at a folder to decide what shape it is
-- in.  Everything after it is told.
resolveWorkbook :: FilePath -> IO (Maybe Workbook)
resolveWorkbook path = do
  isFile <- doesFileExist path
  pure $ if isFile && takeExtension path == workbookExtension
           then Just (Workbook path)
           else Nothing

-- | What to call the workbook in a window title.
workbookName :: Workbook -> String
workbookName = takeFileName . workbookRoot

-- | Can this be a sheet?  It becomes a folder name and is written into an
-- index that is read back with @read@, so it has to be a name a folder can
-- have: not empty, not a path, and not hidden -- which rules out @.@ and @..@
-- along with it.
validSheetName :: String -> Bool
validSheetName raw = case trim raw of
  [] -> False
  name@(first : _) ->
    length name <= 64
      && '/' `notElem` name
      && '\0' `notElem` name
      && first /= '.'

-- | Make a workbook holding one empty sheet.
--
-- Answers with the workbook it made, so that the caller does not have to
-- resolve the path it has just written.
createWorkbook :: FilePath -> String -> IO Workbook
createWorkbook path rawName = do
  already <- doesPathExist path
  when already $ refuse (path ++ " is already there")
  let name = if validSheetName rawName then trim rawName else defaultSheetName
      workbook = Workbook path
  createDirectoryIfMissing True (takeDirectory path)
  writeBook workbook (Book [(name, emptySheet)] (Just name))
  pure workbook

-- | A sheet with nothing in it, at the size a new one gets.  How much empty
-- room that is would be a question about what looks right in a window, so the
-- window answers it and this is only what a sheet is before anybody says.
emptySheet :: Sheet
emptySheet = Sheet [] 0 0 []

-- | A free name, or one with a different number after it.  What the Add Sheet
-- dialog suggests.
--
-- Pure now that a workbook is one value: the names it already has are in hand,
-- so there is nothing to go and look at.
uniqueSheetName :: [String] -> String -> String
uniqueSheetName names base'
  | base `notElem` names = base
  | otherwise = case [ candidate
                     | let (stem, n) = splitTrailingNumber base
                     , candidate <- [ stem ++ show k | k <- [n + 1 ..] ]
                     , candidate `notElem` names ] of
      (free : _) -> free
      [] -> defaultSheetName
  where base = if validSheetName base' then trim base' else defaultSheetName

-- | A name split into what comes before a trailing number and the number
-- itself, so that the sheet after @Sheet 2@ is @Sheet 3@ and the one after
-- @Q1@ is @Q2@.
--
-- The split keeps whatever stood between the two exactly as it was -- a space
-- in the one case, nothing at all in the other -- so a suggested name is
-- spelled the way the name it was suggested from is.  A name with no number on
-- the end is given a space and a 1, which makes @Summary@ into @Summary 2@.
splitTrailingNumber :: String -> (String, Int)
splitTrailingNumber name =
  let (digitsBackwards, stemBackwards) = span isDigit (reverse name)
      trailing = reverse digitsBackwards
      stem = reverse stemBackwards
  in if null trailing || null stem
       then (name ++ " ", 1)
       else (stem, read trailing)

-- | A workbook's folder is named like a file would be: @budget@ becomes
-- @budget.cellar@.
workbookFileName :: String -> String
workbookFileName name
  | ".cellar" `isSuffixOf` name = name
  | otherwise = name ++ ".cellar"
