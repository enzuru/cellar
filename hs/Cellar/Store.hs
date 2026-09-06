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
  ( -- * Sheets
    Sheet (..)
  , sheetDirectory
  , isSheetDirectory
  , createSheetDirectory
  , saveSheet
  , readSheet
  , readSheetCells
  , readSheetMetadata
  , saveCell
  , touchCell
  , cellFilePath
    -- * Workbooks
  , Workbook (..)
  , SheetLayout (..)
  , resolveWorkbook
  , workbookDirectory
  , isWorkbookDirectory
  , workbookName
  , isFormatOne
  , createWorkbook
  , workbookSheetNames
  , workbookActiveSheet
  , workbookSheetDirectory
  , workbookWatchPaths
  , writeWorkbookIndex
  , setWorkbookActive
  , setWorkbookOrder
  , addWorkbookSheet
  , renameWorkbookSheet
  , removeWorkbookSheet
  , validSheetName
  , uniqueSheetName
  , workbookFolderName
    -- * Errors
  , StoreError (..)
  ) where

import Control.Exception (Exception, throwIO, try, SomeException)
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Char (isDigit, toLower)
import Data.List (isSuffixOf, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import System.Directory
import System.FilePath ((</>), takeFileName, takeDirectory, takeExtension)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (IOMode (..), hSetEncoding, utf8, withFile, hPutStr)

import Cellar.Ref
import Cellar.Sexp

primaryFile :: String
primaryFile = "sheet.scm"

cellsDirectory :: String
cellsDirectory = "cells"

cellSuffix :: String
cellSuffix = ".scm"

workbookFile :: String
workbookFile = "workbook.scm"

sheetsDirectory :: String
sheetsDirectory = "sheets"

-- | The sheet format is untouched by tabs, so @sheet.scm@ still says 1.  What
-- is new is the document around it, and @workbook.scm@ says 2 for the layout a
-- reader would need to understand to find the sheets at all.
sheetFormat, workbookFormat :: Integer
sheetFormat = 1
workbookFormat = 2

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

-- Paths

-- | The sheet directory a path names.  A sheet can be opened by its directory
-- or by the primary file inside it, and both arrive here as the directory.
sheetDirectory :: FilePath -> IO FilePath
sheetDirectory path = do
  isDir <- doesDirectoryExist path
  exists <- doesFileExist path
  pure $ if exists && not isDir && takeFileName path == primaryFile
           then takeDirectory path
           else path

-- | Is this a Cellar sheet -- a directory with a primary file in it?
isSheetDirectory :: FilePath -> IO Bool
isSheetDirectory path = do
  directory <- sheetDirectory path
  isDir <- doesDirectoryExist directory
  if not isDir then pure False else doesFileExist (directory </> primaryFile)

cellsIn :: FilePath -> FilePath
cellsIn directory = directory </> cellsDirectory

-- | The file cell @name@ lives in, which may not exist yet -- an empty cell is
-- a cell with no file.  Public because an external editor is pointed straight
-- at it.
cellFilePath :: FilePath -> String -> FilePath
cellFilePath directory name = cellsIn directory </> (name ++ cellSuffix)

-- Making one

-- | An empty sheet folder.  Refuses to write over one that is already there.
createSheetDirectory :: FilePath -> IO ()
createSheetDirectory directory = do
  already <- isSheetDirectory directory
  when already $ refuse (directory ++ " is already a Cellar sheet")
  createDirectoryIfMissing True directory
  createDirectoryIfMissing True (cellsIn directory)
  writePrimary directory 0 0 []

-- Writing

-- | Write a sheet to a folder: the primary file, a file for every cell given,
-- and no file for any cell that is not.
--
-- Nothing here knows what a sheet is.  It is handed the cells as text and
-- writes them as text, which is what lets this module sit beside the part of
-- Cellar that draws a window rather than beside the part that evaluates
-- Scheme -- they are different programs now, in different languages, and only
-- one of them has any business holding a value.
saveSheet :: FilePath -> Sheet -> IO ()
saveSheet directory sheet = do
  createDirectoryIfMissing True directory
  createDirectoryIfMissing True (cellsIn directory)
  writePrimary directory (sheetRows sheet) (sheetColumns sheet) (sheetWidths sheet)
  forM_ (sheetCells sheet) $ \(name, source) -> writeCell directory name source
  stored <- storedCellNames directory
  let live = map fst (sheetCells sheet)
  forM_ stored $ \name ->
    unless (name `elem` live) $ removeFile (cellFilePath directory name)

-- | Write one cell to its own file, or take the file away when the cell is
-- empty.  This is how an edit reaches the disk -- a sheet is saved a cell at a
-- time, so the file for a cell is current the moment you finish typing it.
saveCell :: FilePath -> String -> Maybe String -> IO ()
saveCell directory name source = do
  createDirectoryIfMissing True directory
  createDirectoryIfMissing True (cellsIn directory)
  let file = cellFilePath directory name
  case source of
    Just text | not (all (`elem` (" \t\n\r" :: String)) text) ->
      writeCell directory name text
    _ -> do
      exists <- doesFileExist file
      when exists $ removeFile file

-- | Make sure a cell has a file, and answer with its path.  A cell that
-- already has one is left exactly as it is.
--
-- An empty cell has no file -- 'saveCell' takes it away -- and another program
-- cannot be handed a path that is not there.  Opening an empty cell elsewhere
-- therefore starts by giving it an empty file to open; nothing is written to
-- the cell by that, and the next save of an empty cell removes the file again.
touchCell :: FilePath -> String -> IO FilePath
touchCell directory name = do
  createDirectoryIfMissing True directory
  createDirectoryIfMissing True (cellsIn directory)
  let file = cellFilePath directory name
  exists <- doesFileExist file
  unless exists $ withFile file WriteMode (\_ -> pure ())
  pure file

writeCell :: FilePath -> String -> String -> IO ()
writeCell directory name source =
  -- A trailing newline: these are text files, and diffs of files without one
  -- are a nuisance to read.
  writeIfChanged (cellFilePath directory name)
    (if "\n" `isSuffixOf` source then source else source ++ "\n")

writePrimary :: FilePath -> Int -> Int -> [(Int, Int)] -> IO ()
writePrimary directory rows columns widths =
  writeIfChanged (directory </> primaryFile) (primaryText rows columns widths)

primaryText :: Int -> Int -> [(Int, Int)] -> String
primaryText rows columns widths =
  -- An entry to a line, so that changing the size of a sheet is a one-line
  -- diff rather than a rewritten file.
  ";; A Cellar sheet. The cells are in cells/, one file each.\n"
    ++ "((format . " ++ show sheetFormat ++ ")\n"
    ++ " (rows . " ++ show rows ++ ")\n"
    ++ " (columns . " ++ show columns ++ ")\n"
    ++ " (widths" ++ concatMap width widths ++ "))\n"
  where width (column, pixels) = " (" ++ show column ++ " . " ++ show pixels ++ ")"

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

-- | Everything the sheet at a directory holds.
--
-- Reading is all this does.  The size is reported rather than applied, because
-- a sheet is at least as big as it was saved -- it can be taller than its last
-- full row, and those empty rows are part of what was saved -- and deciding
-- that is the business of whatever owns the sheet, which is the kernel.
readSheet :: FilePath -> IO Sheet
readSheet directory = do
  ok <- isSheetDirectory directory
  unless ok $ refuse (directory ++ " is not a Cellar sheet")
  metadata <- readSheetMetadata directory
  cells <- readSheetCells directory
  pure Sheet
    { sheetCells = cells
    , sheetRows = fromMaybe 0 (intAt "rows" metadata)
    , sheetColumns = fromMaybe 0 (intAt "columns" metadata)
    , sheetWidths = widthsAt metadata
    }

-- | Every cell on disk, sorted by name so that two readings of an unchanged
-- directory compare equal.
readSheetCells :: FilePath -> IO [(String, String)]
readSheetCells directory = do
  names <- storedCellNames directory
  cells <- forM (sort names) $ \name -> do
    text <- readUtf8 (cellFilePath directory name)
    pure (name, T.unpack (T.strip text))
  pure cells

readSheetMetadata :: FilePath -> IO Sexp
readSheetMetadata directory = do
  contents <- try (readUtf8 (directory </> primaryFile))
                :: IO (Either SomeException Text)
  pure $ case contents of
    Left _ -> Nil
    Right text -> either (const Nil) id (parseSexp text)

intAt :: String -> Sexp -> Maybe Int
intAt key metadata = lookupKey key metadata >>= asInt

widthsAt :: Sexp -> [(Int, Int)]
widthsAt metadata = case lookupKey "widths" metadata >>= toList of
  Nothing -> []
  Just entries -> mapMaybe pairOf entries
  where
    pairOf (Pair a b) = (,) <$> asInt a <*> asInt b
    pairOf _ = Nothing

-- | The cells with a file in a directory, by name.
--
-- Only files named for a cell are answered with, and so only those are ever
-- deleted by a save.  A sheet directory is a place a person can keep a README
-- or a .gitignore, and nothing here may touch them.
storedCellNames :: FilePath -> IO [String]
storedCellNames directory = do
  let cells = cellsIn directory
  exists <- doesDirectoryExist cells
  if not exists then pure [] else do
    entries <- listDirectory cells
    pure (mapMaybe cellFileName entries)

-- | The cell a file is named for, or 'Nothing' when it is named for no cell.
cellFileName :: FilePath -> Maybe String
cellFileName file
  | takeExtension file /= cellSuffix = Nothing
  | otherwise =
      let name = take (length file - length cellSuffix) file
      -- parseRef is lenient about what it will accept; the file has to be
      -- spelled exactly the way the cell would be written.
      in case parseRef name of
           Just r | refName r == name -> Just name
           _ -> Nothing

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
data Workbook = Workbook
  { workbookRoot :: FilePath
  , workbookLayout :: SheetLayout
  } deriving (Eq, Show)

-- | Where a workbook keeps its sheets.
data SheetLayout
    -- | The current format: a folder each, under @sheets/@.
  = SheetsUnder
    -- | Written before there were tabs: one sheet, lying at the top of the
    -- workbook's own folder, and named after it.
  | SingleSheet String
  deriving (Eq, Show)

-- | Work out the shape of the workbook a path names, or answer 'Nothing'
-- when it names none.
--
-- This is the only function that looks at a folder to decide what shape it is
-- in.  Everything after it is told.
resolveWorkbook :: FilePath -> IO (Maybe Workbook)
resolveWorkbook path = do
  directory <- workbookDirectory path
  isDir <- doesDirectoryExist directory
  if not isDir then pure Nothing else do
    indexed <- doesFileExist (directory </> workbookFile)
    if indexed
      then pure (Just (Workbook directory SheetsUnder))
      else do
        legacy <- doesFileExist (directory </> primaryFile)
        pure $ if legacy
          then Just (Workbook directory (SingleSheet (legacySheetName directory)))
          else Nothing

-- | Is this a workbook from before tabs?
isFormatOne :: Workbook -> Bool
isFormatOne workbook = case workbookLayout workbook of
  SingleSheet _ -> True
  SheetsUnder -> False

-- | What to call the workbook in a window title.
workbookName :: Workbook -> String
workbookName = takeFileName . workbookRoot

-- | The folder a sheet lives in.
--
-- In a workbook from before tabs that is the workbook's own folder, which is
-- exactly what makes one readable where it lies.  Pure, because the layout was
-- settled when the workbook was opened.
workbookSheetDirectory :: Workbook -> String -> FilePath
workbookSheetDirectory workbook name = case workbookLayout workbook of
  SingleSheet _ -> workbookRoot workbook
  SheetsUnder -> workbookRoot workbook </> sheetsDirectory </> name

-- | The workbook directory a path names.  A workbook can be pointed at by its
-- own folder, by its @workbook.scm@, or by the @sheet.scm@ of any sheet inside
-- it, and all three arrive here as the folder.
workbookDirectory :: FilePath -> IO FilePath
workbookDirectory path = do
  isDir <- doesDirectoryExist path
  exists <- doesFileExist path
  pure $ if isDir || not exists
    then path
    else case takeFileName path of
      name | name == workbookFile -> takeDirectory path
           | name == primaryFile ->
               let sheet = takeDirectory path
                   parent = takeDirectory sheet
               -- sheets/Q1/sheet.scm is two folders down from the workbook;
               -- the sheet.scm of a workbook from before tabs is one.
               in if takeFileName parent == sheetsDirectory
                    then takeDirectory parent
                    else sheet
           | otherwise -> path

-- | Is this a Cellar workbook -- a folder with an index in it, or a single
-- sheet from before there were tabs?
isWorkbookDirectory :: FilePath -> IO Bool
isWorkbookDirectory path = do
  directory <- workbookDirectory path
  isDir <- doesDirectoryExist directory
  if not isDir then pure False else do
    indexed <- doesFileExist (directory </> workbookFile)
    if indexed then pure True else isSheetDirectory directory

-- | The folder's name without the extension: @budget.cellar@ becomes @budget@.
bareName :: FilePath -> String
bareName directory =
  let base = takeFileName directory
  in if ".cellar" `isSuffixOf` base
       then take (length base - length ".cellar") base
       else base

-- | What to call the one sheet of a workbook from before tabs.  Its folder is
-- the workbook's, so it has no name of its own and borrows the workbook's --
-- @budget.cellar@ opens with a tab that says @budget@, which is what it was
-- already being called in the title bar.
legacySheetName :: FilePath -> String
legacySheetName directory =
  let name = bareName directory
  in if validSheetName name then trim name else defaultSheetName

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
-- The git repository is made around the workbook rather than around the sheet,
-- which is the whole reason for this layer: several spreadsheets, one history.
-- Making it is the caller's business; this only lays out the folder.
createWorkbook :: FilePath -> String -> IO ()
createWorkbook directory rawName = do
  already <- isWorkbookDirectory directory
  when already $ refuse (directory ++ " is already a Cellar workbook")
  let name = if validSheetName rawName then trim rawName else defaultSheetName
  createDirectoryIfMissing True directory
  createDirectoryIfMissing True (directory </> sheetsDirectory)
  createSheetDirectory (directory </> sheetsDirectory </> name)
  writeWorkbookIndex directory [name] (Just name)

-- | What the index says.  A workbook from before tabs has no index and answers
-- as though it had one naming its single sheet.
readWorkbookIndex :: Workbook -> IO Sexp
readWorkbookIndex workbook = case workbookLayout workbook of
  SingleSheet name ->
    pure (list [ Pair (Sym "format") (Num 1)
               , list [Sym "sheets", Str name]
               , Pair (Sym "active") (Str name) ])
  SheetsUnder -> do
    contents <- try (readUtf8 (workbookRoot workbook </> workbookFile))
                  :: IO (Either SomeException Text)
    pure $ case contents of
      Left _ -> Nil
      Right text -> either (const Nil) id (parseSexp text)

-- | The sheets of a workbook, in the order the tabs should show them.
--
-- What is on disk decides which sheets there are, and the index decides only
-- their order.  So a sheet that arrived in someone else's commit turns up as a
-- tab rather than being ignored, and one that a @git checkout@ took away leaves
-- rather than being a tab over a folder that is not there.  That makes the
-- index a hint, which is the most that a file two people can edit at once
-- should be.
workbookSheetNames :: Workbook -> IO [String]
workbookSheetNames workbook = do
  onDisk <- storedSheetNames workbook
  index <- readWorkbookIndex workbook
  let listed = [ name | name <- indexSheetNames index, name `elem` onDisk ]
      unlisted = sort [ name | name <- onDisk, name `notElem` listed ]
  pure (listed ++ unlisted)

indexSheetNames :: Sexp -> [String]
indexSheetNames index = case lookupKey "sheets" index >>= toList of
  Nothing -> []
  Just entries -> [ name | Str name <- entries, validSheetName name ]

-- | The sheets that actually have a folder with a sheet in it.
storedSheetNames :: Workbook -> IO [String]
storedSheetNames workbook = case workbookLayout workbook of
  SingleSheet name -> pure [name]
  SheetsUnder -> do
    folders <- sheetFolderNames workbook
    filterM (isSheetDirectory . workbookSheetDirectory workbook) folders

-- | Every folder directly under @sheets/@, whether or not there is a sheet in
-- it yet.
sheetFolderNames :: Workbook -> IO [String]
sheetFolderNames workbook = case workbookLayout workbook of
  SingleSheet _ -> pure []
  SheetsUnder -> do
    let sheets = workbookRoot workbook </> sheetsDirectory
    exists <- doesDirectoryExist sheets
    if not exists then pure [] else do
      entries <- listDirectory sheets
      filterM (\name -> if validSheetName name
                          then doesDirectoryExist (sheets </> name)
                          else pure False)
              entries

-- | The sheet whose tab was showing when the workbook was last written, or the
-- first one when that sheet is no longer there.
workbookActiveSheet :: Workbook -> IO (Maybe String)
workbookActiveSheet workbook = do
  names <- workbookSheetNames workbook
  index <- readWorkbookIndex workbook
  let stored = lookupKey "active" index >>= asString
  pure $ case stored of
    Just active | active `elem` names -> Just active
    _ -> case names of
      (first : _) -> Just first
      [] -> Nothing

-- | Every path that has to be watched for the workbook to notice a change to
-- itself.
--
-- A folder under @sheets/@ is watched whether or not there is a sheet in it
-- yet.  A sheet arriving in a @git checkout@ is a folder that appears and is
-- filled in a moment afterwards, and watching only the folders that are
-- already sheets would mean hearing about that one while it was still empty
-- and never hearing about it again.
workbookWatchPaths :: Workbook -> IO [FilePath]
workbookWatchPaths workbook = do
  let root = workbookRoot workbook
      sheets = root </> sheetsDirectory
  folders <- sheetFolderNames workbook
  names <- workbookSheetNames workbook
  let perSheet name =
        let sheet = workbookSheetDirectory workbook name
        in [cellsIn sheet, sheet </> primaryFile]
  pure $ [root </> workbookFile, sheets]
      ++ map (sheets </>) folders
      ++ concatMap perSheet names

-- | Write which sheets there are and which one is showing.
writeWorkbookIndex :: FilePath -> [String] -> Maybe String -> IO ()
writeWorkbookIndex root names active = do
  createDirectoryIfMissing True root
  writeIfChanged (root </> workbookFile) (indexText names active)

indexText :: [String] -> Maybe String -> String
indexText names active =
  -- A line to an entry and a line to a sheet, so that adding a sheet, renaming
  -- one or dragging a tab is a one-line diff rather than a rewritten file.
  ";; A Cellar workbook. Each sheet is a folder under sheets/.\n"
    ++ "((format . " ++ show workbookFormat ++ ")\n"
    ++ " (sheets"
    ++ concatMap (\name -> "\n  " ++ quoted name) names ++ ")\n"
    ++ " (active . " ++ quoted (fromMaybe "" active) ++ "))\n"
  where quoted = T.unpack . writeSexp . Str

-- | Remember which tab was showing.  A workbook from before tabs has one sheet
-- and no index to write this into, and does not miss it.
setWorkbookActive :: Workbook -> String -> IO ()
setWorkbookActive workbook name =
  unless (isFormatOne workbook) $ do
    names <- workbookSheetNames workbook
    writeWorkbookIndex (workbookRoot workbook) names (Just name)

-- | Remember the order the tabs are in.
setWorkbookOrder :: Workbook -> [String] -> IO ()
setWorkbookOrder workbook names =
  unless (isFormatOne workbook) $ do
    active <- workbookActiveSheet workbook
    writeWorkbookIndex (workbookRoot workbook) names active

-- | Move a workbook from before tabs into @sheets/@, so that it can hold a
-- second sheet.  Returns the name its one sheet now has.
--
-- The folder is renamed, not copied, so that git sees a rename rather than a
-- deletion and an unrelated new file, and @git log --follow@ still walks back
-- through a cell's history.
--
-- Nothing calls this until a second sheet is actually asked for.  A single
-- sheet is perfectly readable where it lies, and rearranging somebody's
-- repository on the way to merely opening it would be a rude way to say hello.
migrateWorkbook :: Workbook -> String -> IO Workbook
migrateWorkbook workbook name = do
  let root = workbookRoot workbook
      target = root </> sheetsDirectory </> name
  createDirectoryIfMissing True (root </> sheetsDirectory)
  createDirectoryIfMissing True target
  renameFile (root </> primaryFile) (target </> primaryFile)
  hasCells <- doesDirectoryExist (cellsIn root)
  when hasCells $ renameDirectory (cellsIn root) (cellsIn target)
  writeWorkbookIndex root [name] (Just name)
  -- The layout has changed, and the type says so: everything holding the old
  -- value is holding a description of a folder that no longer looks like that.
  pure workbook { workbookLayout = SheetsUnder }

-- | Is there already a sheet by this name?  Compared without regard to case,
-- because on a good many filesystems @Q1@ and @q1@ would be one folder.
taken :: Workbook -> String -> IO Bool
taken workbook name = do
  names <- workbookSheetNames workbook
  pure (map toLower name `elem` map (map toLower) names)

-- | Add an empty sheet, and return the name it was given.
-- | Add an empty sheet, and answer with the workbook as it now is and the name
-- the sheet was given.  The workbook comes back because adding a second sheet
-- to one written before there were tabs moves the first, and after that the
-- folder is laid out differently than it was.
addWorkbookSheet :: Workbook -> String -> IO (Workbook, String)
addWorkbookSheet workbook rawName = do
  unless (validSheetName rawName) $
    refuse "A sheet needs a name, and not one with a / in it"
  let name = trim rawName
  moved <- case workbookLayout workbook of
    SingleSheet only -> migrateWorkbook workbook only
    SheetsUnder -> pure workbook
  clash <- taken moved name
  when clash $
    refuse ("This workbook already has a sheet called " ++ name)
  -- Read the order before the folder exists, or the new sheet would be found
  -- on disk and appended to the index twice.
  existing <- workbookSheetNames moved
  createDirectoryIfMissing True (workbookRoot moved </> sheetsDirectory)
  createSheetDirectory (workbookSheetDirectory moved name)
  writeWorkbookIndex (workbookRoot moved) (existing ++ [name]) (Just name)
  pure (moved, name)

-- | Rename a sheet, folder and all.  Returns the name it now has.
-- | Rename a sheet, folder and all.  As with adding one, the workbook comes
-- back, because renaming the only sheet of an older workbook moves it first.
renameWorkbookSheet :: Workbook -> String -> String -> IO (Workbook, String)
renameWorkbookSheet workbook old rawNew = do
  unless (validSheetName rawNew) $
    refuse "A sheet needs a name, and not one with a / in it"
  let new = trim rawNew
  if old == new then pure (workbook, new) else do
    moved <- case workbookLayout workbook of
      SingleSheet only -> migrateWorkbook workbook only
      SheetsUnder -> pure workbook
    clash <- taken moved new
    when (clash && map toLower old /= map toLower new) $
      refuse ("This workbook already has a sheet called " ++ new)
    names <- workbookSheetNames moved
    unless (old `elem` names) $ refuse ("There is no sheet called " ++ old)
    active <- workbookActiveSheet moved
    -- Both worked out before the folder moves, since neither can be read back
    -- afterwards under the name it was asked about.
    renameDirectory (workbookSheetDirectory moved old)
                    (workbookSheetDirectory moved new)
    writeWorkbookIndex (workbookRoot moved)
      (map (\name -> if name == old then new else name) names)
      (if active == Just old then Just new else active)
    pure (moved, new)

-- | Delete a sheet, and return the sheets that are left.  Refuses to delete
-- the last one: a workbook with nothing in it is not something the rest of the
-- program can show.
removeWorkbookSheet :: Workbook -> String -> IO [String]
removeWorkbookSheet workbook name = do
  names <- workbookSheetNames workbook
  unless (name `elem` names) $ refuse ("There is no sheet called " ++ name)
  when (length names <= 1) $ refuse "A workbook has to keep at least one sheet"
  active <- workbookActiveSheet workbook
  active' <- case filter (/= name) names of
    -- Cannot happen: the refusal above is what guarantees it, and saying so in
    -- a pattern rather than in a comment is most of why this is Haskell now.
    [] -> refuse "A workbook has to keep at least one sheet"
    remaining@(first : _) -> do
      removeSheetFiles (workbookSheetDirectory workbook name)
      let stillShowing = if active == Just name then Just first else active
      writeWorkbookIndex (workbookRoot workbook) remaining stillShowing
      pure remaining
  pure active'

-- | Delete the files a sheet is made of, and the folders that held them if
-- nothing else is left in them.
--
-- Only what Cellar wrote is deleted.  A sheet folder is a place a person can
-- keep a README, and a note left in one is a reason to leave the folder
-- standing -- empty of a sheet, and so no longer a tab -- rather than to take
-- the note down with it.
removeSheetFiles :: FilePath -> IO ()
removeSheetFiles directory = do
  let cells = cellsIn directory
  hasCells <- doesDirectoryExist cells
  when hasCells $ do
    names <- storedCellNames directory
    forM_ names $ \name -> removeFile (cellFilePath directory name)
    tryRemoveDirectory cells
  hasPrimary <- doesFileExist (directory </> primaryFile)
  when hasPrimary $ removeFile (directory </> primaryFile)
  tryRemoveDirectory directory

-- | Remove a directory if it is empty, and shrug if it is not.
tryRemoveDirectory :: FilePath -> IO ()
tryRemoveDirectory directory = do
  outcome <- try (removeDirectory directory) :: IO (Either SomeException ())
  pure (either (const ()) id outcome)

-- | A free name, or one with a different number after it.  What the Add Sheet
-- dialog suggests.
uniqueSheetName :: Workbook -> String -> IO String
uniqueSheetName workbook base' = do
  let base = if validSheetName base' then trim base' else defaultSheetName
  clash <- taken workbook base
  if not clash then pure base else do
    let (stem, n) = splitTrailingNumber base
    firstFree [ stem ++ show k | k <- [n + 1 ..] ]
  where
    firstFree [] = pure defaultSheetName
    firstFree (candidate : more) = do
      clash <- taken workbook candidate
      if clash then firstFree more else pure candidate

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
workbookFolderName :: String -> String
workbookFolderName name
  | ".cellar" `isSuffixOf` name = name
  | otherwise = name ++ ".cellar"
