{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tabs, and the folder they came out of.
--
-- Opening a workbook, building a tab for each sheet, writing edits back a cell
-- at a time, and noticing when something else has changed the folder.
--
module Cellar.App.Workbook where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName)
import System.Process (callProcess)

import Data.GI.Base
import qualified GI.Adw as Adw
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import Cellar.Grid
import Cellar.Ref
import Cellar.Sexp
import Cellar.Store
import Cellar.View
import Cellar.Watch
import Cellar.App.Types
import Cellar.App.Kernel

addTab :: App -> String -> IO Tab
addTab app name = do
  widget <- new Gtk.ColumnView
    [ #showRowSeparators := True
    , #showColumnSeparators := True
    , #reorderable := False ]
  Gtk.widgetAddCssClass widget "data-table"
  scroller <- new Gtk.ScrolledWindow [#hexpand := True, #vexpand := True]
  Gtk.scrolledWindowSetChild scroller (Just widget)
  page <- Adw.tabViewAppend (appTabView app) scroller
  Adw.tabPageSetTitle page (T.pack name)
  identifier <- atomicModifyIORef' (appNextId app) (\n -> (n + 1, n + 1))
  nameRef <- newIORef name
  sources <- newIORef []
  tabRef <- newIORef Nothing
  let withTab action = readIORef tabRef >>= mapM_ action
  grid <- newGrid widget (emptyView defaultRows defaultColumns) (appLineMenu app)
    (\r -> withTab $ \tab -> do
        current <- currentTab app
        when (fmap tabId current == Just (tabId tab)) (showSelection app r))
    (\r -> withTab $ \tab -> editCell app tab r)
    (\command -> withTab $ \tab -> onGridCommand app tab command)
  let tab = Tab nameRef identifier page grid sources
  writeIORef tabRef (Just tab)
  modifyIORef' (appTabs app) (++ [tab])
  loadTab app tab
  pure tab

-- | Read the sheet a tab stands for off the disk and hand it to the kernel.

loadTab :: App -> Tab -> IO ()
loadTab app tab = do
  directory <- tabDirectory app tab
  forM_ directory $ \path -> do
    isSheet <- isSheetDirectory path
    when isSheet $ do
      outcome <- try (readSheet path)
      case outcome :: Either SomeException Sheet of
        Left _ -> do
          name <- readIORef (tabName tab)
          notify app (T.pack ("Could not read " ++ name))
        Right sheet -> do
          writeIORef (tabSources tab) (sheetCells sheet)
          gridSetColumnWidths (tabGrid tab) (sheetWidths sheet)
          ask app "open"
            [ Num (fromIntegral (tabId tab))
            -- A sheet is at least the ordinary size on screen, even when it
            -- was saved smaller.
            , Num (fromIntegral (max defaultRows (sheetRows sheet)))
            , Num (fromIntegral (max defaultColumns (sheetColumns sheet)))
            , sourcesSexp (sheetCells sheet) ]
            (\payload -> do
               takeSnapshot app tab payload
               gridSetActive (tabGrid tab) (Ref 0 0))

-- | Take every tab off screen, and let the kernel forget their sheets.  This
-- says nothing about the disk -- it is what opening another workbook does, not
-- what deleting a sheet does.

closeAllTabs :: App -> IO ()
closeAllTabs app = do
  tabs <- readIORef (appTabs app)
  writeIORef (appTabs app) []
  forM_ tabs $ \tab -> do
    ask app "close" [Num (fromIntegral (tabId tab))] (const (pure ()))
    Adw.tabViewClosePage (appTabView app) (tabPage tab)


forgetTab :: App -> Tab -> IO ()
forgetTab app tab = do
  modifyIORef' (appTabs app) (filter (\other -> tabId other /= tabId tab))
  ask app "close" [Num (fromIntegral (tabId tab))] (const (pure ()))

-- | Make a tab for every sheet in the workbook and select one.

buildTabs :: App -> Maybe String -> IO ()
buildTabs app showing = do
  writeIORef (appLoading app) True
  closeAllTabs app
  workbook <- readIORef (appWorkbook app)
  forM_ workbook $ \path -> do
    names <- workbookSheetNames path
    forM_ names (addTab app)
    active <- workbookActiveSheet path
    let wanted = case showing of
          Just name | name `elem` names -> Just name
          _ -> case active of
            Just name | name `elem` names -> Just name
            _ -> case names of { (first : _) -> Just first; [] -> Nothing }
    forM_ wanted (selectTab app)
  writeIORef (appLoading app) False
  tab <- currentTab app
  forM_ tab $ \t -> do
    gridActiveRef (tabGrid t) >>= showSelection app
    gridFocus (tabGrid t)

-- Saving

-- | Put text into a cell: the kernel works out what it comes to, says what it
-- kept, and that is what goes into the cell's file.



persistLayout :: App -> Tab -> IO ()
persistLayout app tab = do
  directory <- tabDirectory app tab
  forM_ directory $ \path -> do
    view <- gridCurrentView (tabGrid tab)
    widths <- gridColumnWidths (tabGrid tab)
    sources <- readIORef (tabSources tab)
    reportFailure app "save the sheet" $
      saveSheet path (Sheet sources (viewRows view) (viewColumns view) widths)

-- | Write every cell of one sheet.  Moving a row or inserting a column renames
-- the files of every cell it shifted, so the cheapest correct answer for those
-- is to write the lot -- it is a few dozen small files, and it deletes the ones
-- left behind.

persistCells :: App -> Tab -> IO ()
persistCells = persistLayout

-- | Run something that touches the disk, and put a toast up if it fails.
--
-- A 'StoreError' carries a sentence written for a person to read, so that is
-- what is shown.  Anything else -- no permission, no space, a file that went
-- while we were looking at it -- does not, so it is reported as whatever was
-- being attempted.

onGridCommand :: App -> Tab -> Command -> IO ()
onGridCommand app tab command = case command of
  Layout -> persistLayout app tab
  Clear r -> setCell app tab r ""
  Move axis from to ->
    ask app "move"
      [ Num (fromIntegral (tabId tab)), Sym (axisName axis)
      , Num (fromIntegral from), Num (fromIntegral to) ]
      (\payload -> takeSnapshot app tab payload >> persistCells app tab)
  Insert axis at ->
    ask app "insert"
      [ Num (fromIntegral (tabId tab)), Sym (axisName axis)
      , Num (fromIntegral at) ]
      (\payload -> takeSnapshot app tab payload >> persistCells app tab)

rewatch :: App -> IO ()
rewatch app = do
  previous <- readIORef (appWatcher app)
  forM_ previous unwatch
  workbook <- readIORef (appWorkbook app)
  case workbook of
    Nothing -> do
      writeIORef (appWatching app) []
      writeIORef (appWatcher app) Nothing
    Just path -> do
      paths <- workbookWatchPaths path
      writeIORef (appWatching app) paths
      watcher <- watchPaths paths (reloadFromDisk app)
      writeIORef (appWatcher app) (Just watcher)

-- | Rebuild the watch when the workbook has gained or lost a folder.
--
-- A sheet arriving from outside is a folder that appears and is filled in a
-- moment later.  The folder appearing is what wakes us; by the time we look
-- there may be nothing in it yet, and if we did not take a watch out on it here
-- we would never hear about it being filled.

rewatchIfChanged :: App -> IO ()
rewatchIfChanged app = do
  workbook <- readIORef (appWorkbook app)
  forM_ workbook $ \path -> do
    wanted <- workbookWatchPaths path
    current <- readIORef (appWatching app)
    when (wanted /= current) (rewatch app)

-- | Something under the workbook folder changed; take the folder as the truth.
--
-- Cellar's own writes come through here too, and have to be harmless when they
-- do -- which they are, because by the time the file lands the shell already
-- has what the file says, and the comparisons below find nothing to do.

reloadFromDisk :: App -> IO ()
reloadFromDisk app = do
  workbook <- readIORef (appWorkbook app)
  forM_ workbook $ \path -> do
    stillThere <- isWorkbookDirectory path
    when stillThere $ do
      rewatchIfChanged app
      names <- workbookSheetNames path
      tabs <- readIORef (appTabs app)
      current <- mapM (readIORef . tabName) tabs
      if sameSet names current
        then do
          changed <- mapM (reloadTab app) tabs
          when (or changed) $ notify app "Reloaded — the workbook changed on disk"
        else do
          -- A sheet arrived or left -- somebody's commit, most likely.
          showing <- currentTab app >>= mapM (readIORef . tabName)
          buildTabs app showing
          rewatch app
          notify app "Reloaded — the sheets changed on disk"

reloadTab :: App -> Tab -> IO Bool
reloadTab app tab = do
  directory <- tabDirectory app tab
  case directory of
    Nothing -> pure False
    Just path -> do
      isSheet <- isSheetDirectory path
      if not isSheet then pure False else do
        onDisk <- readSheetCells path
        held <- readIORef (tabSources tab)
        if onDisk == sortByName held then pure False else do
          metadata <- readSheetMetadata path
          let rows = max defaultRows (fromMaybe 0 (lookupKey "rows" metadata >>= asInt))
              columns = max defaultColumns
                          (fromMaybe 0 (lookupKey "columns" metadata >>= asInt))
          active <- gridActiveRef (tabGrid tab)
          writeIORef (tabSources tab) onDisk
          ask app "open"
            [ Num (fromIntegral (tabId tab)), Num (fromIntegral rows)
            , Num (fromIntegral columns), sourcesSexp onDisk ]
            (\payload -> do
               takeSnapshot app tab payload
               view <- gridCurrentView (tabGrid tab)
               -- Keep the cursor where the user left it, unless the sheet
               -- shrank out from under it.
               gridSetActive (tabGrid tab)
                 (if viewHolds view active then active else Ref 0 0))
          pure True


openWorkbook :: App -> FilePath -> IO Bool
openWorkbook app path = do
  directory <- workbookDirectory path
  isWorkbook <- isWorkbookDirectory directory
  if not isWorkbook
    then do
      notify app (T.pack (takeFileName directory ++ " is not a Cellar workbook"))
      pure False
    else do
      outcome <- try $ do
        writeIORef (appWorkbook app) (Just directory)
        writeIORef (appScratch app) False
        buildTabs app Nothing
        rewatch app
        showSheetPage app
      case outcome :: Either SomeException () of
        Left _ -> do
          notify app (T.pack ("Could not open " ++ takeFileName directory))
          pure False
        Right () -> pure True

-- | Write the size of every sheet of a workbook Cellar has just made.  A sheet
-- folder is created empty -- nought by nought -- while the sheet in front of
-- you is 100 by 26, and this is what makes the file say what the window says.

persistFreshLayouts :: App -> IO ()
persistFreshLayouts app = readIORef (appTabs app) >>= mapM_ (persistLayout app)

-- Actions


stepSheet :: App -> Int -> IO ()
stepSheet app delta = do
  count <- Adw.tabViewGetNPages (appTabView app)
  page <- Adw.tabViewGetSelectedPage (appTabView app)
  forM_ page $ \p -> do
    position <- Adw.tabViewGetPagePosition (appTabView app) p
    let next = fromIntegral position + delta
    when (next >= 0 && next < fromIntegral count) $ do
      target <- Adw.tabViewGetNthPage (appTabView app) (fromIntegral next)
      Adw.tabViewSetSelectedPage (appTabView app) target

-- Dialogs


persistOrder :: App -> IO ()
persistOrder app = do
  loading <- readIORef (appLoading app)
  workbook <- readIORef (appWorkbook app)
  unless loading $ forM_ workbook $ \path -> do
    names <- tabOrder app
    reportFailure app "save the order of the sheets" (setWorkbookOrder path names)


onTabSelected :: App -> IO ()
onTabSelected app = do
  tab <- currentTab app
  forM_ tab $ \t -> do
    gridActiveRef (tabGrid t) >>= showSelection app
    loading <- readIORef (appLoading app)
    workbook <- readIORef (appWorkbook app)
    unless loading $ forM_ workbook $ \path -> do
      name <- readIORef (tabName t)
      reportFailure app "save the workbook" (setWorkbookActive path name)

-- | A tab's close button, or Delete Sheet.  A tab is a sheet of the workbook
-- rather than a view of one, so closing it is deleting it -- which is worth
-- being asked about, and worth refusing when it would leave the workbook with
-- nothing in it.


deleteSheet :: App -> Tab -> IO Bool
deleteSheet app tab = do
  workbook <- readIORef (appWorkbook app)
  case workbook of
    Nothing -> pure False
    Just path -> do
      name <- readIORef (tabName tab)
      outcome <- try (removeWorkbookSheet path name)
      case outcome :: Either StoreError [String] of
        Left (StoreError why) -> notify app (T.pack why) >> pure False
        Right _ -> do
          forgetTab app tab
          rewatch app
          notify app (T.pack ("Deleted " ++ name))
          pure True


addSheet :: App -> String -> IO ()
addSheet app name = do
  workbook <- readIORef (appWorkbook app)
  forM_ workbook $ \path -> do
    legacy <- isFormatOne path
    outcome <- try (addWorkbookSheet path name)
    case outcome :: Either StoreError String of
      Left (StoreError why) -> notify app (T.pack why)
      Right added -> do
        -- A workbook written before there were tabs is moved into sheets/ by
        -- this, which changes where its one sheet is written but nothing about
        -- what it says.  The tabs are rebuilt rather than added to, so that the
        -- sheet that moved is reopened from where it now lives.
        if legacy
          then do
            buildTabs app (Just added)
            found <- tabNamed app added
            forM_ found (persistLayout app)
          else do
            tab <- addTab app added
            selectTab app added
            persistLayout app tab
        rewatch app
        notify app (T.pack ("Added " ++ added))


renameSheet :: App -> Tab -> String -> IO ()
renameSheet app tab name = do
  workbook <- readIORef (appWorkbook app)
  forM_ workbook $ \path -> do
    old <- readIORef (tabName tab)
    outcome <- try (renameWorkbookSheet path old name)
    case outcome :: Either StoreError String of
      Left (StoreError why) -> notify app (T.pack why)
      Right renamed -> do
        writeIORef (tabName tab) renamed
        Adw.tabPageSetTitle (tabPage tab) (T.pack renamed)
        rewatch app
        retitle app

-- | A workbook to think in.  It still lives in a folder -- everything does --
-- but one Cellar picks, out of the way under the data directory, so that
-- starting one asks nothing.  Copy To puts it somewhere you chose.

scratchWorkbook :: App -> IO ()
scratchWorkbook app = do
  directory <- scratchLocation
  outcome <- try (createWorkbook directory firstSheetName)
  case outcome :: Either SomeException () of
    Left _ -> notify app "Could not make a scratch workbook"
    Right () -> do
      opened <- openWorkbook app directory
      when opened $ do
        writeIORef (appScratch app) True
        persistFreshLayouts app
        retitle app


scratchLocation :: IO FilePath
scratchLocation = do
  home <- fromMaybe "." <$> lookupEnv "HOME"
  xdg <- lookupEnv "XDG_DATA_HOME"
  let base = fromMaybe (home </> ".local" </> "share") xdg
      scratches = base </> "cellar" </> "scratch"
  createDirectoryIfMissing True scratches
  -- Named for the moment it was started, so two of them never collide.
  stamp <- timestamp
  let candidates = (scratches </> stamp ++ ".cellar")
                 : [ scratches </> (stamp ++ "-" ++ show n) ++ ".cellar"
                   | n <- [1 :: Int ..] ]
  firstFree candidates
  where
    firstFree [] = pure "."
    firstFree (candidate : more) = do
      taken <- doesPathExist candidate
      if taken then firstFree more else pure candidate


timestamp :: IO String
timestamp = do
  -- GLib is already here and already knows how to do this; pulling in another
  -- way of asking what time it is would be for the sake of it.
  now <- GLib.dateTimeNewNowLocal
  case now of
    Nothing -> pure "sheet"
    Just moment -> do
      formatted <- GLib.dateTimeFormat moment "%Y-%m-%d-%H%M%S"
      pure (maybe "sheet" T.unpack formatted)

copyTo :: App -> FilePath -> Bool -> IO ()
copyTo app directory wantsGit = do
  tabs <- orderedTabs app
  case tabs of
    [] -> notify app "There is nothing to copy"
    (first : _) -> do
      firstName <- readIORef (tabName first)
      outcome <- try $ do
        createWorkbook directory firstName
        forM_ tabs $ \tab -> do
          name <- readIORef (tabName tab)
          unless (name == firstName) (void (addWorkbookSheet directory name))
        forM_ tabs $ \tab -> do
          name <- readIORef (tabName tab)
          folder <- workbookSheetDirectory directory name
          view <- gridCurrentView (tabGrid tab)
          widths <- gridColumnWidths (tabGrid tab)
          sources <- readIORef (tabSources tab)
          saveSheet folder (Sheet sources (viewRows view) (viewColumns view) widths)
        names <- mapM (readIORef . tabName) tabs
        showing <- currentTab app >>= mapM (readIORef . tabName)
        writeWorkbookIndex directory names showing
      case outcome :: Either SomeException () of
        Left _ -> notify app (T.pack ("Could not copy to " ++ takeFileName directory))
        Right () -> do
          when wantsGit (gitInit app directory)
          writeIORef (appWorkbook app) (Just directory)
          writeIORef (appScratch app) False
          rewatch app
          retitle app
          notify app (T.pack ("Now editing " ++ workbookName directory))

-- | Make a git repository of the workbook.  Around the workbook rather than
-- around any one sheet, which is the whole reason a workbook exists.  A
-- workbook without a repository is still a workbook, so failing is not fatal.

gitInit :: App -> FilePath -> IO ()
gitInit app directory = do
  outcome <- try (callProcess "git" ["init", "--quiet", directory])
  case outcome :: Either SomeException () of
    Left _ -> notify app "The folder was made, but git could not be run"
    Right () -> pure ()

-- | Ask for a folder.  A workbook is a folder, so this is the one chooser the
-- application needs: opening one picks the folder, and making one picks the
-- folder to make it in.
