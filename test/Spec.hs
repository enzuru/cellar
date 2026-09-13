-- | Tests for the Haskell shell.
--
-- The pure parts are checked in this process.  The client is checked against
-- the real Guile kernel over a real pipe, because that is the part where being
-- wrong is not a type error: two languages have to agree about a wire format,
-- and the only way to know they do is to make them talk.
--
-- Nothing here needs a display.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import qualified Data.ByteString as B
import Data.IORef
import Data.List (isInfixOf, nub, sort)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.Text as T
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

import qualified GI.Gdk as Gdk

import Cellar.Client
import Cellar.Config
import Cellar.App.State
import Cellar.External
import Cellar.Grid.Model
import Cellar.Protocol
import Cellar.Ref
import Cellar.Sexp
import Cellar.Store
import Cellar.View

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  section "references"
  check failures "a column is letters" "A" (columnName 0)
  check failures "and carries" "AA" (columnName 26)
  check failures "a reference is written the usual way" "D6" (refName (Ref 5 3))
  check failures "and read back" (Just (Ref 5 3)) (parseRef "D6")
  check failures "a wide one too" (Just (Ref 29 26)) (parseRef "AA30")
  check failures "nonsense is not a reference" Nothing (parseRef "zzz")
  check failures "nor is a bare number" Nothing (parseRef "12")
  check failures "nor a row of nought" Nothing (parseRef "A0")
  check failures "every reference round trips"
    [] [ name | row <- [0 .. 40], column <- [0 .. 40]
       , let name = refName (Ref row column)
       , parseRef name /= Just (Ref row column) ]
  check failures "a moved row takes its cells with it"
    (Ref 0 3) (refAfterMove (Ref 2 3) Row 2 0)
  check failures "and the ones it passes slide over"
    (Ref 1 3) (refAfterMove (Ref 0 3) Row 2 0)
  check failures "an insert pushes what is below it down"
    (Ref 3 1) (refAfterInsert (Ref 2 1) Row 2)
  check failures "and leaves what is above alone"
    (Ref 1 1) (refAfterInsert (Ref 1 1) Row 2)
  -- A move and the move back are one permutation and its inverse, so together
  -- they are nothing at all.  The twin of this case is in tests/ref-test.scm,
  -- which checks the Guile copy of the same arithmetic: two implementations of
  -- one rule is two chances to be wrong, and they disagree quietly.
  check failures "a move and the move back leave every index where it started"
    [] [ (i, from, to)
       | i <- [0 .. 5], from <- [0 .. 5], to <- [0 .. 5]
       , shiftIndex (shiftIndex i from to) to from /= i ]

  section "s-expressions"
  let roundTrip s = parseSexp (writeSexp s)
  forM_ sampleSexps $ \s ->
    check failures ("round trip: " ++ T.unpack (T.take 48 (writeSexp s)))
      (Right s) (roundTrip s)
  check failures "a dotted pair is a dotted pair"
    (Right (Pair (Sym "rows") (Num 100))) (parseSexp (T.pack "(rows . 100)"))
  check failures "a proper list is not"
    (Right (list [Sym "a", Sym "b"])) (parseSexp (T.pack "(a b)"))
  check failures "the empty list" (Right Nil) (parseSexp (T.pack "()"))
  check failures "booleans" (Right (list [Bool True, Bool False])) (parseSexp (T.pack "(#t #f)"))
  check failures "a negative number" (Right (Num (-7))) (parseSexp (T.pack "-7"))
  check failures "comments and whitespace are skipped"
    (Right (Sym "a")) (parseSexp (T.pack "  ; a note\n  a  "))
  check failures "Guile's string escapes"
    (Right (Str "a \"quote\", a \\ and a\nnewline"))
    (parseSexp (T.pack "\"a \\\"quote\\\", a \\\\ and a\\nnewline\""))
  check failures "a hex escape" (Right (Str "\a")) (parseSexp (T.pack "\"\\x7;\""))
  check failures "an alist reads as one"
    [("rows", Num 100), ("columns", Num 26)]
    (alist (list [Pair (Sym "rows") (Num 100), Pair (Sym "columns") (Num 26)]))
  check failures "an unterminated string is refused" True
    (either (const True) (const False) (parseSexp (T.pack "\"never ends")))
  check failures "and so is a stray close paren" True
    (either (const True) (const False) (parseSexp (T.pack ")")))
  -- The rest of the ways a message can be malformed.  The reader is written to
  -- fail loudly rather than to be generous, and what it says when it fails is
  -- what somebody reads when the two halves disagree about the wire format, so
  -- the message is checked and not merely the refusal.
  let refused wanted = either (isInfixOf wanted) (const False)
  check failures "a hex escape with no semicolon says so" True
    (refused "semicolon" (parseSexp (T.pack "\"\\x41\"")))
  check failures "a string ending in a backslash says so" True
    (refused "backslash" (parseSexp (T.pack "\"a\\")))
  check failures "a dotted pair with two things after the dot" True
    (refused "dotted pair" (parseSexp (T.pack "(a . b c)")))
  check failures "a second datum after the first" True
    (refused "trailing text" (parseSexp (T.pack "1 2")))
  check failures "a # syntax it has never heard of" True
    (refused "# syntax" (parseSexp (T.pack "#z")))
  check failures "and a message that stops in the middle" True
    (refused "stopped in the middle" (parseSexp (T.pack "")))
  -- Escapes Guile writes that nothing else in the suite happens to send.
  check failures "the awkward escapes"
    (Right (Str "\b\v\f\0")) (parseSexp (T.pack "\"\\b\\v\\f\\0\""))
  check failures "an escape it does not know keeps the character"
    (Right (Str "q")) (parseSexp (T.pack "\"\\q\""))
  check failures "a backslash before a newline joins the lines"
    (Right (Str "ab")) (parseSexp (T.pack "\"a\\\n   b\""))
  check failures "a fraction is read as one"
    (Right (Real 1.5)) (parseSexp (T.pack "1.5"))
  check failures "and written back the same way"
    (Right (Real 1.5)) (parseSexp (writeSexp (Real 1.5)))
  check failures "a fraction truncates where a whole number is wanted"
    (Just 2) (either (const Nothing) asInt (parseSexp (T.pack "2.9")))
  check failures "a symbol comes out as its name" (Just "sheet") (asSymbol (Sym "sheet"))
  check failures "and a string does not" Nothing (asSymbol (Str "sheet"))
  -- Scheme's notion of truth, which the kernel's replies are written in.
  check failures "only #f is false" False (asBool (Bool False))
  check failures "a number is true" True (asBool (Num 0))
  check failures "a dotted list is not a list" Nothing
    (toList (Pair (Sym "a") (Sym "b")))

  section "protocol framing"
  let messages =
        [ Reply 1 (list [Pair (Sym "sheet") (Str "Summary")])
        , Failed 2 "no sheet called \"Q1\" is open"
        ]
      wire = B.concat
        [ encode (list [Sym "reply", Num 1,
                        list [Pair (Sym "sheet") (Str "Summary")]])
        , encode (list [Sym "fail", Num 2, Str "no sheet called \"Q1\" is open"])
        ]
  check failures "a whole buffer decodes" messages (snd (feed newDecoder wire))
  check failures "and so does a byte at a time"
    messages
    (snd (foldl (\(d, acc) byte ->
                   let (d', got) = feed d (B.singleton byte)
                   in (d', acc ++ got))
                (newDecoder, [])
                (B.unpack wire)))
  let one = encode (list [Sym "reply", Num 7, Nil])
  check failures "half a message is not a message yet"
    []
    (snd (feed newDecoder (B.take (B.length one - 3) one)))
  check failures "and completes when the rest turns up"
    [Reply 7 Nil]
    (let (partial, none) = feed newDecoder (B.take (B.length one - 3) one)
         (_, got) = feed partial (B.drop (B.length one - 3) one)
     in none ++ got)

  section "configuration"
  check failures "a plain command" ["vim"] (splitCommand "vim")
  check failures "with arguments" ["xterm", "-e", "vim"] (splitCommand "xterm -e vim")
  check failures "quoting groups" ["my editor", "x"] (splitCommand "'my editor' x")
  check failures "a path is appended when there is no %s"
    ["gnome-text-editor", "/tmp/A1.scm"]
    (editorArgv "gnome-text-editor" "/tmp/A1.scm")
  check failures "and substituted when there is"
    ["xterm", "-e", "vim /tmp/A1.scm"]
    (editorArgv "xterm -e 'vim %s'" "/tmp/A1.scm")

  section "views"
  let snapshot = list
        [ Pair (Sym "sheet") (Str "S")
        , Pair (Sym "rows") (Num 100)
        , Pair (Sym "columns") (Num 26)
        , Pair (Sym "cells") (list
            [ list [Str "A1", Str "Qty", Bool False, Bool False, Bool False, Bool False]
            , list [Str "B2", Str "42", Bool True, Bool False, Str "#fff3b0", Bool False]
            , list [Str "C3", Str "#ERR", Bool False, Bool False, Bool False,
                    Str "Division by zero"]
            ])
        ]
      view = viewFromSnapshot snapshot [("A1", "\"Qty\""), ("B2", "(* 6 7)")]
  check failures "a view is the size the snapshot said" 100 (viewRows view)
  check failures "a cell's text comes through" "Qty" (displayAt view (Ref 0 0))
  check failures "a number is marked as one" True (numberAt view (Ref 1 1))
  check failures "and a string is not" False (numberAt view (Ref 0 0))
  check failures "a background travels" (Nothing, Just "#fff3b0") (styleAt view (Ref 1 1))
  check failures "an error carries its message"
    (Just "Division by zero") (errorAt view (Ref 2 2))
  check failures "and a cell that is fine carries none" Nothing (errorAt view (Ref 0 0))
  check failures "the shell's own source is laid alongside"
    (Just "(* 6 7)") (sourceAt view (Ref 1 1))
  check failures "an empty cell shows nothing" "" (displayAt view (Ref 9 9))
  check failures "a reference off the edge is not held" False (viewHolds view (Ref 200 0))

  -- The grid, as a value.  What a key or a click makes of it, what it asks for
  -- when it does, and what a column carries with it when it moves -- all of it
  -- without a widget, because the model half of the grid knows nothing about
  -- one.  What is left needing a display is the drawing, which
  -- `make check-window' and the smoke scripts cover.
  section "the grid"
  let sheet = emptyView 10 4
      start = newGridModel sheet
  check failures "a new grid is the size of its view"
    (10, 4) (modelRows start, length (modelColumns start))
  check failures "and starts in the corner" (Ref 0 0) (modelActive start)
  check failures "a view only ever grows it"
    10 (modelRows (withView (emptyView 4 4) start))
  check failures "a taller view grows it"
    20 (modelRows (withView (emptyView 20 4) start))

  let pressKey key model = gridEvent (KeyDown key) model
  check failures "Down moves the active cell down"
    (Ref 1 0) (modelActive (fst (pressKey Gdk.KEY_Down start)))
  check failures "and the top row is as far up as it goes"
    (Ref 0 0) (modelActive (fst (pressKey Gdk.KEY_Up start)))
  check failures "End goes to the last column"
    (Ref 0 3) (modelActive (fst (pressKey Gdk.KEY_End start)))
  check failures "Delete asks for the active cell to be cleared"
    [Ask (Clear (Ref 0 0))] (snd (pressKey Gdk.KEY_Delete start))
  check failures "Enter opens the editor on it"
    [Open (Ref 0 0)] (snd (pressKey Gdk.KEY_Return start))
  check failures "a key the grid does not answer changes nothing"
    [] (snd (pressKey Gdk.KEY_F1 start))
  check failures "and the grid says which keys it answers"
    (True, False) (handledKey Gdk.KEY_Down, handledKey Gdk.KEY_F1)

  check failures "a click selects the cell it landed on"
    (Ref 2 1) (modelActive (fst (gridEvent (Pressed (Ref 2 1) 1) start)))
  check failures "and asks for nothing"
    [] (snd (gridEvent (Pressed (Ref 2 1) 1) start))
  check failures "a second click opens the editor"
    [Open (Ref 2 1)] (snd (gridEvent (Pressed (Ref 2 1) 2) start))
  check failures "a click past the edge of the sheet selects nothing"
    (Ref 0 0) (modelActive (fst (gridEvent (Pressed (Ref 99 1) 1) start)))

  let widened = withWidths [(1, 200)] start
  check failures "a column width is remembered by position"
    [(1, 200)] (columnWidths widened)
  check failures "a column cannot be moved off the sheet"
    Nothing (fmap snd (moveLine Column 3 9 start))
  case moveLine Column 1 3 widened of
    Nothing -> check failures "a column moves" True False
    Just (moved, command) -> do
      check failures "moving a column asks for the move"
        (Move Column 1 3) command
      -- The point of a column having a name of its own: what was column B is
      -- column D now, and it is the same column, so it is still that wide.
      check failures "and its width goes with it" [(3, 200)] (columnWidths moved)
  case insertLine Column 1 start of
    Nothing -> check failures "a column is inserted" True False
    Just (grown, command) -> do
      check failures "inserting a column asks for the insert"
        (Insert Column 1) command
      check failures "and the sheet is one column wider"
        5 (length (modelColumns grown))
      -- The new column takes the position; every other column keeps the name
      -- it had, which is what its width and its widget hang on.
      check failures "the new column goes in under a name of its own"
        (map ColumnId [0, 4, 1, 2, 3]) (modelColumns grown)
      check failures "and no two columns share a name"
        5 (length (nub (modelColumns grown)))
  case insertLine Row 0 (fst (pressKey Gdk.KEY_Down start)) of
    Nothing -> check failures "a row is inserted" True False
    Just (grown, _) -> do
      check failures "inserting a row makes the sheet taller" 11 (modelRows grown)
      check failures "and carries the active cell down" (Ref 2 0) (modelActive grown)

  -- More of the grid: the parts a person reaches with a right-click, a drag
  -- or a resize, which the smoke scripts drive through real widgets and which
  -- are worth pinning down here as well, because here they are arithmetic.
  section "the grid, further in"
  check failures "picking a line moves the active cell along it"
    (Just (Ref 7 0)) (modelActive <$> selectLine Row 7 start)
  check failures "and along the other one"
    (Just (Ref 0 2)) (modelActive <$> selectLine Column 2 start)
  check failures "a line off the sheet is not picked"
    Nothing (modelActive <$> selectLine Row 99 start)
  check failures "scrolling asks for a row"
    (Just 4) (modelScroll (scrollTo 4 start))
  check failures "a drag is remembered as it is drawn"
    (Just (Column, 1, 3)) (modelDrag (withDrag (Just (Column, 1, 3)) start))
  check failures "and forgotten when it ends"
    Nothing (modelDrag (withDrag Nothing (withDrag (Just (Row, 0, 1)) start)))
  check failures "a column reports where it is"
    (Just 2) (positionOfColumn (ColumnId 2) start)
  check failures "and a column that is not there reports nothing"
    Nothing (positionOfColumn (ColumnId 99) start)
  check failures "a resize is written down and asks for the layout to be saved"
    (Just 180, [Ask Layout])
    (let (resized, out) = gridEvent (Resized (ColumnId 2) 180) start
     in (lookup (ColumnId 2) [ (c, w) | (p, w) <- columnWidths resized
                   , Just c <- [lookup p (zip [0 ..] (modelColumns resized))] ], out))
  check failures "the default width is not worth writing down"
    [] (columnWidths (fst (gridEvent (Resized (ColumnId 0) 104) start)))
  check failures "Tab moves right and Shift+Tab moves back"
    (Ref 0 1, Ref 0 0)
    ( modelActive (fst (pressKey Gdk.KEY_Tab start))
    , modelActive (fst (pressKey Gdk.KEY_ISO_Left_Tab
                         (fst (pressKey Gdk.KEY_Tab start)))) )
  check failures "Page Down goes ten rows at a time, and stops at the end"
    (Ref 9 0) (modelActive (fst (pressKey Gdk.KEY_Page_Down start)))
  check failures "a row cannot be moved off the sheet"
    Nothing (snd <$> moveLine Row 0 (-1) start)
  check failures "a row insert past the end is refused"
    Nothing (snd <$> insertLine Row 99 start)
  check failures "a view with more columns brings them with it"
    6 (length (modelColumns (withView (emptyView 10 6) start)))
  check failures "and they are named after the ones already there"
    (map ColumnId [0, 1, 2, 3, 4, 5]) (modelColumns (withView (emptyView 10 6) start))

  -- A cell can ask to be drawn in any colour it likes, and GTK has no way to
  -- set one on a widget except through the stylesheet, so each pair of colours
  -- becomes a class of its own and the classes are collected into a sheet.
  let coloured = View 10 4 (M.fromList
        [ ("A1", Cell "1" True (Just "#ff0000") Nothing Nothing Nothing)
        , ("B1", Cell "2" True (Just "#ff0000") Nothing Nothing Nothing)
        , ("C1", Cell "3" True Nothing (Just "#00ff00") Nothing Nothing)
        , ("D1", Cell "4" True Nothing Nothing Nothing Nothing) ])
      -- The window learns the colours and hands them down, because the names
      -- go into one stylesheet and two sheets counting from zero would mean
      -- two colours under one name.
      colours = paletteFor coloured M.empty
      painted = withPalette colours (withView coloured start)
  check failures "a colour a cell asks for becomes a class" 2 (M.size colours)
  check failures "and two cells asking for the same one share it"
    1 (length (nub [ name | ((c, _), name) <- M.toList colours, c == Just "#ff0000" ]))
  check failures "the sheet it is drawn on is given the palette"
    2 (M.size (modelPalette painted))
  check failures "the stylesheet says what each class is"
    True (isInfixOf "color: #ff0000" (T.unpack (paletteCss colours)))
  check failures "and a background as well"
    True (isInfixOf "background-color: #00ff00" (T.unpack (paletteCss colours)))
  check failures "a palette that has seen a colour does not learn it twice"
    2 (M.size (paletteFor coloured colours))
  check failures "and a sheet with no colours in it adds none"
    0 (M.size (paletteFor (emptyView 10 4) M.empty))

  -- The window's own state: the sheets it holds, which one is showing, and
  -- what the kernel owes an answer for.
  section "the window's state"
  let blank = newState defaultConfig "/home/nobody"
      (oneSheet, first') = addTab "Summary" (emptyView 10 4) blank
      (twoSheets, second') = addTab "Q1" (emptyView 10 4) oneSheet
      showing = selectTab (tabId first') twoSheets
  check failures "a new window has no workbook and no sheets"
    (False, []) (isJust (stateWorkbook blank), tabOrder blank)
  check failures "and opens on the start page" False (sheetShowing blank)
  check failures "with nothing to say in the title"
    "No workbook open" (T.unpack (subtitleOf blank))
  check failures "a scratch workbook says so instead"
    "Scratch" (T.unpack (subtitleOf blank { stateScratch = True }))
  check failures "sheets are added in order" ["Summary", "Q1"] (tabOrder twoSheets)
  check failures "and each gets a name of its own"
    True (tabId first' /= tabId second')
  check failures "the one selected is the one asked for"
    (Just "Summary") (tabName <$> currentTab showing)
  check failures "a sheet can be found by name"
    (Just (tabId second')) (tabId <$> tabNamed "Q1" showing)
  check failures "and says where it is" (Just 1) (tabPosition (tabId second') showing)
  check failures "a sheet that is not there is not found"
    Nothing (tabName <$> tabNamed "nowhere" showing)
  check failures "reordering the tabs reorders the sheets"
    ["Q1", "Summary"] (tabOrder (orderTabs [tabId second', tabId first'] showing))
  check failures "a sheet that the order does not name keeps its place"
    ["Q1", "Summary"] (tabOrder (orderTabs [tabId second'] showing))
  -- Closing the sheet that is showing has to leave something showing.
  check failures "forgetting the sheet on screen shows another"
    (Just "Q1") (tabName <$> currentTab (forgetTab (tabId first') showing))
  check failures "and forgetting another leaves the one on screen alone"
    (Just "Summary") (tabName <$> currentTab (forgetTab (tabId second') showing))
  check failures "a sheet's cells can be written into it"
    (Just [("A1", "1")])
    (tabSources <$> tabById (tabId first')
       (withTab (tabId first') (\t -> t { tabSources = [("A1", "1")] }) showing))
  check failures "or into whichever is showing"
    (Just [("B2", "2")])
    (tabSources <$> currentTab
       (withCurrentTab (\t -> t { tabSources = [("B2", "2")] }) showing))
  check failures "what a cell says is read from the sheet showing"
    (Just "1") (sourceOf (Ref 0 0)
       (withCurrentTab (\t -> t { tabSources = [("A1", "1")] }) showing))

  section "the store"
  root <- makeTemporaryDirectory
  let inRoot = (root </>)

  do let path = inRoot "round.cellar"
         cells = [ ("A1", "\"Qty\""), ("A2", "7")
                 , ("C1", "(sum (range 'A2 'A3))")
                 , ("C3", "(if (> A2 5)\n    'over\n    'under)") ]
     createSheetDirectory path
     saveSheet path (Sheet cells 8 3 [(0, 104), (2, 180)])
     back <- readSheet path
     check failures "the cells come back" cells (sort (sheetCells back))
     check failures "the size with them" (8, 3) (sheetRows back, sheetColumns back)
     check failures "and the column widths" [(0, 104), (2, 180)] (sort (sheetWidths back))
     hasB1 <- doesFileExist (cellFilePath path "B1")
     check failures "an empty cell has no file" False hasB1
     -- Which is why opening a cell in another program has to make one: a
     -- path that is not there cannot be handed to anything.
     made <- touchCell path "B1"
     check failures "so opening one makes it" (cellFilePath path "B1") made
     blank <- readFile made
     check failures "empty, as the cell is" "" blank
     kept <- touchCell path "A2"
     held <- readFile kept
     check failures "while a cell that has a file keeps what is in it" "7\n" held

  do let path = inRoot "mine.cellar"
     createSheetDirectory path
     saveSheet path (Sheet [("A1", "1")] 4 2 [])
     writeFile (path </> "README.md") "mine\n"
     writeFile (path </> "cells" </> "helpers.scm") "(define (double x) x)\n"
     saveSheet path (Sheet [("B2", "9")] 4 2 [])
     readme <- doesFileExist (path </> "README.md")
     helpers <- doesFileExist (path </> "cells" </> "helpers.scm")
     gone <- doesFileExist (cellFilePath path "A1")
     check failures "a README is left alone" True readme
     check failures "so is a stray .scm that is not a cell" True helpers
     check failures "while the cleared cell did go" False gone

  do let path = inRoot "book.cellar"
     createWorkbook path "Summary"
     book <- open path
     check failures "a new workbook is of the current shape" False (isFormatOne book)
     names <- workbookSheetNames book
     check failures "with the sheet it was given" ["Summary"] names
     active <- workbookActiveSheet book
     check failures "which is the one showing" (Just "Summary") active
     check failures "and its folder is worked out without asking the disk"
       (path </> "sheets" </> "Summary") (workbookSheetDirectory book "Summary")
     (book1, _) <- addWorkbookSheet book "Q1"
     (book2, _) <- addWorkbookSheet book1 "Q2"
     ordered <- workbookSheetNames book2
     check failures "added sheets keep their order" ["Summary", "Q1", "Q2"] ordered
     clash <- try (addWorkbookSheet book2 "q1")
                :: IO (Either StoreError (Workbook, String))
     check failures "a name differing only in case is refused" True (isLeft clash)
     slash <- try (addWorkbookSheet book2 "a/b")
                :: IO (Either StoreError (Workbook, String))
     check failures "and so is one a folder cannot have" True (isLeft slash)
     suggestion <- uniqueSheetName book2 "Q1"
     check failures "a suggested name counts on from the last" "Q3" suggestion
     summary <- uniqueSheetName book2 "Summary"
     check failures "or gains a number when there was none" "Summary 2" summary
     (book3, _) <- renameWorkbookSheet book2 "Q1" "First Quarter"
     renamed <- workbookSheetNames book3
     check failures "renaming keeps the order"
       ["Summary", "First Quarter", "Q2"] renamed
     setWorkbookOrder book3 ["Q2", "Summary", "First Quarter"]
     reordered <- workbookSheetNames book3
     check failures "and the order can be set"
       ["Q2", "Summary", "First Quarter"] reordered
     remaining <- removeWorkbookSheet book3 "Q2"
     check failures "removing answers with what is left"
       ["Summary", "First Quarter"] remaining
     _ <- removeWorkbookSheet book3 "First Quarter"
     lastOne <- try (removeWorkbookSheet book3 "Summary")
                  :: IO (Either StoreError [String])
     check failures "the last sheet cannot be removed" True (isLeft lastOne)

  do let path = inRoot "keepsake.cellar"
     createWorkbook path "Summary"
     book <- open path
     (book', _) <- addWorkbookSheet book "Q1"
     let folder = workbookSheetDirectory book' "Q1"
     writeFile (folder </> "README") "mine\n"
     _ <- removeWorkbookSheet book' "Q1"
     note <- doesFileExist (folder </> "README")
     names <- workbookSheetNames book'
     check failures "a note keeps its folder standing" True note
     check failures "but the sheet is no longer a tab" ["Summary"] names

  do let path = inRoot "hint.cellar"
     createWorkbook path "Summary"
     book <- open path
     (book', _) <- addWorkbookSheet book "Q1"
     -- As if someone else's commit had brought a sheet in and taken one away.
     createSheetDirectory (path </> "sheets" </> "Arrived")
     removeDirectoryRecursive (path </> "sheets" </> "Q1")
     names <- workbookSheetNames book'
     check failures "a sheet the index never heard of turns up"
       True ("Arrived" `elem` names)
     check failures "one whose folder went is dropped" False ("Q1" `elem` names)

  do let path = inRoot "old.cellar"
     createSheetDirectory path
     saveSheet path (Sheet [("A1", "\"first\""), ("B2", "(* 6 7)")] 12 4 [(0, 120)])
     isWorkbook <- isWorkbookDirectory path
     book <- open path
     names <- workbookSheetNames book
     indexed <- doesFileExist (path </> "workbook.scm")
     check failures "a workbook from before tabs is still a workbook" True isWorkbook
     check failures "of the older shape, and the type says which"
       (SingleSheet "old") (workbookLayout book)
     check failures "with one sheet, named for the folder" ["old"] names
     check failures "living where it always did"
       path (workbookSheetDirectory book "old")
     check failures "and nothing written to say so" False indexed
     -- A second sheet is what moves it, and not before.
     (book', _) <- addWorkbookSheet book "Q1"
     check failures "adding a sheet changes the shape, and says so"
       SheetsUnder (workbookLayout book')
     moved <- doesFileExist (path </> "sheets" </> "old" </> "sheet.scm")
     cleared <- doesFileExist (path </> "sheet.scm")
     nowIndexed <- doesFileExist (path </> "workbook.scm")
     migrated <- readSheet (path </> "sheets" </> "old")
     both <- workbookSheetNames book'
     check failures "adding a sheet moves the old one under sheets/" True moved
     check failures "the top of the workbook is clear" False cleared
     check failures "there is an index now" True nowIndexed
     check failures "the cells came with it"
       (Just "(* 6 7)") (lookup "B2" (sheetCells migrated))
     check failures "and the column widths" [(0, 120)] (sheetWidths migrated)
     check failures "naming both sheets" ["old", "Q1"] both

  do let path = inRoot "finding.cellar"
     createWorkbook path "Summary"
     fromIndex <- resolveWorkbook (path </> "workbook.scm")
     fromSheet <- resolveWorkbook (path </> "sheets" </> "Summary" </> "sheet.scm")
     nothing <- resolveWorkbook (inRoot "not-a-workbook")
     check failures "a workbook is found from its index"
       (Just path) (workbookRoot <$> fromIndex)
     check failures "and from a sheet inside it"
       (Just path) (workbookRoot <$> fromSheet)
     check failures "and a folder that is not one is not found"
       Nothing (workbookRoot <$> nothing)

  do let path = inRoot "watched.cellar"
     createWorkbook path "Summary"
     book <- open path
     (book', _) <- addWorkbookSheet book "Q1"
     let summary = workbookSheetDirectory book' "Summary"
     -- One cell at a time is how an edit reaches the disk; the whole-sheet
     -- write above is only for a workbook being copied.
     saveCell summary "A1" (Just "(* 6 7)")
     written <- readSheetCells summary
     check failures "a cell written on its own is on disk"
       (Just "(* 6 7)") (lookup "A1" written)
     saveCell summary "A1" (Just "   ")
     blanked <- doesFileExist (cellFilePath summary "A1")
     check failures "a cell cleared to whitespace takes its file with it" False blanked
     saveCell summary "A2" Nothing
     never <- doesFileExist (cellFilePath summary "A2")
     check failures "and so does one cleared outright" False never

     setWorkbookActive book' "Q1"
     active <- workbookActiveSheet book'
     order <- workbookSheetNames book'
     check failures "the sheet showing is written down" (Just "Q1") active
     check failures "and the order is left as it was" ["Summary", "Q1"] order

     -- What the watcher is pointed at.  A workbook changed by a commit or a
     -- text editor is noticed through these and nothing else.
     watched <- workbookWatchPaths book'
     check failures "the watch covers the index"
       True ((path </> "workbook.scm") `elem` watched)
     check failures "the folder the sheets are under"
       True ((path </> "sheets") `elem` watched)
     check failures "each sheet's own folder"
       True ((path </> "sheets" </> "Q1") `elem` watched)
     check failures "the cells inside it"
       True ((path </> "sheets" </> "Q1" </> "cells") `elem` watched)
     check failures "and the primary file that says how big it is"
       True ((path </> "sheets" </> "Q1" </> "sheet.scm") `elem` watched)

  check failures "a workbook folder is named .cellar"
    "budget.cellar" (workbookFolderName "budget")
  check failures "and one that says so already is left alone"
    "budget.cellar" (workbookFolderName "budget.cellar")

  section "the external editor"
  do let path = inRoot "editing.cellar"
     createSheetDirectory path
     saveSheet path (Sheet [("A1", "\"before\"")] 4 2 [])
     -- A stand-in editor: it writes the cell it was handed and exits, which is
     -- everything Cellar asks of a real one.
     let stand = inRoot "stand-in-editor"
     writeFile stand "#!/bin/sh\nprintf '\"after\"\\n' > \"$1\"\n"
     permissions <- getPermissions stand
     setPermissions stand permissions { executable = True }
     started <- openExternalEditor stand path (Ref 0 0)
     check failures "the editor was started" (Just stand) started
     -- It is not waited for -- the folder is watched instead -- so the test
     -- waits for the file the way Cellar waits for the watcher.
     landed <- waitFor 100 $ do
       text <- try (readFile (cellFilePath path "A1"))
                 :: IO (Either SomeException String)
       pure $ case text of
         Right written | "after" `isInfixOf` written -> Just written
         _ -> Nothing
     check failures "and wrote the cell's own file" True (isJust landed)
     missing <- openExternalEditor "no-such-editor-anywhere" path (Ref 0 0)
     check failures "a command that is not there is refused, not thrown"
       Nothing missing
     check failures "and an empty command is refused too"
       Nothing =<< openExternalEditor "   " path (Ref 0 0)

     -- The preference that decides which program Open hands a cell to.  There
     -- used to be a switch beside the command; a config file written while it
     -- existed still names the command somebody meant, so the command is what
     -- is read out of it and the key that is gone is written back no more.
     let legacy = inRoot "legacy-config.scm"
     writeFile legacy
       ";; Cellar preferences.\n\
       \((external-editor-enabled . #f) (external-editor-command . \"gedit\"))\n"
     setEnv "CELLAR_CONFIG" legacy
     loaded <- loadConfig
     check failures "an old config file keeps its command"
       "gedit" (externalEditorCommand loaded)
     check failures "and has no recent workbooks in it"
       [] (recentWorkbooks loaded)
     saveConfig loaded
     rewritten <- readFile legacy
     check failures "and saving it drops the switch that went" False
       ("external-editor-enabled" `isInfixOf` rewritten)
     unsetEnv "CELLAR_EDITOR"
     check failures "so the command in it is the one Open runs"
       (Just "gedit") =<< effectiveEditorCommand loaded
     -- CELLAR_EDITOR wins over it.  (Setting it to the empty string, which
     -- means the desktop's own choice, cannot be tested from here: setEnv on
     -- POSIX unsets a variable rather than emptying it.)
     setEnv "CELLAR_EDITOR" "code"
     check failures "unless CELLAR_EDITOR names another"
       (Just "code") =<< effectiveEditorCommand loaded
     unsetEnv "CELLAR_EDITOR"
     check failures "and an empty preference means the desktop's too"
       Nothing =<< effectiveEditorCommand (Config "  " [])

     -- The workbooks opened lately, which the start page and the Open Recent
     -- submenu are both drawn from.
     let recentFile = inRoot "recent-config.scm"
     setEnv "CELLAR_CONFIG" recentFile
     saveConfig (Config "" [inRoot "one.cellar", inRoot "two.cellar"])
     remembered <- loadConfig
     check failures "the recent workbooks are written and read back"
       [inRoot "one.cellar", inRoot "two.cellar"] (recentWorkbooks remembered)
     check failures "opening one again moves it to the front rather than twice"
       ["/b", "/a", "/c"] (rememberRecent "/b" ["/a", "/b", "/c"])
     check failures "and the list stops where it was told to"
       recentLimit
       (length (foldr rememberRecent [] [ show n | n <- [1 .. 30 :: Int] ]))
     -- How one is written down for a person to read.  Both of these are on the
     -- start page and in the menu, and neither needs a window to answer.
     check failures "a folder under the home directory is written with a tilde"
       "~/books" (abbreviate (Just "/home/someone") "/home/someone/books")
     check failures "one outside it is written in full"
       "/srv/books" (abbreviate (Just "/home/someone") "/srv/books")
     check failures "and so is any of them when there is no home to speak of"
       "/home/someone/books" (abbreviate Nothing "/home/someone/books")
     check failures "an underscore in a menu label is doubled, not a mnemonic"
       "sales__2026.cellar" (menuLabel "/home/someone/sales_2026.cellar")
     check failures "and a label is the folder, not the path to it"
       "budget.cellar" (menuLabel "/home/someone/books/budget.cellar")
     unsetEnv "CELLAR_CONFIG"

  section "the kernel, over a real pipe"
  kernelOk <- runKernelTests failures
  removeDirectoryRecursive root

  count <- readIORef failures
  putStrLn ""
  if count == 0 && kernelOk
    then putStrLn "ALL TESTS PASSED" >> exitSuccess
    else do
      putStrLn (show count ++ " FAILURE(S)")
      exitFailure

-- | Report one comparison, counting the ones that went wrong.
check :: (Eq a, Show a) => IORef Int -> String -> a -> a -> IO ()
check failures label expected actual
  | expected == actual = putStrLn ("  ok   " ++ label)
  | otherwise = do
      modifyIORef' failures (+ 1)
      putStrLn ("  FAIL " ++ label ++ ": expected " ++ show expected
                ++ " got " ++ show actual)

section :: String -> IO ()
section title = putStrLn ("-- " ++ title)

-- | Resolve a workbook the tests have just made, and be loud if it is not one.
open :: FilePath -> IO Workbook
open path = do
  resolved <- resolveWorkbook path
  case resolved of
    Just workbook -> pure workbook
    Nothing -> error (path ++ " is not a workbook")

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

sampleSexps :: [Sexp]
sampleSexps =
  [ Sym "set-cell"
  , Str "a \"quote\" and a \\ and a ) and a newline\n"
  , Num 42
  , Num (-1)
  , Bool True
  , Nil
  , list [Sym "request", Num 1, Sym "ping"]
  , list [Pair (Sym "rows") (Num 100), Pair (Sym "active") (Str "Summary")]
  , list [list [Str "A1", Str "Qty", Bool False, Bool False, Bool False, Bool False]]
  ]

makeTemporaryDirectory :: IO FilePath
makeTemporaryDirectory = do
  base <- getTemporaryDirectory
  let path = base </> "cellar-hs-test"
  exists <- doesDirectoryExist path
  unless (not exists) (removeDirectoryRecursive path)
  createDirectoryIfMissing True path
  pure path

-- | Drive the real Guile kernel.
--
-- This is the test that the whole port turns on: a Haskell program and a Guile
-- program agreeing about a wire format, with nothing but the format between
-- them.  If the s-expression reader is wrong about escaping, or the framing is
-- wrong about byte counts, it shows up here and nowhere else.
runKernelTests :: IORef Int -> IO Bool
runKernelTests failures = do
  found <- findKernel
  case found of
    Nothing -> do
      putStrLn "  SKIP the kernel was not found (set CELLAR_KERNEL)"
      pure True
    Just (program, arguments) -> do
      kernel <- startKernel program arguments
      answers <- newIORef ([] :: [(String, String)])
      let sheet = "Sheet 1"
      -- The client answers by number, so the test keeps its own note of what
      -- each number was for, which is what the shell does with it as well.
      asked <- newIORef (M.empty :: M.Map RequestId String)
      let remember key value = modifyIORef' answers ((key, value) :)
          ask op arguments' key = do
            requestId <- call kernel op arguments'
            modifyIORef' asked (M.insert requestId key)
          drain = do
            replies <- takeReplies kernel
            forM_ replies $ \reply -> do
              let (requestId, value) = case reply of
                    Answered n payload -> (n, T.unpack (writeSexp payload))
                    Refused n why -> (n, "FAILED " ++ why)
              found <- M.lookup requestId <$> readIORef asked
              forM_ found $ \key -> remember key value
          settle key = waitFor 200 $ do
            drain
            got <- readIORef answers
            pure (lookup key got)
          expect label key wanted = do
            got <- settle key
            check failures label (Just True) (fmap (isInfixOf wanted) got)

      ask "ping" [] "ping"
      expect "the kernel answers a ping" "ping" "()"

      -- A name with a space in it, because that is what a sheet is called
      -- until somebody renames it, and because it is the name that cannot be
      -- written in front of a reference without help.
      ask "open" [ Str sheet, Num 10, Num 5
                 , list [ Pair (Str "A1") (Str "\"Qty\"")
                        , Pair (Str "A2") (Str "7")
                        , Pair (Str "B2") (Str "(* A2 6)") ] ] "open"
      expect "a sheet opens and comes back rendered" "open"
        "(\"B2\" \"42\" #t #f #f #f)"

      ask "set-cell" [Str sheet, Str "A2", Str "10"] "set"
      expect "an edit recomputes what depends on it" "set"
        "(\"B2\" \"60\" #t #f #f #f)"
      expect "and the source comes back as the model kept it" "set"
        "(source . \"10\")"

      -- Sheets naming each other, over the wire and back.
      ask "open" [ Str "Summary", Num 10, Num 5
                 , list [ Pair (Str "A1") (Str "(* #{Sheet 1!A2}# 2)") ] ] "other"
      expect "a cell can read a cell on another sheet" "other"
        "(\"A1\" \"20\" #t #f #f #f)"

      ask "set-cell" [Str sheet, Str "A2", Str "21"] "spread"
      expect "and hears about it when that cell changes" "spread"
        "(\"A1\" \"42\" #t #f #f #f)"
      expect "which arrives as the rest of the book" "spread" "(others "

      ask "set-cell" [Str sheet, Str "C1", Str "(+ Summary!A1 1)"] "back"
      expect "a plain sheet name needs no help" "back"
        "(\"C1\" \"43\" #t #f #f #f)"

      ask "rename" [Str "Summary", Str "Totals"] "rename"
      expect "renaming a sheet rewrites what names it" "rename"
        "(\"C1\" . \"(+ Totals!A1 1)\")"

      ask "set-cell" [Str sheet, Str "A2", Str "(/ 1 0)"] "bad"
      expect "an error is rendered and carries its message" "bad" "\"#ERR\""

      -- The case the whole wire format has to survive: a cell whose text is
      -- full of the punctuation the messages are made of.
      let awkward = "(string-append \"a )\" \"and a \\\" and a \\\\\")"
      ask "set-cell" [Str sheet, Str "A3", Str awkward] "awkward"
      expect "a cell full of quotes and parens survives the trip" "awkward"
        "a )and a \\\" and a \\\\"

      ask "preview" [Str sheet, Str "A4", Str "(* 3 3)"] "preview"
      expect "a preview is evaluated without being kept" "preview"
        "(display . \"9\")"

      ask "snapshot" [Str "nowhere"] "missing"
      expect "a sheet that is not open is refused" "missing"
        "FAILED no sheet called \"nowhere\" is open"

      ask "move" [Str sheet, Sym "row", Num 0, Num 1] "move"
      expect "a move reports the sources it rewrote" "move" "(sources"

      before <- outstanding kernel
      check failures "nothing is left outstanding" 0 before

      -- The stall watchdog, which decides when Cellar offers to interrupt a
      -- cell.  Nothing is pumped between here and the end of the block: a
      -- reply the main loop has not handed out yet leaves the request
      -- outstanding, which is exactly the state being timed, and it makes the
      -- timing of the test its own rather than the kernel's.
      ask "ping" [] "older"
      threadDelay 300000
      ask "ping" [] "newer"
      waiting <- outstanding kernel
      oldestOp <- waitingOp kernel
      check failures "two requests wait, and the older one is the one timed"
        (2, Just "ping") (waiting, oldestOp)
      waited <- waitingFor kernel
      check failures "which has been waiting as long as it was left"
        True (maybe False (>= 0.3) waited)
      check failures "long enough to count as stalled" True =<< stalled kernel 0.2
      -- A kernel that is still starting has been slow for reasons that have
      -- nothing to do with the cell it was handed, so the first answer of any
      -- kind restarts the clock rather than the request being reported.
      markReady kernel
      afterReady <- waitingFor kernel
      check failures "marking the kernel ready starts the clock again"
        True (maybe False (< 0.3) afterReady)
      check failures "so nothing is stalled any more" False =<< stalled kernel 0.2
      _ <- settle "older"
      _ <- settle "newer"
      cleared <- outstanding kernel
      idle <- waitingOp kernel
      check failures "and both answers clear both requests" 0 cleared
      check failures "leaving nothing waiting to be named" Nothing idle

      -- The reason the kernel is its own process.
      ask "set-cell" [Str sheet, Str "A5", Str "(let loop () (loop))"] "runaway"
      threadDelay 1500000
      drain
      spinning <- outstanding kernel
      check failures "a cell that will not finish leaves a request outstanding" 1 spinning
      stalledNow <- stalled kernel 0.5
      check failures "and the kernel counts as stalled" True stalledNow
      -- Pumping over a wedged kernel has to return, every time.
      forM_ [1 :: Int .. 50] (const drain)
      check failures "and the shell takes what there is without blocking" True True

      restartKernel kernel
      afterRestart <- outstanding kernel
      alive <- kernelAlive kernel
      check failures "restarting abandons what was outstanding" 0 afterRestart
      check failures "and leaves a kernel running" True alive

      ask "ping" [] "ping2"
      expect "which answers" "ping2" "()"

      stopKernel kernel
      stopped <- kernelAlive kernel
      check failures "a stopped kernel is not alive" False stopped
      pure True

-- | Poll until something turns up, pumping as we go.
waitFor :: Int -> IO (Maybe a) -> IO (Maybe a)
waitFor 0 _ = pure Nothing
waitFor tries action = do
  got <- action
  case got of
    Just value -> pure (Just value)
    Nothing -> threadDelay 50000 >> waitFor (tries - 1) action

-- | Where the kernel is.  @CELLAR_KERNEL@ names it outright; otherwise it is
-- looked for beside the tests, which is where it lives in the source tree.
findKernel :: IO (Maybe (FilePath, [String]))
findKernel = do
  override <- lookupEnv "CELLAR_KERNEL"
  case override of
    Just command | not (null command) -> case words command of
      (program : arguments) -> pure (Just (program, arguments))
      [] -> pure Nothing
    _ -> do
      here <- getCurrentDirectory
      let script = here </> "bin" </> "cellar-kernel.scm"
      exists <- doesFileExist script
      pure $ if exists
        then Just ("guile", ["-L", here </> "src", "-s", script])
        else Nothing
