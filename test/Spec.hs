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
import Data.List (isInfixOf, sort)
import Data.Maybe (isJust)
import qualified Data.Text as T
import System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

import Cellar.Client
import Cellar.Config
import Cellar.External
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
       Nothing =<< effectiveEditorCommand (Config "  ")
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
      let remember key value = modifyIORef' answers ((key, value) :)
          ask op arguments' key = call kernel op arguments'
            (\payload -> remember key (T.unpack (writeSexp payload)))
            (\why -> remember key ("FAILED " ++ why))
          settle key = waitFor 200 $ do
            _ <- pump kernel
            got <- readIORef answers
            pure (lookup key got)
          expect label key wanted = do
            got <- settle key
            check failures label (Just True) (fmap (isInfixOf wanted) got)

      ask "ping" [] "ping"
      expect "the kernel answers a ping" "ping" "()"

      ask "open" [ Num 1, Num 10, Num 5
                 , list [ Pair (Str "A1") (Str "\"Qty\"")
                        , Pair (Str "A2") (Str "7")
                        , Pair (Str "B2") (Str "(* A2 6)") ] ] "open"
      expect "a sheet opens and comes back rendered" "open"
        "(\"B2\" \"42\" #t #f #f #f)"

      ask "set-cell" [Num 1, Str "A2", Str "10"] "set"
      expect "an edit recomputes what depends on it" "set"
        "(\"B2\" \"60\" #t #f #f #f)"
      expect "and the source comes back as the model kept it" "set"
        "(source . \"10\")"

      ask "set-cell" [Num 1, Str "A2", Str "(/ 1 0)"] "bad"
      expect "an error is rendered and carries its message" "bad" "\"#ERR\""

      -- The case the whole wire format has to survive: a cell whose text is
      -- full of the punctuation the messages are made of.
      let awkward = "(string-append \"a )\" \"and a \\\" and a \\\\\")"
      ask "set-cell" [Num 1, Str "A3", Str awkward] "awkward"
      expect "a cell full of quotes and parens survives the trip" "awkward"
        "a )and a \\\" and a \\\\"

      ask "preview" [Num 1, Str "A4", Str "(* 3 3)"] "preview"
      expect "a preview is evaluated without being kept" "preview"
        "(display . \"9\")"

      ask "snapshot" [Num 99] "missing"
      expect "a sheet that is not open is refused" "missing"
        "FAILED no sheet called 99 is open"

      ask "move" [Num 1, Sym "row", Num 0, Num 1] "move"
      expect "a move reports the sources it rewrote" "move" "(sources"

      before <- outstanding kernel
      check failures "nothing is left outstanding" 0 before

      -- The reason the kernel is its own process.
      ask "set-cell" [Num 1, Str "A5", Str "(let loop () (loop))"] "runaway"
      threadDelay 1500000
      _ <- pump kernel
      spinning <- outstanding kernel
      check failures "a cell that will not finish leaves a request outstanding" 1 spinning
      stalledNow <- stalled kernel 0.5
      check failures "and the kernel counts as stalled" True stalledNow
      -- Pumping over a wedged kernel has to return, every time.
      forM_ [1 :: Int .. 50] (const (pump kernel))
      check failures "and the shell pumps over it without blocking" True True

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
