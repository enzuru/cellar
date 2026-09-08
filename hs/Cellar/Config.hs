-- | Preferences that outlive the session.
--
-- One small alist, written to @$XDG_CONFIG_HOME/cellar/config.scm@.  It holds
-- the command to open a cell with, for when the program the desktop would pick
-- is not the one you want, and the workbooks opened lately.
--
-- The file is s-expressions because the Guile shell wrote it that way and a
-- change of language on this side is no reason to make somebody's config file
-- unreadable by the version they had yesterday.
module Cellar.Config
  ( Config (..)
  , defaultConfig
  , configFilePath
  , loadConfig
  , saveConfig
  , effectiveEditorCommand
  , recentLimit
  , rememberRecent
  , abbreviate
  , menuLabel
  , EditorOverride (..)
  , editorOverride
  , splitCommand
  , editorArgv
  ) where

import Control.Exception (SomeException, try)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.IO (IOMode (..), hSetEncoding, utf8, withFile)
import System.FilePath ((</>), takeDirectory, takeFileName)

import Cellar.Sexp

-- | The command Open runs on a cell, or empty for whatever the desktop opens
-- text files with.
--
-- There used to be a switch beside it -- /use an external editor/ -- because
-- one button had to serve both editors and something had to say which it meant.
-- The cell bar has two buttons now: the pencil is always Cellar's own editor
-- and the folder is always another program, so the only thing left to say is
-- which other program, and a command that is empty answers that with \"the one
-- you already open text files in\".
data Config = Config
  { externalEditorCommand :: String
    -- | The workbooks opened lately, newest first.  Folders, because that is
    -- what a workbook is, which is also why GTK's own recent-files list is no
    -- use here: it is keyed on files, and it was deprecated in GTK 4.10
    -- besides.  Ten paths in a preferences file is the whole of it.
  , recentWorkbooks :: [FilePath]
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config "" []

-- | How many workbooks the list remembers.  Enough to hold a week of work,
-- short enough to read without scrolling.
recentLimit :: Int
recentLimit = 10

-- | Put a workbook at the front of the list, where it is the one opened last.
-- A workbook already in the list moves rather than repeats.
rememberRecent :: FilePath -> [FilePath] -> [FilePath]
rememberRecent path paths = take recentLimit (path : filter (/= path) paths)

-- | A folder as it reads to somebody who lives in it: the home directory,
-- when the path is under it, written as @~@.  The home directory is passed in
-- rather than looked up, which is what makes this answerable without a window
-- or an environment.
abbreviate :: Maybe FilePath -> FilePath -> String
abbreviate home path = fromMaybe path $ do
  root <- home
  rest <- stripLeading root path
  pure ('~' : rest)
  where
    stripLeading prefix full = case splitAt (length prefix) full of
      (start, rest) | start == prefix -> Just rest
      _ -> Nothing

-- | A workbook's folder as a menu label.  An underscore in a label is a
-- mnemonic, so a workbook called @sales_2026@ would show as @sales2026@ with a
-- letter underlined; doubling them is how one is spelled literally.
--
-- This and 'abbreviate' are how the recent workbooks are written down for a
-- person to read.  They live here, beside the list itself, because they are
-- arithmetic on strings: the module that puts them on screen needs GTK to
-- compile, and nothing that needs GTK can be tested without a display.
menuLabel :: FilePath -> String
menuLabel = concatMap (\c -> if c == '_' then "__" else [c]) . takeFileName

-- | Where the preferences live.  @CELLAR_CONFIG@ overrides it, which is how
-- the tests get a config file of their own.
configFilePath :: IO FilePath
configFilePath = do
  override <- lookupEnv "CELLAR_CONFIG"
  case override of
    Just path | not (null path) -> pure path
    _ -> do
      xdg <- lookupEnv "XDG_CONFIG_HOME"
      home <- lookupEnv "HOME"
      let base = fromMaybe (fromMaybe "." home </> ".config") xdg
      pure (base </> "cellar" </> "config.scm")

-- | Read the preferences back.  A missing file is the ordinary first-run case
-- and a corrupt one is not worth refusing to start over; either way the
-- defaults stand.
loadConfig :: IO Config
loadConfig = do
  path <- configFilePath
  exists <- doesFileExist path
  if not exists then pure defaultConfig else do
    contents <- try (readUtf8 path) :: IO (Either SomeException Text)
    pure $ case contents of
      Left _ -> defaultConfig
      -- A file from the version that had the switch has an
      -- @external-editor-enabled@ key too.  It is read straight past rather
      -- than rejected: the command in that file is still the command you
      -- meant, and the next save drops the key.
      Right text -> case parseSexp text of
        Left _ -> defaultConfig
        Right value -> Config
          { externalEditorCommand =
              fromMaybe (externalEditorCommand defaultConfig)
                (lookupKey "external-editor-command" value >>= asString)
          , recentWorkbooks =
              fromMaybe []
                (lookupKey "recent-workbooks" value >>= toList >>= mapM asString)
          }

saveConfig :: Config -> IO ()
saveConfig config = do
  path <- configFilePath
  createDirectoryIfMissing True (takeDirectory path)
  withFile path WriteMode $ \handle -> do
    hSetEncoding handle utf8
    TIO.hPutStr handle text
  where
    text = T.pack ";; Cellar preferences.\n" <> writeSexp value <> T.pack "\n"
    value = list
      [ Pair (Sym "external-editor-command") (Str (externalEditorCommand config))
      , Pair (Sym "recent-workbooks") (list (map Str (recentWorkbooks config)))
      ]

-- | What @CELLAR_EDITOR@ has to say, if anything.
data EditorOverride
  = UseDesktop         -- ^ Set, but empty: ignore the preference and let the
                       --   desktop pick the program.
  | UseCommand String  -- ^ Set to a command: force that.
  | NoOverride         -- ^ Not set at all: the preference decides.
  deriving (Eq, Show)

editorOverride :: IO EditorOverride
editorOverride = do
  value <- lookupEnv "CELLAR_EDITOR"
  pure $ case value of
    Nothing -> NoOverride
    Just raw | all isSpace raw -> UseDesktop
             | otherwise -> UseCommand (trim raw)

-- | The command Open should run, or 'Nothing' for the program the desktop
-- opens text files with.  @CELLAR_EDITOR@ wins over the saved preference: set
-- it to a command to force that command for one run, or to the empty string to
-- force the desktop's own choice.
effectiveEditorCommand :: Config -> IO (Maybe String)
effectiveEditorCommand config = do
  override <- editorOverride
  pure $ case override of
    UseDesktop -> Nothing
    UseCommand command -> Just command
    NoOverride
      | let command = trim (externalEditorCommand config)
      , not (null command) -> Just command
      | otherwise -> Nothing

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Split a command the way a shell would: single quotes take everything
-- literally, double quotes group without hiding backslashes, and a backslash
-- outside single quotes escapes the character after it.
splitCommand :: String -> [String]
splitCommand = go [] [] False Nothing
  where
    go arguments current started quotation input = case input of
      [] -> reverse (if started then emit else arguments)
      ('\\' : next : rest)
        | quotation == Just '\'' -> go arguments ('\\' : current) True quotation (next : rest)
        | otherwise -> go arguments (next : current) True quotation rest
      (c : rest)
        | quotation == Nothing && (c == '"' || c == '\'') ->
            go arguments current True (Just c) rest
        | quotation == Just c -> go arguments current True Nothing rest
        | quotation == Nothing && isSpace c ->
            if started then go emit [] False Nothing rest
                       else go arguments [] False Nothing rest
        | otherwise -> go arguments (c : current) True quotation rest
      where emit = reverse current : arguments

-- | The argument vector that runs a command on a path.  A @%s@ anywhere in the
-- command becomes the file name -- @xterm -e vim %s@ -- and without one the
-- file is added at the end, which is what a plain @gnome-text-editor@ wants.
-- Substitution happens after splitting, so a path with spaces in it stays a
-- single argument.
editorArgv :: String -> FilePath -> [String]
editorArgv command path =
  let tokens = splitCommand command
      substituted = map (substitute path) tokens
      used = any snd substituted
      argv = map fst substituted
  in case argv of
       [] -> []
       _ | used -> argv
         | otherwise -> argv ++ [path]

-- | A token with every @%s@ replaced by the path and every @%%@ by a literal
-- @%@, and whether a @%s@ was there at all.
substitute :: FilePath -> String -> (String, Bool)
substitute path = go [] False
  where
    go acc used input = case input of
      [] -> (reverse acc, used)
      ('%' : 's' : rest) -> go (reverse path ++ acc) True rest
      ('%' : '%' : rest) -> go ('%' : acc) used rest
      (c : rest) -> go (c : acc) used rest

-- | A file's contents as text, decoded as UTF-8 whatever the locale says.
readUtf8 :: FilePath -> IO Text
readUtf8 path = withFile path ReadMode $ \handle -> do
  hSetEncoding handle utf8
  contents <- TIO.hGetContents handle
  T.length contents `seq` pure contents
