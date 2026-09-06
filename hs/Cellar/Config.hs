-- | Preferences that outlive the session.
--
-- One small alist, written to @$XDG_CONFIG_HOME/cellar/config.scm@.  It holds
-- the only preference Cellar has: whether to hand cells to an external editor
-- instead of the built-in one, and what to run when it does.
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
  , EditorOverride (..)
  , editorOverride
  , splitCommand
  , editorArgv
  ) where

import Control.Exception (SomeException, try)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)

import Cellar.Sexp

data Config = Config
  { externalEditorEnabled :: Bool
  , externalEditorCommand :: String
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config False ""

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
    contents <- try (readFile path) :: IO (Either SomeException String)
    pure $ case contents of
      Left _ -> defaultConfig
      Right text -> case parseSexp text of
        Left _ -> defaultConfig
        Right value -> Config
          { externalEditorEnabled =
              maybe (externalEditorEnabled defaultConfig) asBool
                (lookupKey "external-editor-enabled" value)
          , externalEditorCommand =
              fromMaybe (externalEditorCommand defaultConfig)
                (lookupKey "external-editor-command" value >>= asString)
          }

saveConfig :: Config -> IO ()
saveConfig config = do
  path <- configFilePath
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path text
  where
    text = ";; Cellar preferences.\n" ++ writeSexp value ++ "\n"
    value = list
      [ Pair (Sym "external-editor-enabled") (Bool (externalEditorEnabled config))
      , Pair (Sym "external-editor-command") (Str (externalEditorCommand config))
      ]

-- | What @CELLAR_EDITOR@ has to say, if anything.
data EditorOverride
  = UseInternal        -- ^ Set, but empty: force the built-in editor.
  | UseCommand String  -- ^ Set to a command: force that.
  | NoOverride         -- ^ Not set at all: the preference decides.
  deriving (Eq, Show)

editorOverride :: IO EditorOverride
editorOverride = do
  value <- lookupEnv "CELLAR_EDITOR"
  pure $ case value of
    Nothing -> NoOverride
    Just raw | all isSpace raw -> UseInternal
             | otherwise -> UseCommand (trim raw)

-- | The command to run instead of the built-in editor, or 'Nothing' to use the
-- built-in one.  @CELLAR_EDITOR@ wins over the saved preference: set it to a
-- command to force an external editor for one run, or to the empty string to
-- force the built-in one.
effectiveEditorCommand :: Config -> IO (Maybe String)
effectiveEditorCommand config = do
  override <- editorOverride
  pure $ case override of
    UseInternal -> Nothing
    UseCommand command -> Just command
    NoOverride
      | externalEditorEnabled config
      , let command = trim (externalEditorCommand config)
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
