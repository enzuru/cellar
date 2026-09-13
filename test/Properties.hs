{-# LANGUAGE OverloadedStrings #-}

-- | The laws, run against generated input.
--
-- The suite in @test/Spec.hs@ says what the rules are, one example at a time,
-- and a person reading it learns the program from it.  This says the rules
-- hold, which an example cannot: an example is one point, and the points
-- somebody thought to write down are the ones they already had in mind.
--
-- Both are kept.  A property that fails tells you a law is broken and hands
-- you the smallest input that breaks it; an example tells you what the law was
-- for.
--
-- The one worth most here is the first.  The arithmetic for where a reference
-- lands when a sheet is rearranged is written twice, once in "Cellar.Ref" and
-- once in @src/cellar/ref.scm@, because both halves of Cellar need it and
-- neither owns it.  Two copies of one rule drift quietly, and nothing in
-- either language would say so.  That property runs the same cases through
-- both and compares what they print.
--
-- Nothing here needs a display.  It does need @guile@ on the path and the
-- working directory at the top of the repository, which is where @make check@
-- runs it from.
module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as B
import Data.Char (isAlphaNum)
import Data.List (sort)
import System.Exit (exitFailure)
import System.Process (readProcess)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cellar.Grid.Model
import Cellar.Protocol
import Cellar.Ref
import Cellar.Sexp
import Cellar.View

main :: IO ()
main = do
  held <- checkSequential $ Group "the laws"
    [ ("a reference means the same thing to the kernel", refMatchesTheKernel)
    , ("every reference survives being written and read", refRoundTrips)
    , ("an insert and the delete of what it opened cancel out", insertIsUndone)
    , ("a move and the move back leave everything where it was", moveIsUndone)
    , ("moving an item rearranges the list and takes nothing out", moveIsAPermutation)
    , ("an s-expression survives being written and read", sexpRoundTrips)
    , ("the decoder reads the same messages however the bytes arrive", framingIgnoresChunks)
    , ("no two columns of a grid share an identifier", columnsStayDistinct)
    , ("a view only ever grows the grid", theGridOnlyGrows)
    ]
  unless held exitFailure

--
-- The arithmetic both halves know
--

-- | One call, to be made in Haskell and in Scheme and compared.
--
-- Indices are generated non-negative because that is what a sheet has.  A
-- negative column index is not something either half is asked for, and the two
-- do disagree about it: Scheme's @remainder@ and Haskell's @mod@ take opposite
-- signs.
data Call
  = ColumnNameOf Int
  | ParseColumnOf String
  | RefNameOf Ref
  | ParseRefOf String
  | ShiftIndexOf Int Int Int
  | AfterMoveOf Ref Axis Int Int
  | ShiftForInsertOf Int Int
  | AfterInsertOf Ref Axis Int
  | ShiftForDeleteOf Int Int
  | AfterDeleteOf Ref Axis Int
  | ShiftPastDeleteOf Int Int
  | PastDeleteOf Ref Axis Int
  deriving (Show)

refMatchesTheKernel :: Property
refMatchesTheKernel = withTests 60 . property $ do
  calls <- forAll (Gen.list (Range.linear 10 60) genCall)
  fromTheKernel <- evalIO (askGuile (map inScheme calls))
  map inHaskell calls === fromTheKernel

-- | Ask the kernel's own module what these come to.
--
-- One process for the whole batch rather than one per call: a Guile start is
-- some ten milliseconds, and a property that spent a second of it per case
-- would be a property nobody runs.
askGuile :: [String] -> IO [String]
askGuile expressions = lines <$> readProcess "guile" ["-L", "src", "-c", program] ""
  where
    program = unlines
      [ "(use-modules (cellar ref))"
      , "(for-each (lambda (answer) (write answer) (newline))"
      , "          (list " ++ unwords expressions ++ "))"
      ]

inScheme :: Call -> String
inScheme call = case call of
  ColumnNameOf n -> "(column->name " ++ show n ++ ")"
  ParseColumnOf s -> "(name->column " ++ schemeString s ++ ")"
  RefNameOf r -> "(ref->name " ++ schemeRef r ++ ")"
  ParseRefOf s -> "(name->ref " ++ schemeString s ++ ")"
  ShiftIndexOf i from to -> unwords ["(shift-index", show i, show from, show to ++ ")"]
  AfterMoveOf r axis from to ->
    unwords ["(ref-after-move", schemeRef r, schemeAxis axis, show from, show to ++ ")"]
  ShiftForInsertOf i at -> unwords ["(shift-index-for-insert", show i, show at ++ ")"]
  AfterInsertOf r axis at ->
    unwords ["(ref-after-insert", schemeRef r, schemeAxis axis, show at ++ ")"]
  ShiftForDeleteOf i at -> unwords ["(shift-index-for-delete", show i, show at ++ ")"]
  AfterDeleteOf r axis at ->
    unwords ["(ref-after-delete", schemeRef r, schemeAxis axis, show at ++ ")"]
  ShiftPastDeleteOf i at -> unwords ["(shift-index-past-delete", show i, show at ++ ")"]
  PastDeleteOf r axis at ->
    unwords ["(ref-past-delete", schemeRef r, schemeAxis axis, show at ++ ")"]

-- | The same answer, written the way Guile's @write@ writes it.
inHaskell :: Call -> String
inHaskell call = case call of
  ColumnNameOf n -> show (columnName n)
  ParseColumnOf s -> maybe "#f" show (parseColumn s)
  RefNameOf r -> show (refName r)
  ParseRefOf s -> maybe "#f" pairOf (parseRef s)
  ShiftIndexOf i from to -> show (shiftIndex i from to)
  AfterMoveOf r axis from to -> pairOf (refAfterMove r axis from to)
  ShiftForInsertOf i at -> show (shiftIndexForInsert i at)
  AfterInsertOf r axis at -> pairOf (refAfterInsert r axis at)
  ShiftForDeleteOf i at -> maybe "#f" show (shiftIndexForDelete i at)
  AfterDeleteOf r axis at -> maybe "#f" pairOf (refAfterDelete r axis at)
  ShiftPastDeleteOf i at -> show (shiftIndexPastDelete i at)
  PastDeleteOf r axis at -> pairOf (refPastDelete r axis at)

pairOf :: Ref -> String
pairOf (Ref row column) = "(" ++ show row ++ " . " ++ show column ++ ")"

schemeRef :: Ref -> String
schemeRef (Ref row column) = unwords ["(make-ref", show row, show column ++ ")"]

schemeAxis :: Axis -> String
schemeAxis Row = "'row"
schemeAxis Column = "'column"

-- | Names are letters and digits and nothing else, which is what a cell's name
-- is, so neither side has to agree about escaping to be compared.
schemeString :: String -> String
schemeString s = show s

genCall :: Gen Call
genCall = Gen.choice
  [ ColumnNameOf <$> genIndex
  , ParseColumnOf <$> genName
  , RefNameOf <$> genRef
  , ParseRefOf <$> genName
  , ShiftIndexOf <$> genIndex <*> genIndex <*> genIndex
  , AfterMoveOf <$> genRef <*> genAxis <*> genIndex <*> genIndex
  , ShiftForInsertOf <$> genIndex <*> genIndex
  , AfterInsertOf <$> genRef <*> genAxis <*> genIndex
  , ShiftForDeleteOf <$> genIndex <*> genIndex
  , AfterDeleteOf <$> genRef <*> genAxis <*> genIndex
  , ShiftPastDeleteOf <$> genIndex <*> genIndex
  , PastDeleteOf <$> genRef <*> genAxis <*> genIndex
  ]

-- | An index, small far more often than not.
--
-- Where two copies of this arithmetic drift is at the boundaries: one of them
-- writing @<@ where the other writes @<=@ changes the answer only when two of
-- the three indices are equal or next to each other.  Three numbers drawn
-- independently out of nought to eight hundred are almost never equal, so a
-- generator of that shape runs thousands of cases through the interesting
-- functions and never once asks the question they differ on.  The wide range
-- is kept for the sake of columns past @Z@, which need an index above 26.
genIndex :: Gen Int
genIndex = Gen.frequency
  [ (5, Gen.int (Range.linear 0 12))
  , (1, Gen.int (Range.linear 0 800))
  ]

genRef :: Gen Ref
genRef = Ref <$> genIndex <*> genIndex

genAxis :: Gen Axis
genAxis = Gen.element [Row, Column]

-- | A mix of names that are references and names that are not, so that the two
-- halves are compared on what they refuse as well as on what they take.
genName :: Gen String
genName = Gen.choice
  [ (\r -> refName r) <$> genRef
  , columnName <$> genIndex
  , Gen.string (Range.linear 0 6) (Gen.filter isAlphaNum Gen.ascii)
  ]

refRoundTrips :: Property
refRoundTrips = property $ do
  r <- forAll genRef
  parseRef (refName r) === Just r
  parseColumn (columnName (refColumn r)) === Just (refColumn r)

--
-- Rearranging
--

-- | An insert and the delete of what it opened leave the sheet as it was.
insertIsUndone :: Property
insertIsUndone = property $ do
  r <- forAll genRef
  axis <- forAll genAxis
  at <- forAll genIndex
  refAfterDelete (refAfterInsert r axis at) axis at === Just r

moveIsUndone :: Property
moveIsUndone = property $ do
  (from, to, items) <- forAll genMove
  moveItem to from (moveItem from to items) === items
  i <- forAll (Gen.int (Range.linear 0 (length items - 1)))
  shiftIndex (shiftIndex i from to) to from === i

moveIsAPermutation :: Property
moveIsAPermutation = property $ do
  (from, to, items) <- forAll genMove
  let moved = moveItem from to items
  sort moved === sort items
  -- Where the moved item landed, and where the arithmetic said it would.
  length moved === length items
  moved !! to === items !! from

-- | A list with at least one thing in it, and two positions inside it.
genMove :: Gen (Int, Int, [Int])
genMove = do
  items <- Gen.list (Range.linear 1 30) (Gen.int (Range.linear 0 1000))
  from <- Gen.int (Range.linear 0 (length items - 1))
  to <- Gen.int (Range.linear 0 (length items - 1))
  pure (from, to, items)

--
-- The wire
--

sexpRoundTrips :: Property
sexpRoundTrips = property $ do
  value <- forAll genSexp
  parseSexp (writeSexp value) === Right value

-- | The subset the protocol stays inside.
--
-- Symbols are generated from an alphabet that cannot be read back as something
-- else: a symbol of digits is a number, a lone @.@ is what makes a dotted
-- pair, and a leading @#@ is a boolean.  Strings are unrestricted, because the
-- writer escapes them and the reader is meant to take anything back.  Doubles
-- are finite, because @NaN@ has no written form either side reads.
genSexp :: Gen Sexp
genSexp = Gen.recursive Gen.choice
  [ Sym <$> genSymbol
  , Str <$> Gen.string (Range.linear 0 12) Gen.unicode
  , Num <$> (fromIntegral <$> Gen.int (Range.linearFrom 0 (-100000) 100000))
  , Real <$> Gen.double (Range.linearFracFrom 0 (-1000) 1000)
  , Bool <$> Gen.bool
  , pure Nil
  ]
  [ Gen.subterm2 genSexp genSexp Pair
  , list <$> Gen.list (Range.linear 0 4) genSexp
  ]

genSymbol :: Gen String
genSymbol = (:) <$> Gen.alpha <*> Gen.string (Range.linear 0 6) letterOrDigit
  where letterOrDigit = Gen.choice [Gen.alpha, Gen.digit, pure '-']

framingIgnoresChunks :: Property
framingIgnoresChunks = property $ do
  bodies <- forAll (Gen.list (Range.linear 0 6) genSexp)
  numbers <- forAll (Gen.list (Range.singleton (length bodies))
                              (Gen.int (Range.linear 0 100000)))
  let sent = zipWith (\n body -> list [Sym "reply", Num (fromIntegral n), body])
                     numbers bodies
      bytes = B.concat (map encode sent)
      expected = zipWith Reply numbers bodies
  splits <- forAll (Gen.list (Range.linear 0 8)
                             (Gen.int (Range.linear 0 (B.length bytes))))
  readAll (chop (sort splits) bytes) === expected
  -- The whole thing at once is the same, which is what the split is measured
  -- against rather than against a hand-written list.
  readAll [bytes] === expected

-- | Feed the chunks in order and take back every message that completed.
readAll :: [B.ByteString] -> [Message]
readAll = go newDecoder
  where
    go _ [] = []
    go decoder (chunk : rest) =
      let (next, messages) = feed decoder chunk
      in messages ++ go next rest

-- | Cut a buffer at these offsets, which are in order and may repeat.
chop :: [Int] -> B.ByteString -> [B.ByteString]
chop [] bytes = [bytes]
chop (at : rest) bytes =
  let (here, more) = B.splitAt at bytes
  in here : chop (map (subtract at) rest) more

--
-- The grid
--

-- | What somebody does to a sheet, as far as the columns are concerned.
data Change = InsertAt Int | MoveFromTo Int Int | Grew Int Int
  deriving (Show)

columnsStayDistinct :: Property
columnsStayDistinct = property $ do
  (start, changes) <- forAll genChanges
  let columns = modelColumns (foldl apply (began start) changes)
  sort columns === dedup (sort columns)

theGridOnlyGrows :: Property
theGridOnlyGrows = property $ do
  (start, changes) <- forAll genChanges
  let steps = scanl apply (began start) changes
      pairs = zip steps (drop 1 steps)
  assert (all (\(a, b) -> modelRows b >= modelRows a) pairs)
  assert (all (\(a, b) -> width b >= width a) pairs)
  where width = length . modelColumns

began :: (Int, Int) -> GridModel
began (rows, columns) = newGridModel (emptyView rows columns)

apply :: GridModel -> Change -> GridModel
apply model change = case change of
  InsertAt at -> maybe model fst (insertLine Column at model)
  MoveFromTo from to -> maybe model fst (moveLine Column from to model)
  Grew rows columns -> withView (emptyView rows columns) model

-- | The size a sheet starts at, and what is then done to it.  The size rather
-- than the model itself, because a generated value has to be printable when a
-- property fails and a 'GridModel' is not.
genChanges :: Gen ((Int, Int), [Change])
genChanges = do
  size <- (,) <$> small <*> small
  changes <- Gen.list (Range.linear 0 20) genChange
  pure (size, changes)
  where
    small = Gen.int (Range.linear 1 12)
    genChange = Gen.choice
      [ InsertAt <$> Gen.int (Range.linear 0 14)
      , MoveFromTo <$> Gen.int (Range.linear 0 14) <*> Gen.int (Range.linear 0 14)
      , Grew <$> Gen.int (Range.linear 0 20) <*> Gen.int (Range.linear 0 20)
      ]

dedup :: Eq a => [a] -> [a]
dedup (a : b : rest) | a == b = dedup (b : rest)
dedup (a : rest) = a : dedup rest
dedup [] = []
