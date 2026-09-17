-- | What the shell can ask the kernel, one constructor for each.
--
-- The wire carries @(request 7 set-cell "Summary" "A1" "=1+2")@, which is a
-- name and a list of s-expressions, and anything at all can be built in that
-- shape.  The point of this module is that only the twelve things the kernel
-- answers can be built in /this/ shape, with the right number of arguments,
-- each of the right kind.  A request the kernel would refuse is a request that
-- does not compile.
--
-- The kernel keeps the same list, in @%operations@ in @src/cellar/kernel.scm@,
-- because it dispatches on it and can say what it takes.  Two copies of one
-- list drift, so the kernel answers an @operations@ request with its own and
-- the suite checks the two against each other, the way the reference
-- arithmetic is checked.  See @test/Window.hs@.
--
-- What is /not/ here is which answer a request comes back with.  Pairing an
-- operation with the wrong tag is still possible, and making it impossible
-- wants 'Cellar.App.State.Tag' indexed by the shape of its reply, which is a
-- GADT and reaches into the map of what the kernel owes and into every test
-- that shows a tag.  That is a lot of machinery for a mistake nobody has made
-- yet.
module Cellar.Op
  ( Op (..)
  , opName
  , opArguments
  , opArity
  , everyOp
  ) where

import Cellar.Ref (Axis (..))
import Cellar.Sexp

-- | A request, with its arguments already the right shape.
--
-- Sheets are named by the string the kernel knows them by, which is the name
-- on the tab.  Cells are named the way they are written, @A1@.
data Op
  = Ping
    -- ^ Nothing in particular, so that the first answer says the kernel is up.
  | Open String Int Int [(String, String)]
    -- ^ Hand over a sheet read off the disk: its name, how many rows and
    -- columns to give it, and what is in its cells.
  | Close String
  | Rename String String
  | SetCell String String String
    -- ^ The sheet, the cell, and what was typed into it.
  | Preview String String String
    -- ^ The same, evaluated without being kept.
  | Move String Axis Int Int
  | Insert String Axis Int
  | Delete String Axis Int
  | Recalculate String
  | Snapshot String
    -- ^ What the sheet comes to now.  Nothing in the window asks for this;
    -- the kernel answers it and the kernel's own suite uses it.
  | Sources String
    -- ^ What was typed into every cell of the sheet.  Nothing in the window
    -- asks for this either.
  | Operations
    -- ^ What the kernel answers, and how many arguments each one takes.
  deriving (Eq, Show)

-- | The name that goes on the wire.
opName :: Op -> String
opName op = case op of
  Ping -> "ping"
  Open {} -> "open"
  Close {} -> "close"
  Rename {} -> "rename"
  SetCell {} -> "set-cell"
  Preview {} -> "preview"
  Move {} -> "move"
  Insert {} -> "insert"
  Delete {} -> "delete"
  Recalculate {} -> "recalculate"
  Snapshot {} -> "snapshot"
  Sources {} -> "sources"
  Operations -> "operations"

opArguments :: Op -> [Sexp]
opArguments op = case op of
  Ping -> []
  Open sheet rows columns cells ->
    [ Str sheet, Num (fromIntegral rows), Num (fromIntegral columns)
    , list [ Pair (Str name) (Str source) | (name, source) <- cells ] ]
  Close sheet -> [Str sheet]
  Rename from to -> [Str from, Str to]
  SetCell sheet cell source -> [Str sheet, Str cell, Str source]
  Preview sheet cell source -> [Str sheet, Str cell, Str source]
  Move sheet axis from to ->
    [Str sheet, axisOf axis, Num (fromIntegral from), Num (fromIntegral to)]
  Insert sheet axis at -> [Str sheet, axisOf axis, Num (fromIntegral at)]
  Delete sheet axis at -> [Str sheet, axisOf axis, Num (fromIntegral at)]
  Recalculate sheet -> [Str sheet]
  Snapshot sheet -> [Str sheet]
  Sources sheet -> [Str sheet]
  Operations -> []

opArity :: Op -> Int
opArity = length . opArguments

-- | One of each, for asking the kernel whether it knows the same twelve.
--
-- The arguments are stand-ins and are never sent: only the name and the number
-- of them is looked at.
everyOp :: [Op]
everyOp =
  [ Ping
  , Open "" 0 0 []
  , Close ""
  , Rename "" ""
  , SetCell "" "" ""
  , Preview "" "" ""
  , Move "" Row 0 0
  , Insert "" Row 0
  , Delete "" Row 0
  , Recalculate ""
  , Snapshot ""
  , Sources ""
  , Operations
  ]

axisOf :: Axis -> Sexp
axisOf Row = Sym "row"
axisOf Column = Sym "column"
