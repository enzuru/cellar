-- | Cellar: a spreadsheet whose cells are Guile expressions.
--
-- This program is the shell -- the window, the folder on disk, the tabs.  It
-- starts the kernel, which is a Guile program, and talks to it over a pipe.
module Main (main) where

import System.Environment (getArgs)

import Cellar.App (runApp)

main :: IO ()
main = getArgs >>= runApp
