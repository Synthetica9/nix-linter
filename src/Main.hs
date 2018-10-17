module Main where

import           Data.Foldable
import           System.Environment

import           Nix.Parser
import           Nix.Pretty

import           Linter

main :: IO ()
main = do
  args <- getArgs
  for_ args $ \arg -> do
    Success parsed <- parseNixFileLoc arg
    for_ (checkAll parsed) print
