module Main where

import           Data.Foldable
import           System.Environment

import           Nix.Parser
import           Nix.Pretty

import           Nix.Linter

main :: IO ()
main = do
  args <- getArgs
  for_ args $ \arg -> do
    parsed <- parseNixFileLoc arg
    case parsed of
      Success parse -> for_ (checkAll parse) print
      Failure err   -> putStr "Parse Error: " >> print  err
