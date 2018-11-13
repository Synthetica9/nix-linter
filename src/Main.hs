module Main where

import           Data.Foldable
import           System.Environment

import           Nix.Parser
import           Nix.Pretty

import           Nix.Linter
import           Nix.Linter.Types

main :: IO ()
main = do
  args <- getArgs
  for_ args $ \arg -> do
    parsed <- parseNixFileLoc arg
    case parsed of
      Success parse -> for_ (checkAll parse) (putStrLn . prettyOffense)
      Failure err   -> putStr "Parse Error: " >> print  err
