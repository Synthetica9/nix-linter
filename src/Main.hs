{-# LANGUAGE RecordWildCards #-}


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
  for_ args (runCheck checkAll)


runCheck :: Check -> FilePath -> IO ()
runCheck check file = do
    parsed <- parseNixFileLoc file
    case parsed of
      Success parse -> for_ (check parse) $ \x@(Offense {..}) -> do
        let explanation = prettyOffense x
        putStrLn explanation
        case rewrite of
          Nothing -> pure ()
          (Just e) -> do
            putStrLn "Why not:"
            print $ prettyNix e
      Failure err   -> putStr "Parse Error: " >> print err
