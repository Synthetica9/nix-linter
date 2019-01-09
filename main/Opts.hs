{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Opts where

import           Nix.Linter
import           System.Console.CmdArgs

data NixLinter = NixLinter
  { check       :: [String]
  , json        :: Bool
  , json_stream :: Bool
  , recursive   :: Bool
  , out         :: FilePath
  , files       :: [FilePath]
  , help_for    :: [String]
  } deriving (Show, Data, Typeable)

nixLinter :: NixLinter
nixLinter = NixLinter
  { check = def &= name "W" &= help "checks to enable"
  , json  = def &= help "Use JSON output"
  , json_stream = def &= name "J" &= help "Use a newline-delimited stream of JSON objects instead of a JSON list (implies --json)"
  , recursive = def &= help "Recursively walk given directories (like find)"
  , out = def &= help "File to output to" &= opt "-" &= typFile
  , files = def &= args &= typ "FILES"
  , help_for = def &= typ "CHECKS"
  } &= verbosity &= details (mkChecksHelp Nix.Linter.checks) &= program "nix-linter"

mkChecksHelp :: [AvailableCheck] -> [String]
mkChecksHelp xs = "Available checks:" : (mkDetails <$> xs) where
  mkDetails (AvailableCheck{..}) = "    " ++ show category ++ mkDis defaultEnabled
  mkDis False = " (disabled by default)"
  mkDis _     = ""
