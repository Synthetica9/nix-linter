{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where

import           Data.Foldable
import           System.Environment

import           Nix.Parser
import           Nix.Pretty

import           Nix.Linter
import           Nix.Linter.Types

import           System.Console.CmdArgs

data NixLinter = NixLinter
  {
    checks    :: [(String, Bool)]
  , json      :: Bool
  , file_list :: Bool
  , out       :: FilePath
  , files     :: [FilePath]
  } deriving (Show, Data, Typeable)


nixLinter = NixLinter
  {
    checks = def
  , json  = def &= help "Use JSON output"
  , file_list = def &= help "Read files to process (like xargs)"
  , out = def &= help "File to output to" &= typ "FILE"
  , files = def &= args &= typ "FILES"
  } &= verbosity &= details (mkChecksHelp Nix.Linter.checks)

mkChecksHelp :: [AvailableCheck] -> [String]
mkChecksHelp xs = "Available checks:" : (mkDetails <$> xs) where
  mkDetails (AvailableCheck{..}) = "    " ++ name ++ mkDis defaultEnabled
  mkDis False = " (disabled by default)"
  mkDis _     = ""

main = print =<< cmdArgs nixLinter
-- main :: IO ()
-- main = do
--   args <- getArgs
--   for_ args (runCheck checkAll)



runCheck :: Check -> FilePath -> IO ()
runCheck c file = do
    parsed <- parseNixFileLoc file
    case parsed of
      Success parse -> for_ (c parse) $ \x@(Offense {..}) -> do
        let explanation = prettyOffense x
        putStrLn explanation
        case rewrite of
          Nothing -> pure ()
          (Just e) -> do
            putStrLn "Why not:"
            print $ prettyNix e
      Failure err   -> putStr "Parse Error: " >> print err
