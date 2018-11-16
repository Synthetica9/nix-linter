{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}


module Main where

import           Prelude                hiding (log)

import           Control.Arrow          ((&&&))
import           Data.Foldable          (toList, traverse_)
import           Data.Function          ((&))
import           Data.Maybe             (catMaybes)
import           Data.Traversable       (for)
import           System.Exit
import           System.IO              (hPutStrLn, isEOF, stderr)
import           System.IO.Unsafe       (unsafeInterleaveIO)

import qualified Data.ByteString.Lazy   as B


import           Nix.Parser

import qualified Data.Set               as S

import           Data.Aeson             (encode)

import           Nix.Linter
import           Nix.Linter.Types
import           Nix.Linter.Utils

import           System.Console.CmdArgs

data NixLinter = NixLinter
  {
    check       :: [OffenseCategory]
  , noCheck     :: [OffenseCategory]
  , json        :: Bool
  , json_stream :: Bool
  , file_list   :: Bool
  , out         :: FilePath
  , files       :: [FilePath]
  } deriving (Show, Data, Typeable)

nixLinter :: NixLinter
nixLinter = NixLinter
  {
    check = def &= help "checks to enable"
  , noCheck = def &= help "checks to disable"
  , json  = def &= help "Use JSON output"
  , json_stream = def &= name "J" &= help "Use a newline-delimited stream of JSON objects instead of a JSON list (implies --json)"
  , file_list = def &= help "Read files to process (like xargs)"
  , out = def &= help "File to output to" &= typ "FILE"
  , files = def &= args &= typ "FILES"
  } &= verbosity &= details (mkChecksHelp Nix.Linter.checks)

getChecks :: [AvailableCheck] -> NixLinter -> Either String Check
getChecks avail (NixLinter{..}) = let
    defaults = S.fromList $ category <$> filter defaultEnabled avail
    explitEnabled = S.fromList check
    explitDisabled = S.fromList noCheck
    conflicting = S.intersection explitEnabled explitDisabled
    finalEnabled = defaults `S.union` explitDisabled `S.difference` explitDisabled
    lookupTable = (category &&& baseCheck) <$> avail
    getCheck = flip lookup lookupTable
    checks = catMaybes (getCheck <$> toList finalEnabled)
  in if conflicting /= S.empty
    then Left $ "Checks both enabled and disabled: " ++ show conflicting
    else Right $ combineChecks checks


mkChecksHelp :: [AvailableCheck] -> [String]
mkChecksHelp xs = "Available checks:" : (mkDetails <$> xs) where
  mkDetails (AvailableCheck{..}) = "    " ++ show category ++ mkDis defaultEnabled
  mkDis False = " (disabled by default)"
  mkDis _     = ""

main :: IO ()
main =  cmdArgs nixLinter >>= runChecks

log :: String -> IO ()
log = hPutStrLn stderr

stdinContents :: IO [String]
stdinContents = do
  eof <- isEOF
  if eof
    then pure []
    else (:) <$> getLine <*> (unsafeInterleaveIO $ stdinContents)

runChecks :: NixLinter -> IO ()
runChecks (opts@NixLinter{..}) = do
  let enabledChecks = getChecks checks opts
  combined <- case enabledChecks of
    Left err -> fail err
    Right cs -> pure cs

  let noFiles = null files
  files' <- if file_list
    then
      if noFiles
        then stdinContents
        else (concat :: [[String]] -> [FilePath]) <$> lines <$$> for files readFile
    else
      if noFiles
        then (whenNormal $ log $ "No files to parse.") >> exitFailure
        else pure files

  (results :: [Offense]) <- fmap concat $ for files' $ \f -> unsafeInterleaveIO $ do
    whenLoud $ log $ "Parsing file... " ++ f
    parsed <- parseNixFileLoc f
    case parsed of
      Success result -> pure $ combined result
      Failure why    -> (log $ "Parse failed: \n" ++ show why) >> pure []

  results & if json_stream
    then traverse_ $ \w -> B.putStr (encode w) >> putStr "\n"
    else if json
      then B.putStr . encode
      else traverse_ (putStrLn . prettyOffense)
