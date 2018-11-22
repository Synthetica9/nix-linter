{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}


module Main where

import           Prelude                              hiding (log)

import           Control.Arrow                        ((&&&))
import           Control.Concurrent.ParallelIO.Global
import           Data.Foldable                        (for_, toList, traverse_)
import           Data.Function                        ((&))
import           Data.List                            (isSuffixOf)
import           Data.Maybe                           (fromJust)
import           Data.Traversable                     (for)
import           System.Directory.Tree                (readDirectoryWithL,
                                                       zipPaths)
import           System.Exit
import           System.IO
import           System.IO.Unsafe                     (unsafeInterleaveIO)

import qualified Data.ByteString.Lazy                 as B


import           Nix.Parser

import qualified Data.Set                             as S

import           Data.Aeson                           (encode)

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
  , walk        :: Bool
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
  , walk = def &= help "Walk given directories (like find)"
  , file_list = def &= help "Read files to process (like xargs)"
  , out = def &= help "File to output to" &= typFile
  , files = def &= args &= typ "FILES"
  } &= verbosity &= details (mkChecksHelp Nix.Linter.checks)

getChecks :: [AvailableCheck] -> NixLinter -> Either String [OffenseCategory]
getChecks avail (NixLinter{..}) = let
    defaults = S.fromList $ category <$> filter defaultEnabled avail
    explitEnabled = S.fromList check
    explitDisabled = S.fromList noCheck
    conflicting = S.intersection explitEnabled explitDisabled
    finalEnabled = defaults `S.union` explitEnabled `S.difference` explitDisabled
    checks = toList finalEnabled
  in if conflicting /= S.empty
    then Left $ "Checks both enabled and disabled: " ++ show conflicting
    else Right $ checks

checkCategories :: [AvailableCheck] -> [OffenseCategory] -> Check
checkCategories avail enabled = let
    lookupTable = (category &&& baseCheck) <$> avail
    getCheck = flip lookup lookupTable
    -- fromJust, because we _want_ to crash when an unknown check shows up,
    -- because that's certainly a bug!
    checks = fromJust <$> (getCheck <$> toList enabled)
  in combineChecks checks

mkChecksHelp :: [AvailableCheck] -> [String]
mkChecksHelp xs = "Available checks:" : (mkDetails <$> xs) where
  mkDetails (AvailableCheck{..}) = "    " ++ show category ++ mkDis defaultEnabled
  mkDis False = " (disabled by default)"
  mkDis _     = ""

main :: IO ()
main =  cmdArgs nixLinter >>= runChecks >> stopGlobalPool

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
  enabled <- case getChecks checks opts of
    Left err -> fail err
    Right cs -> pure cs

  whenLoud $ do
    log "Enabled checks:"
    for_ enabled $ \check -> do
      log $ "- " ++ show check

  let combined = checkCategories checks enabled

  let noFiles = null files
  paths <- if file_list
    then
      if noFiles
        then stdinContents
        else (concat :: [[String]] -> [FilePath]) <$> lines <$$> for files readFile
    else
      if noFiles
        then (whenNormal $ log $ "No files to parse.") >> exitFailure
        else pure files

  files' :: [FilePath] <- if not walk
    then pure paths
    else
      fmap (filter (isSuffixOf ".nix") . concat . fmap (toList . fmap fst . zipPaths))
      $ for paths $ readDirectoryWithL $ const $ pure ()

  (results :: [Offense]) <- fmap concat $ parallel $ files' <&> \f -> unsafeInterleaveIO $ do
    whenLoud $ log $ "Parsing file... " ++ f
    parsed <- extraWorkerWhileBlocked $ parseNixFileLoc f
    case parsed of
      Success result -> pure $ combined result
      Failure why    -> (log $ "Parse failed: \n" ++ show why) >> pure []

  let withOut = if null out
      then ($ stdout)
      else \action -> do
        whenLoud $ log $ "Opening file " ++ out
        withFile out WriteMode action

  withOut $ \handle -> results & if json_stream
    then traverse_ $ \w -> B.hPut handle (encode w) >> hPutStr handle "\n"
    else if json
      then B.hPut handle . encode
      else traverse_ (hPutStrLn handle . prettyOffense)
