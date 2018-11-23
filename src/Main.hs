{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


{-# OPTIONS_GHC -Wno-name-shadowing #-}


module Main where

import           Prelude                              hiding (log)

import           Control.Arrow                        ((&&&))
import           Control.Concurrent.ParallelIO.Global
import           Data.Char                            (isUpper, toLower)
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

import qualified Data.Set                             as Set

import           Data.Aeson                           (encode)

import           Nix.Linter
import           Nix.Linter.Types
import           Nix.Linter.Utils

import           System.Console.CmdArgs

data NixLinter = NixLinter
  { check       :: [String]
  , json        :: Bool
  , json_stream :: Bool
  , file_list   :: Bool
  , recursive   :: Bool
  , out         :: FilePath
  , files       :: [FilePath]
  } deriving (Show, Data, Typeable)

nixLinter :: NixLinter
nixLinter = NixLinter
  { check = def &= name "W" &= help "checks to enable"
  , json  = def &= help "Use JSON output"
  , json_stream = def &= name "J" &= help "Use a newline-delimited stream of JSON objects instead of a JSON list (implies --json)"
  , recursive = def &= help "Recursively walk given directories (like find)"
  , file_list = def &= help "Read files to process (like xargs)"
  , out = def &= help "File to output to" &= typFile
  , files = def &= args &= typ "FILES"
  } &= verbosity &= details (mkChecksHelp Nix.Linter.checks)

parseCheckArg :: String -> Either String (Set.Set OffenseCategory -> Set.Set OffenseCategory)
parseCheckArg arg = case filter ((fmap toLower arg ==) . fst) lookupTable of
    []       -> Left $ "No parse: " ++ arg
    [(_, x)] -> Right $ x
    _        -> Left $ "Ambiguous parse: " ++ arg
  where
  sets =  ((show &&& Set.singleton) <$> category <$> checks) ++ multiChecks
  names = conversions =<< sets
  conversions (name, x) = (,x) <$> (fmap toLower <$> ([id, filter isUpper] <*> [name]))
  lookupTable = do
    (name, s) <- names
    (prefix, f) <- [("", Set.union), ("no-", Set.difference)]
    pure (prefix ++ name, flip f s)

getChecks :: NixLinter -> Either [String] [OffenseCategory]
getChecks (NixLinter {..}) = let
    defaults = Set.fromList $ category <$> filter defaultEnabled checks
    parsedArgs = sequenceEither $ parseCheckArg <$> check
    categories = (\fs -> foldl (flip ($)) defaults fs) <$> parsedArgs
  in Set.toList <$> categories

checkCategories :: [OffenseCategory] -> Check
checkCategories enabled = let
    lookupTable = (category &&& baseCheck) <$> checks
    getCheck = flip lookup lookupTable
    -- fromJust, because we _want_ to crash when an unknown check shows up,
    -- because that's certainly a bug!
    checks' = fromJust <$> (getCheck <$> enabled)
  in combineChecks checks'

getCombined :: NixLinter -> IO Check
getCombined opts = do
  enabled <- case getChecks opts of
    Right cs -> pure cs
    Left err -> do
      for_ err print
      exitFailure

  whenLoud $ do
    log "Enabled checks:"
    if null enabled
      then log "  (None)"
      else for_ enabled $ \check -> do
        log $ "- " ++ show check

  pure $ checkCategories enabled

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
  combined <- getCombined opts

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

  files' :: [FilePath] <- if not recursive
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
