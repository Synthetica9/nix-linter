{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-name-shadowing #-}

module Main where

import           Prelude                hiding (log)

import           Control.Arrow          ((&&&), (>>>))
import           Control.Monad          (join)
import           Control.Monad.Trans    (MonadIO, liftIO)
import           Data.Char              (isUpper, toLower)
import           Data.Foldable          (foldMap, for_)
import           Data.Function          ((&))
import           Data.List              (isSuffixOf)
import           Data.Maybe             (fromJust)
import           Path.Internal          (toFilePath)
import           Path.IO                (getCurrentDir, listDir, resolveDir')
import           System.Exit
import           System.IO

import qualified Data.ByteString.Lazy   as B


import           Nix.Parser

import qualified Data.Set               as Set

import           Streamly
import qualified Streamly.Prelude       as S

import           Data.Aeson             (encode)

import           Nix.Linter
import           Nix.Linter.Types
import           Nix.Linter.Utils

import           System.Console.CmdArgs

data NixLinter = NixLinter
  { check       :: [String]
  , json        :: Bool
  , json_stream :: Bool
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
main =  cmdArgs nixLinter >>= runChecks

log :: String -> IO ()
log = hPutStrLn stderr

-- Example from https://hackage.haskell.org/package/streamly
listDirRecursive :: (IsStream t, MonadIO m, MonadIO (t m), Monoid (t m FilePath)) => FilePath -> t m FilePath
listDirRecursive path = resolveDir' path >>= readDir
  where
    readDir dir = do
      (dirs, files) <- listDir dir
      -- liftIO $ mapM_ putStrLn
      --        $ map show dirs ++ map show files
      S.fromList (toFilePath <$> files) `serial` foldMap readDir dirs

parseFiles = S.mapMaybeM $ (\path ->
  parseNixFileLoc path >>= \case
    Success parse -> do
      pure $ Just parse
    Failure why -> do
      liftIO $ whenNormal $ log $ "Failure when parsing:\n" ++ show why
      pure Nothing)

pipeline (NixLinter {..}) combined = let
    exitLog x = S.yieldM . liftIO . const (log x >> exitFailure)
    walk = case (recursive, null files) of
      (False, True) -> exitLog "No files to parse, quitting..."
      (True, False) -> (>>= listDirRecursive) -- Walk a tree
      (True, True) -> exitLog "This will parse the current dir recursivly in the future"
      (False, False) -> id

    printer = if
      | json_stream -> \w -> B.putStr (encode w) >> putStr "\n"
      | json -> B.putStr . encode
      | otherwise -> putStrLn . prettyOffense
  in
    S.fromList files
    & walk
    & S.filter (isSuffixOf ".nix")
    & aheadly . parseFiles
    & aheadly . (S.map (combined >>> S.fromList) >>> join)
    & S.mapM (liftIO . printer)


runChecks :: NixLinter -> IO ()
runChecks (opts@NixLinter{..}) = do
  combined <- getCombined opts

  runStream $ pipeline opts combined
