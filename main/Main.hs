{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-name-shadowing #-}

module Main where

import           Prelude                hiding (log, readFile)

import           Control.Arrow          ((>>>))
import           Control.Monad          (join, when)
import           Control.Monad.Trans    (MonadIO, liftIO)
import           Data.Foldable          (for_)
import           Data.Function          ((&))
import           Data.IORef
import           Data.List              (isSuffixOf, sortOn)
import           Data.Text              (Text)
import qualified Data.Text              as T

import           Data.Text.IO

import           Path.Internal          (toFilePath)
import           Path.IO                (listDir, resolveDir')
import           System.Console.Pretty
import           System.Exit
import           System.IO              (IOMode (..), stderr, stdout, withFile)

import qualified Data.ByteString.Lazy   as B

import           Nix.Parser

import qualified Data.Set               as Set

import           Streamly
import           Streamly.Prelude       ((.:))
import qualified Streamly.Prelude       as S

import           Data.Aeson             (encode)

import           Opts

import           Nix.Linter
import           Nix.Linter.Types
import           Nix.Linter.Utils

import           Paths_nix_linter

import           System.Console.CmdArgs

getChecks :: [OffenseCategory] -> [String] -> Either [String] [OffenseCategory]
getChecks defaults' check = let
    defaults = Set.fromList $ defaults'
    parsedArgs = sequenceEither $ parseCheckArg <$> check
    categories = (\fs -> foldl (flip ($)) defaults fs) <$> parsedArgs
  in Set.toList <$> categories

getCombined :: [String] -> IO Check
getCombined check = do
  let defaults = category <$> filter defaultEnabled checks
  enabled <- case getChecks defaults check of
    Right cs -> pure cs
    Left err -> do
      for_ err print
      exitFailure

  whenLoud $ do
    log "Enabled checks:"
    if null enabled
      then log "  (None)"
      else for_ enabled $ \check -> do
        log $ "- " <> pShow check

  pure $ checkCategories enabled

main :: IO ()
main =  cmdArgs nixLinter >>= runChecks

log :: Text -> IO ()
log = hPutStrLn stderr

-- Example from https://hackage.haskell.org/package/streamly
listDirRecursive :: (IsStream t, MonadIO m, MonadIO (t m), Monoid (t m FilePath)) => FilePath -> t m FilePath
listDirRecursive path = resolveDir' path >>= readDir
  where
    readDir dir = do
      (dirs, files) <- listDir dir
      S.fromList (toFilePath <$> files) `serial` foldMap readDir dirs

parseFiles = S.mapMaybeM $ (\path ->
  parseNixFileLoc path >>= \case
    Right parse -> do
      pure $ Just parse
    Left why -> do
      liftIO $ whenNormal $ log $ "Failure when parsing:\n" <> pShow why
      pure Nothing)

pipeline (NixLinter {..}) combined = let
    exitLog x = S.yieldM . liftIO . const (log x >> exitFailure)
    walker = if recursive
      then (>>= listDirRecursive)
      else id

    walk = case (recursive, null files) of
      (False, True) -> exitLog "No files to parse, quitting..."
      (True, True)  -> ("." .:) >>> walker
      (_, _)        -> walker

  in
    S.fromList files
    & walk
    & S.filter ((recursive -->) <$> isSuffixOf ".nix")
    & S.map (\p -> if p == "-" then "/dev/stdin" else p)
    & aheadly . parseFiles
    & aheadly . (S.map (combined >>> S.fromList) >>> join)

extraHelp :: OffenseCategory -> IO ()
extraHelp cat = do
  log $ "-W " <> (color Blue $ style Bold $ pShow cat) <> "\n"

  example <- getDataFileName ("examples/" <> show cat <> ".nix")
  mainExample <- readFile example
  let indented = T.unlines $ ("    " <>) <$> T.lines mainExample
  log $ indented <> "\n"

runChecks :: NixLinter -> IO ()
runChecks (opts@NixLinter{..}) = do
  when (not $ null help_for) $ do
    cats <- case (getChecks [] help_for) of
      Left err -> do
        for_ err (log . T.pack)
        exitFailure
      Right xs -> pure xs
    mapM_ extraHelp (sortOn show cats)
    exitSuccess

  combined <- getCombined check

  let withOutHandle = if null out
      then ($ stdout)
      else withFile out WriteMode

      printer = \handle -> if
        | json_stream -> \w -> B.hPut handle (encode w) >> hPutStr handle "\n"
        | json -> B.hPutStr handle . encode
        | otherwise -> hPutStrLn handle . prettyOffense

      results = pipeline opts combined

  -- We can't use the naive implementation here, because that opens the files
  -- multiple times
  withOutHandle $ \handle -> do
    hasIssues <- newIORef False
    S.drain $ flip S.mapM results $ \result -> do
      printer handle result
      -- "Smuggle" the result out of the Streamly datatype
      writeIORef hasIssues True

    hadIssues <- readIORef hasIssues
    if hadIssues then exitFailure else exitSuccess
