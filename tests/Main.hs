{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad.Trans      (liftIO)
import           Data.Foldable            (for_)
import           Data.Function            (on)
import qualified Data.Set                 as Set
import           System.Directory         (listDirectory)
import           System.FilePath          ((</>))

import           Nix.Linter
import           Nix.Linter.Types
import           Nix.Linter.Utils
import           Paths_nix_linter

import           Nix.Expr.Types.Annotated
import           Nix.Parser

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH


case_all_offense_categories_covered :: Assertion
case_all_offense_categories_covered = do
  let available = category <$> checks
      all = [minBound..maxBound] :: [OffenseCategory]
  (assertEqual "" `on` Set.fromList) all available

case_examples_match :: Assertion
case_examples_match = let
  in do
    exampleDir <- liftIO $ getDataFileName "examples"
    examples <- liftIO $ listDirectory exampleDir
    for_ examples $ \example -> do
      let strippedName = takeWhile (/= '.') example
      f <- liftIO $ eitherIO $ parseCheckArg strippedName
      let category = f Set.empty
          check = checkCategories $ Set.toList category

      Success parsed <- parseNixFileLoc $ exampleDir </> example
      let offenses = Set.fromList $ offense <$> check parsed
      assertEqual strippedName offenses category

main = $defaultMainGenerator
