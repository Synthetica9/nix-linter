{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Control.Monad.Trans      (liftIO)
import           Data.Char                (toLower)
import           Data.Foldable            (for_)
import           Data.Function            (on)
import           Data.Set                 ((\\))
import qualified Data.Set                 as Set
import           Data.Traversable         (for)
import           System.Directory         (doesFileExist, listDirectory)
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


stripExtension :: FilePath -> String
stripExtension = takeWhile (/= '.')

parseCategory name = do
  f <- eitherIO $ parseCheckArg name
  pure (f Set.empty)

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
      let strippedName = stripExtension example
      category <- parseCategory strippedName
      let check = checkCategories $ Set.toList category

      parsed <- parseNixFileLoc (exampleDir </> example) >>= \case
        Success x   -> pure x
        Failure err -> assertFailure (show err)

      let offenses = Set.fromList $ offense <$> check parsed
      assertEqual strippedName offenses category

case_all_categories_have_example :: Assertion
case_all_categories_have_example =
  do
    let all = [minBound..maxBound] :: [OffenseCategory]
    exampleDir <- liftIO $ getDataFileName "examples"
    diff <- concat <$$> for all $ \cat -> do
      let path = exampleDir <> "/" <> show cat <> ".nix"
      exists <- doesFileExist path
      pure $ if exists then [] else [cat]
    assertBool ("Missing: " ++ show diff) (null diff)

main = $defaultMainGenerator
