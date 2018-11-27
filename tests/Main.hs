{-# LANGUAGE TemplateHaskell #-}

import           Data.Function    (on)
import qualified Data.Set         as Set

import           Nix.Linter
import           Nix.Linter.Types

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH


case_all_offense_categories_covered :: Assertion
case_all_offense_categories_covered = do
  let available = category <$> checks
      all = [minBound..maxBound] :: [OffenseCategory]
  (assertEqual "" `on` Set.fromList) all available



main = $defaultMainGenerator
