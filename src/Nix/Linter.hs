{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nix.Linter ( combineChecks, checks, AvailableCheck(..), multiChecks
                  , parseCheckArg, checkCategories) where

import           Nix.Linter.Checks
