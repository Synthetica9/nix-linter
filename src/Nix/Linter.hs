{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nix.Linter (combineChecks, checks, AvailableCheck(..), multiChecks) where

import           Nix.Linter.Checks
