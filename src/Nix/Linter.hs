{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nix.Linter (combineChecks, checks, AvailableCheck(..)) where

import           Nix.Linter.Checks
