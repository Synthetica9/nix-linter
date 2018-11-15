{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nix.Linter (checkAll, checks, AvailableCheck(..)) where

import           Nix.Linter.Checks
