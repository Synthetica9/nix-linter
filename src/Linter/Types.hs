{-# LANGUAGE KindSignatures #-}

module Linter.Types where

import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated

import           Text.Megaparsec.Pos      (unPos)

data Offense = Offense OffenseType SourcePos

prettySourcePos :: SourcePos -> String
prettySourcePos (SourcePos file l c) = file ++ ":" ++ show (unPos l) ++ ":" ++ show (unPos c)

instance Show Offense where
  show (Offense t pos) = show t ++ " at " ++ prettySourcePos pos

data OffenseType
  = RepetitionWithoutWith
  | UnusedLetBind VarName
  | EmptyInherit
  deriving (Show)
