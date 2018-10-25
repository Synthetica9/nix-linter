{-# LANGUAGE KindSignatures #-}

module Linter.Types where

import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated

import           Text.Megaparsec.Pos      (unPos)

data Offense = Offense OffenseType SrcSpan

prettySourcePos :: SourcePos -> String
prettySourcePos (SourcePos file l c) = file ++ ":" ++ show (unPos l) ++ ":" ++ show (unPos c)

prettySourceSpan :: SrcSpan -> String
prettySourceSpan (SrcSpan pos1@(SourcePos f1 l1 c1) pos2@(SourcePos f2 l2 c2))
  | f1 /= f2 = base ++ prettySourcePos pos2 -- It could happen I guess?
  | l1 /= l2 = base ++ show (unPos l2) ++ ":" ++ show (unPos c2)
  | c1 /= c2 = base ++ show (unPos c2)
    where base = prettySourcePos pos1 ++ "-"

instance Show Offense where
  show (Offense t pos) = show t ++ " at " ++ prettySourceSpan pos

data OffenseType
  = RepetitionWithoutWith
  | UnusedLetBind VarName
  | UnusedArg VarName
  | EmptyInherit
  | UnneededRec
  deriving (Show)
