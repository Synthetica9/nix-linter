{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nix.Linter where

import           Control.Monad
import           Data.Fix
import           Data.Foldable            (fold)
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Maybe
import           Data.Set                 (member)
import           Data.Text                (isPrefixOf, pack)

import           Data.Pair

import           Nix.Atoms
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.TH                   (freeVars)

import           Nix.Linter.Morphisms
import           Nix.Linter.Types
import           Nix.Linter.Utils

maximumRepetitionsWithoutWith = 3

hasRef :: VarName -> NExprLoc -> Bool
hasRef name t = member name $ freeVars t


values :: [Binding r] -> [r]
values = (f =<<)  where
  f (NamedVar _ x _) = [x]
  f _                = []


checkUnusedLetBinding :: CheckBase
checkUnusedLetBinding = \case
  (NLet_ loc binds usedIn) -> let
    offenses = choose binds >>= \case
      (bind, others) -> case bind of
        NamedVar (StaticKey name :| []) _ _ -> [
          Offense (UnusedLetBind name) loc
            | not $ any (hasRef name) (values others)
            , not $ hasRef name usedIn]
        _ -> []
          in offenses
  _ -> []


checkUnusedArg :: CheckBase
checkUnusedArg = \case
  NAbs_ pos params usedIn -> let
    names = filter (not . isPrefixOf "_") $ case params of
       Param name           -> [name]
       ParamSet xs _ global -> maybeToList global ++ (fst <$> xs)
    offenses = [Offense (UnusedArg name) pos | name <- names, not $ hasRef name usedIn]
    in offenses
  _ -> []


checkEmptyInherit :: CheckBase
checkEmptyInherit = \case
  NSet_ pos xs -> xs >>= \case
    Inherit Nothing [] _ -> [Offense EmptyInherit pos]
    _ -> []
  _ -> []


checkUnneededRec :: CheckBase
checkUnneededRec = \case
  NRecSet_ pos binds -> let
      needsRec = choose binds <&> \case
        (bind, others) -> case bind of
          NamedVar (StaticKey name :| []) _ _ -> not $ any (hasRef name) (values others)
          _ -> False
    in [Offense UnneededRec pos | not $ or needsRec]
  _ -> []

checkOpBase :: OffenseType -> Pair (UnwrappedNExprLoc -> Bool) -> NBinaryOp -> Bool -> CheckBase
checkOpBase ot (Pair p1 p2) op reflexive = \case
  NBinary_ pos op' (Fix e2) (Fix e1) ->
    [Offense ot pos | p1 e1 && p2 e2 || p1 e2 && p2 e1 && reflexive, op == op']
  _ -> []


checkSymmetricOpBase :: OffenseType -> (UnwrappedNExprLoc -> Bool) -> NBinaryOp -> CheckBase
checkSymmetricOpBase ot p op = checkOpBase ot (dup p) op False


checkListLiteralConcat :: CheckBase
checkListLiteralConcat = checkSymmetricOpBase ListLiteralConcat isListLiteral NConcat where
  isListLiteral = \case
    NList_ _ _ -> True
    _ -> False


checkSetLiteralUpdate :: CheckBase
checkSetLiteralUpdate = checkSymmetricOpBase SetLiteralUpdate isSetLiteral NUpdate where
  isSetLiteral = \case
    NSet_ _ _ -> True
    NRecSet_ _ _ -> True
    _ -> False


checkUpdateEmptySet :: CheckBase
checkUpdateEmptySet = checkOpBase UpdateEmptySet (Pair (const True) isEmptySetLiteral) NUpdate True where
  isEmptySetLiteral = \case
    NSet_ _ [] -> True
    NRecSet_ _ [] -> True
    _ -> False


-- Works, but the pattern can be useful, so not in the full list of checks.
checkUnneededAntiquote :: CheckBase
checkUnneededAntiquote = \case
  NStr_ pos (DoubleQuoted [Antiquoted _]) ->
    [Offense UnneededAntiquote pos]
  _ -> []

checkNegateAtom :: CheckBase
checkNegateAtom = \case
  NUnary_ pos NNot (Fix (NConstant_ _ (NBool _))) -> [Offense NegateAtom pos]
  _ -> []

checks :: [CheckBase]
checks =
  [ checkUnneededRec
  , checkEmptyInherit
  , checkUnusedArg
  , checkUnusedLetBinding
  , checkListLiteralConcat
  , checkSetLiteralUpdate
  , checkUpdateEmptySet
  -- , checkUnneededAntiquote
  , checkNegateAtom
  ]

check :: CheckBase -> Check
check = collectingPara

checkAll :: Check
checkAll = check $ mergeCheckBase checks
