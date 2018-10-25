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
          in Just offenses
  _ -> Nothing


checkUnusedArg :: CheckBase
checkUnusedArg = \case
  NAbs_ pos params usedIn -> let
    names = filter (not . isPrefixOf "_") $ case params of
       Param name           -> [name]
       ParamSet xs _ global -> maybeToList global ++ (fst <$> xs)
    offenses = [Offense (UnusedArg name) pos | name <- names, not $ hasRef name usedIn]
    in Just offenses
  _ -> Nothing


checkEmptyInherit :: CheckBase
checkEmptyInherit = \case
  NSet_ pos xs -> Just $ xs >>= \case
    Inherit Nothing [] _ -> [Offense EmptyInherit pos]
    _ -> []
  _ -> Nothing


checkUnneededRec :: CheckBase
checkUnneededRec = \case
  NRecSet_ pos binds -> let
      needsRec = choose binds <&> \case
        (bind, others) -> case bind of
          NamedVar (StaticKey name :| []) _ _ -> not $ any (hasRef name) (values others)
          _ -> False
    in Just [Offense UnneededRec pos | not $ or needsRec]
  _ -> Nothing


checkListLiteralConcat :: CheckBase
checkListLiteralConcat = \case
  NBinary_ pos NConcat (Fix (NList_ _ _)) (Fix (NList_ _ _)) ->
    Just [Offense ListLiteralConcat pos]
  _ -> Nothing


checkSetLiteralUpdate :: CheckBase
checkSetLiteralUpdate = \case
  NBinary_ pos NUpdate (Fix (NSet_ _ _)) (Fix (NSet_ _ _)) ->
    Just [Offense SetLiteralUpdate pos]
  _ -> Nothing


checkUpdateEmptySet :: CheckBase
checkUpdateEmptySet = \case
  NBinary_ pos NUpdate (Fix (NSet_ _ xs1)) (Fix (NSet_ _ xs2)) ->
    guard (any null [xs1, xs2]) >> Just [Offense UpdateEmptySet pos]
  _ -> Nothing


-- Works, but the pattern can be useful, so not in the full list of checks.
checkUnneededAntiquote :: CheckBase
checkUnneededAntiquote = \case
  NStr_ pos (DoubleQuoted [Antiquoted _]) ->
    Just [Offense UnneededAntiquote pos]
  _ -> Nothing


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
  ]

check :: CheckBase -> Check
check = collectingPara'

checkAll :: Check
checkAll = check $ mergeCheckBase checks
