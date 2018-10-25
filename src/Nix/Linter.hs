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
hasRef name t = member name $ (freeVars t)


values :: [Binding r] -> [r]
values = (f =<<)  where
  f (NamedVar _ x _) = [x]
  f _                = []


checkUnusedLetBinding :: Check
checkUnusedLetBinding = collectingPara' $ \case
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


checkUnusedArg :: Check
checkUnusedArg = collectingPara' $ \case
  NAbs_ pos params usedIn -> let
    names = filter (not . isPrefixOf "_") $ case params of
       Param name           -> [name]
       ParamSet xs _ global -> maybeToList global ++ (fst <$> xs)
    offenses = [Offense (UnusedArg name) pos | name <- names, not $ hasRef name usedIn]
    in Just offenses
  _ -> Nothing


checkEmptyInherit :: Check
checkEmptyInherit = collectingPara' $ \case
  NSet_ pos xs -> Just $ xs >>= \case
    Inherit Nothing [] _ -> [Offense EmptyInherit pos]
    _ -> []
  _ -> Nothing


checkUnneededRec :: Check
checkUnneededRec = collectingPara' $ \case
  NRecSet_ pos binds -> let
      needsRec = choose binds <&> \case
        (bind, others) -> case bind of
          NamedVar (StaticKey name :| []) _ _ -> not $ any (hasRef name) (values others)
          _ -> False
      newOffenses = [Offense UnneededRec pos | not $ or needsRec]
    in Just newOffenses
  _ -> Nothing


checkListLiteralConcat :: Check
checkListLiteralConcat = collectingPara' $ \case
  NBinary_ pos NConcat (Fix (NList_ _ _)) (Fix (NList_ _ _)) ->
    Just [Offense ListLiteralConcat pos]
  _ -> Nothing


checks :: [Check]
checks =
  [ checkUnneededRec
  , checkEmptyInherit
  , checkUnusedArg
  , checkUnusedLetBinding
  , checkListLiteralConcat
  ]

checkAll :: Check
checkAll x = ($ x) =<< checks
