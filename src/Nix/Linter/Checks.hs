{-# LANGUAGE LambdaCase #-}

module Nix.Linter.Checks where

import           Data.Function            ((&))
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Maybe               (maybeToList)

import           Data.Fix
import           Data.Pair

import           Nix.Atoms
import           Nix.Expr.Shorthands
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated

import           Nix.Linter.Tools
import           Nix.Linter.Types
import           Nix.Linter.Utils         (choose, (<&>))


checkUnusedLetBinding :: CheckBase
checkUnusedLetBinding warn e = [ (warn (UnusedLetBind name)) {pos=singletonSpan loc}
  | NLet_ _ binds usedIn <- [unFix e]
  , (bind, others) <- choose binds
  , NamedVar (StaticKey name :| []) _ loc <- [bind]
  , all (noRef name) (values others)
  , name `noRef` usedIn
  ]

checkUnusedArg :: CheckBase
checkUnusedArg warn e = [ warn (UnusedArg name)
 | NAbs_ _ params usedIn <- [unFix e]
 , name <- case params of
    Param name           -> [name]
    ParamSet xs _ global -> maybeToList global ++ (fst <$> xs)
  , name `noRef` usedIn
  ]

checkEmptyInherit :: CheckBase
checkEmptyInherit warn e = [ (warn EmptyInherit) {pos=singletonSpan loc}
  | (bindings, _) <- [topLevelBinds e]
  , Inherit _ [] loc <- bindings
  ]

checkUnneededRec :: CheckBase
checkUnneededRec warn e = [ warn UnneededRec
  | NRecSet_ _ binds <- [unFix e]
  , not $ or $ choose binds <&> \case
    (bind, others) -> case bind of
      NamedVar (StaticKey name :| []) _ _ -> all (noRef name) (values others)
      _                                   -> False
  ]

checkOpBase :: OffenseCategory -> Pair (UnwrappedNExprLoc -> Bool) -> NBinaryOp -> Bool -> CheckBase
checkOpBase ot (Pair p1 p2) op reflexive warn e = [ warn ot
  | NBinary_ _ op' (Fix e2) (Fix e1) <- [unFix e]
  , p1 e1 && p2 e2 || p1 e2 && p2 e1 && reflexive
  , op == op'
  ]

checkSymmetricOpBase :: OffenseCategory -> (UnwrappedNExprLoc -> Bool) -> NBinaryOp -> CheckBase
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
checkUnneededAntiquote warn e = [ warn UnneededAntiquote
  | NStr_ _ (DoubleQuoted [Antiquoted _]) <- [unFix e]
  ]

checkNegateAtom :: CheckBase
checkNegateAtom warn e= [warn NegateAtom & suggest (mkBool $ not b)
  | NUnary_ _ NNot e' <- [unFix e]
  , NConstant_ _ (NBool b) <- [unFix e']
  ]

checkEtaReduce :: CheckBase
checkEtaReduce warn e = [ warn (EtaReduce x)
  | NAbs_ _ (Param x) e' <- [unFix e]
  , NBinary_ _ NApp xs e'' <- [unFix e']
  , NSym_ _ x' <- [unFix e'']
  , x == x'
  , x `noRef` xs
  ]

checkFreeLetInFunc :: CheckBase
checkFreeLetInFunc warn e = [ (warn (FreeLetInFunc x)) {pos=getPos e'}
  | NAbs_ _ (Param x) e' <- [unFix e]
  , NLet_ _ xs _ <- [unFix e']
  , all (noRef x) $ values xs
  ]

checkDIYInherit :: CheckBase
checkDIYInherit warn e = [ (warn $ DIYInherit x) { pos=singletonSpan loc}
  | (binds, _) <- [topLevelBinds e]
  , NamedVar (StaticKey x :| []) e' loc <- binds
  , NSym_ _ x' <- [unFix e']
  , x == x'
  ]

checkLetInInheritRecset :: CheckBase
checkLetInInheritRecset warn e = [(warn $ LetInInheritRecset name)
  | NLet_ _ binds usedIn <- [unFix e]
  , (inner, outer) <- chooseTrees usedIn
  , NRecSet_ _ set <- [unFix inner]
  , (this, others) <- choose binds
  , let names = simpleBoundNames this
  , let allNamesFree x = all (`noRef` x) names
  , name <- names
  , plainInherits name set
  , all allNamesFree $ values others
  , allNamesFree outer
  ]

checks :: [CheckBase]
checks =
  [ checkUnusedLetBinding
  , checkUnusedArg
  , checkEmptyInherit
  , checkUnneededRec
  , checkListLiteralConcat
  , checkSetLiteralUpdate
  , checkUpdateEmptySet
  -- , checkUnneededAntiquote
  , checkNegateAtom
  , checkEtaReduce
  , checkFreeLetInFunc
  , checkLetInInheritRecset
  , checkDIYInherit
  ]

checkAll :: Check
checkAll e = (check <$> checks) >>= ($ e)
