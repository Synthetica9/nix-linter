{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Nix.Linter.Checks where

import           Control.Arrow            ((&&&))
import           Data.Char                (isUpper, toLower)
import           Data.Function            ((&))
import           Data.List                (isInfixOf, sortOn)
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Maybe               (fromJust, fromMaybe, maybeToList)
import           Data.Ord                 (Down (..))
import           Data.Text                (Text)


import qualified Data.Set                 as Set

import           Data.Fix
import           Data.Pair

import           Nix.Atoms
import           Nix.Expr.Shorthands
import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated

import           Nix.Linter.Tools
import           Nix.Linter.Types
import           Nix.Linter.Utils         (choose, sorted, (<$$>), (<&>))

varName :: Text
varName = "varName"

checkUnusedLetBinding :: CheckBase
checkUnusedLetBinding warn e = [ (warn UnusedLetBind)
  & setLoc loc
  & note' varName name
  | NLet_ _ binds usedIn <- [unFix e]
  , (bind, others) <- choose binds
  , NamedVar (StaticKey name :| []) _ loc <- [bind]
  , all (noRef name) (values others)
  , name `noRef` usedIn
  ]

checkUnusedArg :: CheckBase
checkUnusedArg warn e = [ warn UnusedArg
  & note' varName name
 | NAbs_ _ params usedIn <- [unFix e]
 , name <- case params of
    Param name           -> [name]
    ParamSet xs _ global -> maybeToList global ++ (fst <$> xs)
  , nonIgnoredName name
  , name `noRef` usedIn
  ]

checkEmptyInherit :: CheckBase
checkEmptyInherit warn e = [ (warn EmptyInherit) {pos=singletonSpan loc}
  | (bindings, _, _) <- [topLevelBinds e]
  , Inherit _ [] loc <- bindings
  ]

checkUnneededRec :: CheckBase
checkUnneededRec warn e = [ warn UnneededRec
  | NSet_ _ann NRecursive binds <- [unFix e]
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
  isSetLiteral e = let (_, _, isLit) = topLevelBinds (Fix e) in isLit

checkUpdateEmptySet :: CheckBase
checkUpdateEmptySet = checkOpBase UpdateEmptySet (Pair (const True) isEmptySetLiteral) NUpdate True where
  isEmptySetLiteral e = let (xs, _, isLit) = topLevelBinds (Fix e) in null xs && isLit

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
checkEtaReduce warn e = [ warn EtaReduce & suggest' xs
  & note' varName x
  | NAbs_ _ (Param x) e' <- [unFix e]
  , NBinary_ _ NApp xs e'' <- [unFix e']
  , NSym_ _ x' <- [unFix e'']
  , x == x'
  , x `noRef` xs
  ]

checkFreeLetInFunc :: CheckBase
checkFreeLetInFunc warn e = [ warn FreeLetInFunc
  & note' varName x
  & suggest (mkLets (stripAnnotation <$$> xs) $ mkFunction (Param x) $ stripAnnotation e'')
  | NAbs_ _ (Param x) e' <- [unFix e]
  , NLet_ _ xs e'' <- [unFix e']
  , all (noRef x) $ values xs
  ]

checkDIYInherit :: CheckBase
checkDIYInherit warn e = [ warn DIYInherit
  & setLoc loc
  & note' varName x
  | (binds, _, _) <- [topLevelBinds e]
  , NamedVar (StaticKey x :| []) e' loc <- binds
  , NSym_ _ x' <- [unFix e']
  , x == x'
  ]

checkLetInInheritRecset :: CheckBase
checkLetInInheritRecset warn e = [ warn LetInInheritRecset
  & note' varName name
  | NLet_ _ binds usedIn <- [unFix e]
  , (inner, outer) <- chooseTrees usedIn
  , NSet_ _ann NRecursive set <- [unFix inner]
  , (this, others) <- choose binds
  , let names = simpleBoundNames this
  , let allNamesFree x = all (`noRef` x) names
  , name <- names
  , plainInherits name set
  , all allNamesFree $ values others
  , allNamesFree outer
  ]

checkEmptyLet :: CheckBase
checkEmptyLet warn e = [ warn EmptyLet & suggest' e'
  | NLet_ _ [] e' <- [unFix e]
  ]

checkUnfortunateArgName :: CheckBase
checkUnfortunateArgName warn e = [ warn UnfortunateArgName
  & note' "now" name & note' "suggested" name'
  | NAbs_ _ (Param name) e' <- [unFix e]
  , (inner, outer) <- chooseTrees e'
  , (bindings, context, _) <- [topLevelBinds inner]
  , NamedVar (StaticKey name' :| []) e'' _ <- bindings
  , name' /= name
  , NSym_ _ name'' <- [unFix e'']
  , name'' == name

  , let valid = not . plainInheritsAnywhere name
  -- These are expensive! Do them last:
  , valid context
  , valid $ Fix $ NSet_ generated NRecursive bindings
  , valid outer
  ]

checkBetaReduction :: CheckBase
checkBetaReduction warn e = [ warn BetaReduction
  | NBinary_ _ NApp e' _ <- [unFix e]
  , NAbs_ _ _ _ <- [unFix e']
  ]

checkAlphabeticalBindings :: CheckBase
checkAlphabeticalBindings warn e = [ warn AlphabeticalBindings
  | (bindings, _, _) <- [topLevelBinds e]
  , not $ sorted $ const () <$$> bindings
  ]

checkAlphabeticalArgs :: CheckBase
checkAlphabeticalArgs warn e = [ warn AlphabeticalArgs
  | NAbs_ _ (ParamSet xs _ _) _ <- [unFix e]
  , not $ sorted $ const () <$$> xs
  ]

checkSequentialLet :: CheckBase
checkSequentialLet warn e = [ warn SequentialLet
  | NLet_ _ _ e' <- [unFix e]
  , NLet_ _ _ _ <- [unFix e']
  ]

checkEmptyVariadicParamSet :: CheckBase
checkEmptyVariadicParamSet warn e = [ warn EmptyVariadicParamSet
  & suggest (Fix $ NAbs (Param $ fromMaybe "_" x) $ stripAnnotation e')
  & note IncreasesGenerality
  | NAbs_ _ (ParamSet [] True x) e' <- [unFix e]
  ]

data AvailableCheck = AvailableCheck
  { defaultEnabled :: Bool
  , category       :: OffenseCategory
  , baseCheck      :: CheckBase
  , description    :: String
  }

enabledCheck, disabledCheck :: OffenseCategory -> CheckBase -> String -> AvailableCheck
enabledCheck = AvailableCheck True
disabledCheck = AvailableCheck False

checks :: [AvailableCheck]
checks = sortOn (Down . defaultEnabled &&& show . category)
  [ enabledCheck UnusedLetBind checkUnusedLetBinding ""
  , enabledCheck UnusedArg checkUnusedArg ""
  , enabledCheck EmptyInherit checkEmptyInherit ""
  , enabledCheck UnneededRec checkUnneededRec ""
  , enabledCheck ListLiteralConcat checkListLiteralConcat ""
  , enabledCheck SetLiteralUpdate checkSetLiteralUpdate ""
  , enabledCheck UpdateEmptySet checkUpdateEmptySet ""
  , disabledCheck UnneededAntiquote checkUnneededAntiquote ""
  , enabledCheck NegateAtom checkNegateAtom ""
  , enabledCheck EtaReduce checkEtaReduce ""
  , enabledCheck FreeLetInFunc checkFreeLetInFunc ""
  , enabledCheck LetInInheritRecset checkLetInInheritRecset ""
  , enabledCheck DIYInherit checkDIYInherit ""
  , enabledCheck EmptyLet checkEmptyLet ""
  , enabledCheck UnfortunateArgName checkUnfortunateArgName ""
  , disabledCheck BetaReduction checkBetaReduction ""
  , disabledCheck AlphabeticalBindings checkAlphabeticalBindings ""
  , disabledCheck AlphabeticalArgs checkAlphabeticalArgs ""
  , enabledCheck SequentialLet checkSequentialLet ""
  , disabledCheck EmptyVariadicParamSet checkEmptyVariadicParamSet ""
  ]

multiChecks :: [(String, Set.Set OffenseCategory)]
multiChecks = Set.fromList <$$>
  [ ("All", category <$> checks)
  , ("Default", category <$> filter defaultEnabled checks)
  , mkMulti "Alphabetical"
  , mkMulti "Unused"
  ] where
    mkMulti s = (toLower <$> s, filter (isInfixOf s . show) $ category <$> checks)

combineChecks :: [CheckBase] -> Check
combineChecks c e = (check <$> c) >>= ($ e)

parseCheckArg :: String -> Either String (Set.Set OffenseCategory -> Set.Set OffenseCategory)
parseCheckArg arg = case filter ((fmap toLower arg ==) . fst) lookupTable of
    []       -> Left $ "No parse: " ++ arg
    [(_, x)] -> Right $ x
    _        -> Left $ "Ambiguous parse: " ++ arg
  where
  sets =  ((show &&& Set.singleton) <$> category <$> checks) ++ multiChecks
  names = conversions =<< sets
  conversions (name, x) = (,x) <$> (fmap toLower <$> ([id, filter isUpper] <*> [name]))
  lookupTable = do
    (name, s) <- names
    (prefix, f) <- [("", Set.union), ("no-", Set.difference)]
    pure (prefix ++ name, flip f s)

checkCategories :: [OffenseCategory] -> Check
checkCategories enabled = let
    lookupTable = (category &&& baseCheck) <$> checks
    getCheck = flip lookup lookupTable
    -- fromJust, because we _want_ to crash when an unknown check shows up,
    -- because that's certainly a bug!
    checks' = fromJust <$> (getCheck <$> enabled)
  in combineChecks checks'
