{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nix.Linter where

import           Control.Monad
import           Data.Fix
import           Data.Foldable            (fold)
import           Data.List                (find)
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
import           Nix.Linter.Traversals
import           Nix.Linter.Types
import           Nix.Linter.Utils

maximumRepetitionsWithoutWith = 3

hasRef, noRef :: VarName -> NExprLoc -> Bool
hasRef name t = member name $ freeVars t

noRef = not ... hasRef

getFreeVarName :: NExprLoc -> VarName
getFreeVarName x = let
    candidates = pack . ("_freeVar" ++) . show <$> [1..]
    -- We are guarranteed to find a good candidate, because candidates is
    -- infinite and x is strict
    Just var = find (not . (`member` freeVars x)) candidates
  in var

getFreeVar :: NExprLoc -> NExprLoc
getFreeVar = Fix . NSym_ generated . getFreeVarName

topLevelBinds :: NExprLoc -> ([Binding NExprLoc], NExprLoc)
topLevelBinds e = case unFix e of
  -- `let x = 1; y = x; in y` is valid, so e is the context!
  NLet_    _ xs _ -> (xs, e)
  NRecSet_ _ xs   -> (xs, e)
  -- Nonrecursive, so no context. We make up a context that can't possibly be valid.
  NSet_    _ xs   -> (xs, getFreeVar e)
  -- Otherwise, our context is just empty!
  _               -> ([], e)

generatedPos :: SourcePos
generatedPos = let z = mkPos 1 in SourcePos "<generated!>" z z

generated :: SrcSpan
generated = join SrcSpan generatedPos

chooseTrees :: NExprLoc -> [(NExprLoc, NExprLoc)]
chooseTrees e = do
  (inner, outer) <- contextList e
  pure (inner, outer $ getFreeVar e)

values :: [Binding r] -> [r]
values = (f =<<)  where
  f (NamedVar _ x _) = [x]
  f _                = []


checkUnusedLetBinding :: CheckBase
checkUnusedLetBinding = \case
  (NLet_ loc binds usedIn) ->
    choose binds >>= \case
      (bind, others) -> case bind of
        NamedVar (StaticKey name :| []) _ _ -> [
            UnusedLetBind name
            | all (noRef name) (values others)
            , name `noRef` usedIn]
        _ -> []
  _ -> []


checkUnusedArg :: CheckBase
checkUnusedArg = \case
  NAbs_ _ params usedIn -> let
    names = filter (not . isPrefixOf "_") $ case params of
       Param name           -> [name]
       ParamSet xs _ global -> maybeToList global ++ (fst <$> xs)
    in [UnusedArg name | name <- names, name `noRef` usedIn]
  _ -> []


checkEmptyInherit :: CheckBase
checkEmptyInherit = \case
  NSet_ _ xs -> xs >>= \case
    Inherit _ [] _ -> [EmptyInherit]
    _ -> []
  _ -> []

checkUnneededRec :: CheckBase
checkUnneededRec = \case
  NRecSet_ _ binds -> let
      needsRec = choose binds <&> \case
        (bind, others) -> case bind of
          NamedVar (StaticKey name :| []) _ _ -> all (noRef name) (values others)
          _ -> False
    in [UnneededRec | not $ or needsRec]
  _ -> []

checkOpBase :: OffenseType -> Pair (UnwrappedNExprLoc -> Bool) -> NBinaryOp -> Bool -> CheckBase
checkOpBase ot (Pair p1 p2) op reflexive = \case
  NBinary_ _ op' (Fix e2) (Fix e1) ->
    [ot | p1 e1 && p2 e2 || p1 e2 && p2 e1 && reflexive, op == op']
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
  NStr_ _ (DoubleQuoted [Antiquoted _]) ->
    [UnneededAntiquote]
  _ -> []

checkNegateAtom :: CheckBase
checkNegateAtom = \case
  NUnary_ _ NNot (Fix (NConstant_ _ (NBool _))) -> [NegateAtom]
  _ -> []

checkEtaReduce :: CheckBase
checkEtaReduce = \case
  NAbs_ _ (Param x) (Fix (NBinary_ _ NApp xs (Fix (NSym_ _ x')))) ->
    [EtaReduce x | x == x', x `noRef` xs]
  _ -> []

checkFreeLetInFunc :: CheckBase
checkFreeLetInFunc = \case
  NAbs_ _ (Param x) (Fix (NLet_ _ xs _)) -> [FreeLetInFunc x | all (noRef x) $ values xs]
  _ -> []

staticKeys :: [NKeyName x] -> [VarName]
staticKeys xs = do
  StaticKey x <- xs
  pure x

simpleBoundNames :: Binding x -> [VarName]
simpleBoundNames (NamedVar (StaticKey x :| []) _ _) = [x]
simpleBoundNames (Inherit _ xs _)                   = staticKeys xs

plainInherits :: VarName -> [Binding x] -> Bool
plainInherits x xs = or $ do
  Inherit Nothing ys _ <- xs
  pure $ x `elem` staticKeys ys

checkDIYInherit :: CheckBase
checkDIYInherit x = (fst . topLevelBinds $ Fix x) >>= \case
  (NamedVar (StaticKey x :| []) (Fix (NSym_ _ x')) _) -> [DIYInherit x | x == x']
  _ -> []

checkLetInInheritRecset :: CheckBase
checkLetInInheritRecset = \case
  NLet_ _ binds usedIn -> chooseTrees usedIn >>= \case
    (Fix (NRecSet_ _ set), outer) -> choose binds >>= \case
      (this, others) -> let
          names = simpleBoundNames this
          allNamesFree x = all (`noRef` x) names
          othersFree = all allNamesFree (values others)
            && allNamesFree outer
        in [LetInInheritRecset name | name <- names, plainInherits name set, othersFree]
    _ -> []
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
  , checkEtaReduce
  , checkFreeLetInFunc
  , checkLetInInheritRecset
  , checkDIYInherit
  ]

getSpan :: NExprLocF r -> SrcSpan
getSpan = annotation . getCompose

check :: CheckBase -> Check
check base = collectingPara (\x -> Offense (getSpan x) <$> base x)

checkAll :: Check
checkAll = check $ mergeCheckBase checks
