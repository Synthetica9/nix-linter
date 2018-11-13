module Nix.Linter.Tools.FreeVars where

import           Data.Fix
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Maybe               (mapMaybe)
import           Data.Set                 (Set, (\\))
import qualified Data.Set                 as Set

import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated

freeVars' :: NExprLoc -> Set VarName
freeVars' = freeVars . stripAnnotation

freeVars :: NExpr -> Set VarName
freeVars e = case unFix e of
  (NConstant _) -> Set.empty
  (NStr string) -> foldMap freeVars string
  (NSym var) -> Set.singleton var
  (NList list) -> foldMap freeVars list
  (NSet bindings) -> foldMap bindFree bindings
  (NRecSet bindings) -> foldMap bindFree bindings \\ foldMap bindDefs bindings
  (NLiteralPath _) -> Set.empty
  (NEnvPath _) -> Set.empty
  (NUnary _ expr) -> freeVars expr
  (NBinary _ left right) -> freeVars left `Set.union` freeVars right
  (NSelect expr path orExpr) -> freeVars expr `Set.union` pathFree path `Set.union` maybe Set.empty freeVars orExpr
  (NHasAttr expr path) -> freeVars expr `Set.union` pathFree path
  (NAbs (Param varname) expr) -> Set.delete varname (freeVars expr)
  (NAbs (ParamSet set _ varname) expr) ->
    -- Include all free variables from the expression and the default arguments
    freeVars expr `Set.union` Set.unions (mapMaybe (fmap freeVars . snd) set)
    -- But remove the argument name if existing, and all arguments in the parameter set
    \\ maybe Set.empty Set.singleton varname \\ Set.fromList (map fst set)
  (NLet bindings expr) -> freeVars expr `Set.union` foldMap bindFree bindings \\ foldMap bindDefs bindings
  (NIf cond th el) -> freeVars cond `Set.union` freeVars th `Set.union` freeVars el
  -- Evaluation is needed to find out whether x is a "real" free variable in `with y; x`, we just include it
  -- This also makes sense because its value can be overridden by `x: with y; x`
  (NWith set expr) -> freeVars set `Set.union` freeVars expr
  (NAssert assertion expr) -> freeVars assertion `Set.union` freeVars expr

  where

    staticKey :: NKeyName r -> Maybe VarName
    staticKey (StaticKey varname) = Just varname
    staticKey (DynamicKey _)      = Nothing

    bindDefs :: Binding r -> Set VarName
    bindDefs (Inherit _ keys _) = Set.fromList $ mapMaybe staticKey keys
    bindDefs (NamedVar (StaticKey varname :| _) _ _) = Set.singleton varname
    bindDefs (NamedVar (DynamicKey _ :| _) _ _) = Set.empty

    bindFree :: Binding NExpr -> Set VarName
    bindFree (Inherit Nothing keys _) = Set.fromList $ mapMaybe staticKey keys
    bindFree (Inherit (Just scope) _ _) = freeVars scope
    bindFree (NamedVar path expr _) = pathFree path `Set.union` freeVars expr

    pathFree :: NAttrPath NExpr -> Set VarName
    pathFree = foldMap (foldMap freeVars)
