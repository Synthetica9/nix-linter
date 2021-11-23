module Nix.Linter.Tools.FreeVars (freeVars, freeVars', freeVarsIgnoreTopBinds
                                 , freeVarsIgnoreTopBinds') where
import           Data.Set                 (Set)

import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.TH                   (freeVars)

import qualified Data.Set                 as Set
import           Data.Maybe               (mapMaybe)
import           Data.Fix                 (unFix, Fix (..))

freeVars' :: NExprLoc -> Set VarName
freeVars' = freeVars . stripAnnotation

freeVarsIgnoreTopBinds' :: NExprLoc -> Set VarName
freeVarsIgnoreTopBinds' = freeVarsIgnoreTopBinds . stripAnnotation

-- gets the free variables of the expression, assuming the top level bindings
-- were not bound. Note that this function is *not* recursive, since we want to
-- count bindings deeper in the tree
freeVarsIgnoreTopBinds :: NExpr -> Set VarName
freeVarsIgnoreTopBinds e =
  case unFix e of
    (NSet NRecursive bindings) -> bindFreeVars bindings
    (NAbs (Param _) expr) -> freeVars expr
    (NAbs (ParamSet set _ _) expr) ->
      freeVars expr <> (Set.unions $ freeVars <$> mapMaybe snd set)
    (NLet bindings expr) -> freeVars expr <> bindFreeVars bindings
    noTopBinds -> freeVars $ Fix noTopBinds
 where
  bindFreeVars :: Foldable t => t (Binding NExpr) -> Set VarName
  bindFreeVars = foldMap bind1Free
   where
    bind1Free :: Binding NExpr -> Set VarName
    bind1Free (Inherit  Nothing     keys _) = Set.fromList $ mapMaybe staticKey keys
    bind1Free (Inherit (Just scope) _    _) = freeVars scope
    bind1Free (NamedVar path        expr _) = pathFree path <> freeVars expr

  staticKey :: NKeyName r -> Maybe VarName
  staticKey (StaticKey  varname) = pure varname
  staticKey (DynamicKey _      ) = mempty

  pathFree :: NAttrPath NExpr -> Set VarName
  pathFree = foldMap mapFreeVars

  mapFreeVars :: Foldable t => t NExpr -> Set VarName
  mapFreeVars = foldMap freeVars
