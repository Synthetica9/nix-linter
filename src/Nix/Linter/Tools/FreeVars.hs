module Nix.Linter.Tools.FreeVars (getFreeVars, getFreeVars') where
import           Data.Set                 (Set)

import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated

getFreeVars' :: NExprLoc -> Set VarName
getFreeVars' = getFreeVars . stripAnnotation
