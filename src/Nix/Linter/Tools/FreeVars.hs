module Nix.Linter.Tools.FreeVars (freeVars, freeVars') where
import           Data.Set                 (Set)

import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.TH                   (freeVars)

freeVars' :: NExprLoc -> Set VarName
freeVars' = freeVars . stripAnnotation
