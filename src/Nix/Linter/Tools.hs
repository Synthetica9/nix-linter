{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}


module Nix.Linter.Tools where

import           Control.Monad             (join)
import           Data.Fix
import           Data.List                 (find)
import           Data.List.NonEmpty        (NonEmpty (..))
import           Data.Set                  (member)
import           Data.Text                 (isPrefixOf, pack)

import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated

import           Nix.Linter.Tools.FreeVars

import           Nix.Linter.Traversals
import           Nix.Linter.Utils


hasRef, noRef :: VarName -> NExprLoc -> Bool
hasRef name t = member name $ getFreeVars' t

noRef = not ... hasRef

getFreeVarName :: NExprLoc -> VarName
getFreeVarName x = let
    candidates = (VarName <$> pack . ("_freeVar" ++)) . show <$> ([1..] :: [Integer])
    -- We are guarranteed to find a good candidate, because candidates is
    -- infinite and x is strict
    Just var = find (not . (`member` getFreeVars' x)) candidates
  in var


getFreeVar :: NExprLoc -> NExprLoc
getFreeVar = Fix . NSymAnnF generated . getFreeVarName

topLevelBinds :: NExprLoc -> ([Binding NExprLoc], NExprLoc, Bool)
topLevelBinds e = case unFix e of
  NSetAnnF _ann Recursive xs -> (xs, e, True)
  -- Nonrecursive, so no context. We make up a context that can't possibly be valid.
  NSetAnnF _ann NonRecursive xs -> (xs, getFreeVar e, True)
  -- `let x = 1; y = x; in y` is valid, so e is the context!
  NLetAnnF _ xs _ -> (xs, e, False)
  -- Otherwise, our context is just empty!
  _               -> ([], e, False)

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

staticKeys :: [NKeyName x] -> [VarName]
staticKeys xs = do
  StaticKey x <- xs
  pure x

simpleBoundNames :: Binding x -> [VarName]
simpleBoundNames (NamedVar (StaticKey x :| []) _ _) = [x]
simpleBoundNames (Inherit _ xs _)                   = xs
simpleBoundNames _                                  = []

plainInherits :: VarName -> [Binding x] -> Bool
plainInherits x xs = or $ do
  Inherit Nothing ys _ <- xs
  pure $ x `elem` ys

plainInheritsAnywhere :: VarName -> NExprLoc -> Bool
plainInheritsAnywhere x e = any (plainInherits x . (\(a, _, _) -> a) . topLevelBinds) $ universe e

nonIgnoredName :: VarName -> Bool
nonIgnoredName (VarName x) = not $ isPrefixOf "_" x
