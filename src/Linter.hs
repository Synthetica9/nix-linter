{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Linter where

import           Control.Monad
import           Data.Fix
import           Data.Foldable            (fold)
import           Data.List.NonEmpty
import           Data.Maybe

import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated

import           Linter.Types

import qualified Text.Megaparsec.Pos      as MPP

maximumRepetitionsWithoutWith = 3


-- repetitionsWithoutWith :: NExprF r -> [OffenseF r]
-- repetitionsWithoutWith = traverse _


-- From https://blog.sumtypeofway.com/recursion-schemes-part-iii-folds-in-context/,
-- but removed the author's head from his ass.

-- cata ::              (a ->       b  -> b) -> b     -> [a]    -> b
-- para ::              (a -> ([a], b) -> b) -> b     -> [a]    -> b
-- cata :: Functor f => (f a           -> a)          -> Fix f -> a
para    :: Functor f => (f (Fix f, a)  -> a)          -> Fix f -> a
para rAlg = rAlg . fmap fanout . unFix
    where fanout t = (t, para rAlg t)


foldingPara :: (Functor f, Foldable f) => (a -> a -> a) -> a -> (f (Fix f, a) -> Maybe a) -> Fix f -> a
foldingPara (+) ϵ f = para rAlgebra where
  rAlgebra x = fromMaybe (foldr ((+) . snd) ϵ x) $ f x

foldingPara' = foldingPara mappend mempty

foldingCata :: (Functor f, Foldable f) => (a -> a -> a) -> a -> (f a -> Maybe a) -> Fix f -> a
foldingCata (+) ϵ f = cata algebra where
  algebra x = fromMaybe (foldr (+) ϵ x) $ f x

foldingCata' :: (Functor f, Foldable f, Monoid m) => (f m -> Maybe m) -> Fix f -> m
foldingCata' = foldingCata mappend mempty

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

choose :: [a] -> [(a, [a])]
choose []       = []
choose (x : xs) = (x, xs) : ((x :) <$$> choose xs)

hasUsefulRef :: VarName -> NExpr -> Bool
hasUsefulRef name = foldingCata (||) False $ \case
  NSym name'  -> Just $ name == name'
  NAssert _ x -> Just x
  NSet xs  -> Just $ or $ xs <&> \case
    Inherit Nothing names _ -> StaticKey name `elem` names -- Dynamic keys aren't allowed in this context.
    x -> or x
  _           -> Nothing


fromMegaparsecPos :: MPP.SourcePos -> SourcePos
fromMegaparsecPos (MPP.SourcePos file x y) = SourcePos file x y

checkUnused :: NExpr -> [Offense]
checkUnused = foldingPara' $ \case
  (NLet binds (usedIn, otherOffenses)) -> Just $ otherOffenses ++
    concat (choose binds <&> \(bind, binds) -> let
      extraO = fold $ snd <$> bind :: [Offense]
      bind' = fst <$> bind
      binds' = fst <$$> binds
      offenses = case bind' of
        (NamedVar (StaticKey name :| []) val pos) -> [Offense
          (UnusedLetBind name)
          (fromMegaparsecPos pos)
          | not $ hasUsefulRef name $ Fix $ NLet binds' usedIn]
        _                               -> []
      in offenses ++ extraO)
  _ -> Nothing

checkEmptyInherit :: NExpr -> [Offense]
checkEmptyInherit = foldingPara' $ \case
  NSet xs -> Just $ concat $ xs <&> \case
    Inherit Nothing [] pos -> [Offense EmptyInherit (fromMegaparsecPos pos)]
    x -> concat $ snd <$> x
  _ -> Nothing

checks :: [NExpr -> [Offense]]
checks = [checkEmptyInherit, checkUnused]

checkAll :: NExpr -> [Offense]
checkAll x = ($ x) =<< checks
