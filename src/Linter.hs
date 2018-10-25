{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Linter where

import           Control.Monad
import           Data.Fix
import           Data.Foldable            (fold)
import qualified Data.HashSet             as S
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Maybe
import           Data.Set                 (member)
import           Data.Text                (isPrefixOf, pack)

import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.TH                   (freeVars)

import           Linter.Types

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

foldingPara' :: (Functor f, Foldable f, Monoid m) => (f (Fix f, m) -> Maybe m) -> Fix f -> m
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

gatherNames :: NExprLoc -> S.HashSet VarName
gatherNames = cata $ \case
   NSym_ _ var -> S.singleton var
   Compose (Ann _ x) -> fold x

choose :: [a] -> [(a, [a])]
choose []       = []
choose (x : xs) = (x, xs) : ((x :) <$$> choose xs)

hasRef :: VarName -> NExprLoc -> Bool
hasRef name t = member name $ (freeVars t)

values :: [Binding r] -> [r]
values = (f =<<)  where
  f (NamedVar _ x _) = [x]
  f _                = []

checkUnusedLetBinding :: Check
checkUnusedLetBinding = foldingPara' $ \case
  (NLet_ loc binds (usedIn, otherOffenses)) -> let
      newOffenses = choose (fst <$$> binds) >>= \case
        (bind, others) -> case bind of
          NamedVar (StaticKey name :| []) _ _ -> [
            Offense (UnusedLetBind name) loc
              | not $ any (hasRef name) (values others)
              , not $ hasRef name usedIn]
          _ -> []
            in Just $ otherOffenses ++ newOffenses
  _ -> Nothing


checkUnusedArg :: Check
checkUnusedArg = foldingPara' $ \a -> let
  Ann loc content = getCompose a in case content of
    NAbs params (usedIn, otherOffenses) -> let
      names = filter (not . isPrefixOf "_") $ case params of
         Param name           -> [name]
         ParamSet xs _ global -> maybeToList global ++ (fst <$> xs)
      offenses = [Offense (UnusedArg name) loc | name <- names, not $ hasRef name usedIn]
      in Just $ otherOffenses ++ offenses
    _ -> Nothing


checkEmptyInherit :: Check
checkEmptyInherit = foldingPara' $ \case
  NSet_ pos xs -> Just $ concat $ xs <&> \case
    Inherit Nothing [] _ -> [Offense EmptyInherit pos]
    x -> concat $ snd <$> x
  _ -> Nothing

checkUnneededRec :: Check
checkUnneededRec = foldingPara' $ \case
  NRecSet_ pos binds -> let
      needsRec = choose (fst <$$> binds) <&> \case
        (bind, others) -> case bind of
          NamedVar (StaticKey name :| []) _ _ -> not $ any (hasRef name) (values others)
          _ -> False
      newOffenses = [Offense UnneededRec pos | not $ or needsRec]
      in Just $ newOffenses
  _ -> Nothing

checks :: [Check]
checks = [checkUnneededRec, checkEmptyInherit, checkUnusedArg, checkUnusedLetBinding]

checkAll :: Check
checkAll x = ($ x) =<< checks
