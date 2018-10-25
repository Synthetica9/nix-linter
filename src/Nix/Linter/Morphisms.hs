module Nix.Linter.Morphisms where

import           Data.Fix
import           Data.Maybe

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
