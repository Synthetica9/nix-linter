module Nix.Linter.Utils where

import           Data.Either (partitionEithers)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap


choose :: [a] -> [(a, [a])]
choose []       = []
choose (x : xs) = (x, xs) : ((x :) <$$> choose xs)

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted xs = and $ (<=) <$> xs <*> tail xs

sequenceEither :: [Either a b] -> Either [a] [b]
sequenceEither x = case partitionEithers x of
  ([], rights) -> Right rights
  (lefts, _)   -> Left lefts
