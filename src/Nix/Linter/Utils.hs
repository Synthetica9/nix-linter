module Nix.Linter.Utils where

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap


(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap


choose :: [a] -> [(a, [a])]
choose []       = []
choose (x : xs) = (x, xs) : ((x :) <$$> choose xs)
