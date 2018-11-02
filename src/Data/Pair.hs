{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}


module Data.Pair where

import Control.Monad (join)

data Pair a = Pair a a deriving (Functor, Foldable, Show)
dup = join Pair
