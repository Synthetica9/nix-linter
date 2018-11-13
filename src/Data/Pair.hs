{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}


module Data.Pair where

import           Control.Monad (join)

data Pair a = Pair a a deriving (Functor, Foldable, Show)

dup :: a -> Pair a
dup = join Pair
