-- | Functions ported to Data.Fix from Data.Generics.Fixplate (ported as I need them)
module Nix.Linter.Traversals (contextList) where


import           Data.Fix
import qualified Data.Generics.Fixplate            as F

import           Control.Arrow                     ((&&&), (***))
import qualified Data.Generics.Fixplate.Traversals as T

-- TODO: use unsafecoerce?
fixToMu :: Functor f => Fix f -> F.Mu f
fixToMu = F.Fix . fmap fixToMu . unFix

muToFix :: Functor f => F.Mu f -> Fix f
muToFix = Fix . fmap muToFix . F.unFix

contextList :: Traversable f => Fix f -> [(Fix f, Fix f -> Fix f)]
contextList = fmap (muToFix *** ((muToFix .) . (. fixToMu))) . T.contextList . fixToMu
