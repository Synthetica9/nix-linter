-- | Functions ported to Data.Fix from Data.Generics.Fixplate (ported as I need them)
module Nix.Linter.Traversals (contextList, universe) where


import           Data.Fix
import qualified Data.Generics.Fixplate            as F

import           Control.Arrow                     ((***))
import qualified Data.Generics.Fixplate.Traversals as T

fixToMu :: Functor f => Fix f -> F.Mu f
fixToMu = F.Fix . fmap fixToMu . unFix

muToFix :: Functor f => F.Mu f -> Fix f
muToFix = Fix . fmap muToFix . F.unFix

contextList :: Traversable f => Fix f -> [(Fix f, Fix f -> Fix f)]
contextList = fmap (muToFix *** ((muToFix .) . (. fixToMu))) . T.contextList . fixToMu

-- If the functor constraint ever gives the slightest problem, I'm goingo change
-- fixToMu and muToFix to just use unsafeCoerce instead.
universe :: (Foldable f, Functor f) => Fix f -> [Fix f]
universe = fmap muToFix . T.universe . fixToMu
