{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Nix.Linter.Types where

import           Control.Monad            (join)
import           Data.Fix
import           Data.Text                (Text, pack)

import           Data.Aeson
import qualified Data.Aeson.Key           as K
import           GHC.Generics
import           System.Console.CmdArgs   (Data)

import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Pretty               (prettyNix)

import           Nix.Linter.Traversals
import           Nix.Linter.Utils

data OffenseF a = Offense
  { offending :: NExprLoc
  , rewrite   :: Maybe NExpr -- Location info isn't important here, because none of it will be accurate.
  , pos       :: SrcSpan
  , notes     :: [Note]
  , offense   :: a
  } deriving (Functor, Show, Generic)

type Offense = OffenseF OffenseCategory
type Check = NExprLoc -> [Offense]

instance ToJSON Offense where
  toJSON o@(Offense{..}) = object
    [ "offending" .= showNix (stripAnnotation offending)
    , "rewrite" .= toJSON (showNix <$> rewrite)
    , "pos" .= toJSON pos
    , "notes" .= toJSON notes
    , "offense" .= toJSON offense
    , "file" .= sourceName (spanBegin pos)
    , "description" .= toJSON (describe o)
    ] where showNix = pack . show . prettyNix

data Note
  = IncreasesGenerality
  | Note Text Text deriving (Show, Generic)

instance ToJSON Note where
  toJSONList xs = object $ toJSON <$$> convert <$> xs where
    convert (Note a b) = (K.fromText a, Just b)
    convert x          = (K.fromText $ pack $ show x, Nothing)

setLoc :: SourcePos -> Offense -> Offense
setLoc l x = x { pos=singletonSpan l }

setPos :: SrcSpan -> Offense -> Offense
setPos l x = x { pos=l }

setOffender :: NExprLoc -> Offense -> Offense
setOffender e x = setPos (getPos e) $ x {offending=e}

suggest :: NExpr -> Offense -> Offense
suggest e x = x {rewrite = pure e}

suggest' :: NExprLoc -> Offense -> Offense
suggest' e = suggest $ stripAnnotation e

note :: Note -> Offense -> Offense
note n x = x {notes = n : notes x}

note' :: Text -> VarName -> Offense -> Offense
note' a (VarName b) = note $ Note a b

getPos :: NExprLoc -> SrcSpan
getPos = annotation . getCompose . unFix

-- For ease of pattern matching
type UnwrappedNExprLoc = NExprLocF (Fix NExprLocF)

type CheckBase = (OffenseCategory -> Offense) -> NExprLoc -> [Offense]

getSpan :: NExprLoc -> SrcSpan
getSpan = annotation . getCompose . unFix

check :: CheckBase -> Check
check base tree = (\e -> base (Offense e Nothing (getSpan e) []) e) =<< universe tree

pShow :: Show a => a -> Text
pShow = pack . show

prettySourcePos :: SourcePos -> Text
prettySourcePos (SourcePos file l c) = pack file <> ":" <> pShow (unPos l) <> ":" <> pShow (unPos c)

prettySourceSpan :: SrcSpan -> Text
prettySourceSpan (SrcSpan pos1@(SourcePos f1 l1 c1) pos2@(SourcePos f2 l2 c2))
  | f1 /= f2 = base <> prettySourcePos pos2 -- It could happen I guess?
  | l1 /= l2 = base <> pShow (unPos l2) <> ":" <> pShow (unPos c2)
  | c1 /= c2 = base <> pShow (unPos c2)
  | otherwise = prettySourcePos pos1
    where base = prettySourcePos pos1 <> "-"

singletonSpan :: SourcePos -> SrcSpan
singletonSpan = join SrcSpan

getNote :: Offense -> Text -> Maybe Text
getNote (Offense {..}) key = lookup key lt
  where lt = [ (k, v) | Note k v <- notes ]

-- TODO: escape
quoteVar :: Text -> Text
quoteVar v = "`" <> v <> "`"

describe :: Offense -> Text
describe full@(Offense {..}) = let
    fullNote = getNote full
    o = offense
    -- TODO: extract
    varName = fullNote "varName"
    now = fullNote "now"
    suggested = fullNote "suggested"

    whyNot = fmap (pack . show . prettyNix) rewrite
  in case o of
    UnusedLetBind | Just x <- varName -> "Unused `let` bind " <> quoteVar x
    UnusedArg | Just x <- varName -> "Unused argument " <> quoteVar x
    EmptyInherit -> "Empty `inherit`"
    UnneededRec -> "Unneeded `rec` on set"
    EtaReduce | Just x <- varName -> "Possible η-reduction of argument " <> quoteVar x
    ListLiteralConcat -> "Concatenating two list literals"
    SetLiteralUpdate -> "Concatenating two set literals with `//`"
    UpdateEmptySet -> "Updating an empty set with `//`"
    NegateAtom | Just r <- whyNot -> "Negating an atom, why not " <> quoteVar r
    EmptyVariadicParamSet -> "Using `{ ... }` as pattern match"
    DIYInherit | Just x <- varName -> "Use " <> quoteVar ("inherit " <> x)
    SequentialLet -> "Sequential `let` blocks (`let ... in let ... in ...`)"
    EmptyLet -> "Empty `let` block"
    UnfortunateArgName | Just x <- now, Just x' <- suggested -> "Unfortunate argument name "
      <> quoteVar x <> " prevents the use of `inherit`, why not use " <> quoteVar x'
    FreeLetInFunc -> "Move `let` block outside function definition"
    _ -> pShow o

prettyOffense :: Offense -> Text
prettyOffense o@(Offense {..}) = describe o <> " at " <> prettySourceSpan pos

data OffenseCategory
  = UnusedLetBind
  | UnusedArg
  | EmptyInherit
  | UnneededRec
  | ListLiteralConcat
  | SetLiteralUpdate
  | UpdateEmptySet
  | UnneededAntiquote
  | NegateAtom
  | EtaReduce
  | FreeLetInFunc
  | LetInInheritRecset
  | DIYInherit
  | EmptyLet
  | UnfortunateArgName
  | BetaReduction
  | AlphabeticalBindings
  | AlphabeticalArgs
  | SequentialLet
  | EmptyVariadicParamSet
  deriving (Show, Generic, Data, Ord, Eq, Bounded, Enum)

instance ToJSON OffenseCategory
