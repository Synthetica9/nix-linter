{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Nix.Linter.Types where

import           Control.Monad            (join)
import           Data.Fix
import           Data.Text                (Text, pack)

import           Data.Aeson
import           GHC.Generics
import           System.Console.CmdArgs   (Data)

import           Nix.Expr.Types
import           Nix.Expr.Types.Annotated
import           Nix.Pretty               (prettyNix)
import           Text.Megaparsec.Pos      (unPos)

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
  toJSON (Offense{..}) = object
    [ "offending" .= showNix (stripAnnotation offending)
    , "rewrite" .= toJSON (showNix <$> rewrite)
    , "pos" .= toJSON pos
    , "notes" .= toJSON notes
    , "offense" .= toJSON offense
    , "file" .= sourceName (spanBegin pos)
    ] where showNix = pack . show . prettyNix

data Note
  = IncreasesGenerality
  | Note Text Text deriving (Show, Generic)

instance ToJSON Note where
  toJSONList xs = object $ toJSON <$$> convert <$> xs where
    convert (Note a b) = (a, Just b)
    convert x          = (pack $ show x, Nothing)

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

note' :: Text -> Text -> Offense -> Offense
note' a b = note $ Note a b

getPos :: NExprLoc -> SrcSpan
getPos = annotation . getCompose . unFix

-- For ease of pattern matching
type UnwrappedNExprLoc = NExprLocF (Fix NExprLocF)

type CheckBase = (OffenseCategory -> Offense) -> NExprLoc -> [Offense]

getSpan :: NExprLoc -> SrcSpan
getSpan = annotation . getCompose . unFix

check :: CheckBase -> Check
check base tree = (\e -> base (Offense e Nothing (getSpan e) []) e) =<< universe tree

prettySourcePos :: SourcePos -> String
prettySourcePos (SourcePos file l c) = file ++ ":" ++ show (unPos l) ++ ":" ++ show (unPos c)

prettySourceSpan :: SrcSpan -> String
prettySourceSpan (SrcSpan pos1@(SourcePos f1 l1 c1) pos2@(SourcePos f2 l2 c2))
  | f1 /= f2 = base ++ prettySourcePos pos2 -- It could happen I guess?
  | l1 /= l2 = base ++ show (unPos l2) ++ ":" ++ show (unPos c2)
  | c1 /= c2 = base ++ show (unPos c2)
  | otherwise = prettySourcePos pos1
    where base = prettySourcePos pos1 ++ "-"

singletonSpan :: SourcePos -> SrcSpan
singletonSpan = join SrcSpan

prettyOffense :: Offense -> String
prettyOffense (Offense {..}) = show offense ++ " at " ++ prettySourceSpan pos

data OffenseCategory
  = RepetitionWithoutWith
  | UnusedLetBind
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
  deriving (Show, Generic, Data, Ord, Eq)

instance ToJSON OffenseCategory
