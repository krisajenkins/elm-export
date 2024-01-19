{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

module Elm.Sorter (Sorter, mkRecordSorter, mkCustom, HasElmSorter (..), render) where

import Data.Generics.Product.Fields (HasField')
import Data.Int (Int32, Int64)
import Data.Proxy
import Data.Text (Text, pack)
import Elm.Common (letIn)
import GHC.Generics
import GHC.TypeLits
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

data Sorter
  = Alphabetical
  | Increasing
  | ByField Text Sorter
  | ByNewtype Text Sorter
  | Custom Text
  deriving (Eq, Show)

render :: Sorter -> Doc
render = \case
  Alphabetical -> "Sort.alphabetical"
  Increasing -> "Sort.increasing"
  ByField field sorter -> parens ("Sort.by ." <> pretty field <> " " <> render sorter)
  ByNewtype constructor sorter ->
    let fnName = "un" <> constructor
     in parens $
          letIn
            [(fnName <> " (" <> constructor <> " value)", "value")]
            (hsep ["Sort.by", pretty fnName, render sorter])
  Custom custom -> parens ("Sort.custom" <+> pretty custom)

{-
>>> render (ByNewtype "SchoolId" (ByField "name" Alphabetical))
(let
     unNewtype (SchoolId value) =
         value
 in
 Sort.by unNewtype (Sort.by .name (Sort.alphabetical)))
-}

class HasElmSorter a where
  elmSorter :: Proxy a -> Sorter
  elmSorter _ = genericElmSorter . from $ (undefined :: a)
  default elmSorter :: (Generic a, GenericElmSorter (Rep a)) => Proxy a -> Sorter

class GenericElmSorter f where
  genericElmSorter :: f a -> Sorter

instance
  (HasElmSorter interior, KnownSymbol consName) =>
  GenericElmSorter
    ( D1
        ('MetaData name version int 'True)
        (C1 ('MetaCons consName _p 'False) (S1 _s (K1 _i interior)))
    )
  where
  genericElmSorter = elmSorterNewtype

instance
  (HasElmSorter interior, KnownSymbol fieldName) =>
  GenericElmSorter
    ( D1
        ('MetaData name version int 'True)
        (C1 ('MetaCons _consName _p 'True) (S1 ('MetaSel ('Just fieldName) _un _stri _lazy) (K1 _i interior)))
    )
  where
  genericElmSorter = elmSorterRecord

elmSorterNewtype ::
  forall
    interior
    (consName :: Symbol)
    (name :: Symbol)
    (version :: Symbol)
    (int :: Symbol)
    (_p :: FixityI)
    (_s :: Meta)
    _i
    a.
  (HasElmSorter interior, KnownSymbol consName) =>
  D1
    ('MetaData name version int 'True)
    (C1 ('MetaCons consName _p 'False) (S1 _s (K1 _i interior)))
    a ->
  Sorter
elmSorterNewtype _ =
  ByNewtype (pack $ symbolVal (Proxy :: Proxy consName)) (elmSorter (Proxy :: Proxy interior))

elmSorterRecord ::
  forall
    interior
    (fieldName :: Symbol)
    (name :: Symbol)
    (version :: Symbol)
    (int :: Symbol)
    (_consName :: Symbol)
    (_p :: FixityI)
    (_un :: SourceUnpackedness)
    (_stri :: SourceStrictness)
    (_lazy :: DecidedStrictness)
    _i
    a.
  (HasElmSorter interior, KnownSymbol fieldName) =>
  D1
    ('MetaData name version int 'True)
    ( C1
        ('MetaCons _consName _p 'True)
        (S1 ('MetaSel ('Just fieldName) _un _stri _lazy) (K1 _i interior))
    )
    a ->
  Sorter
elmSorterRecord _ =
  ByField (pack $ symbolVal (Proxy :: Proxy fieldName)) (elmSorter (Proxy :: Proxy interior))

instance HasElmSorter Int where
  elmSorter _ = Increasing

instance HasElmSorter Int32 where
  elmSorter _ = Increasing

instance HasElmSorter Int64 where
  elmSorter _ = Increasing

instance HasElmSorter Double where
  elmSorter _ = Increasing

instance HasElmSorter Text where
  elmSorter _ = Alphabetical

instance HasElmSorter String where
  elmSorter _ = Alphabetical

mkRecordSorter :: forall (field :: Symbol) record type_. (HasField' field record type_, KnownSymbol field, HasElmSorter type_) => Proxy record -> Sorter
mkRecordSorter _ = ByField (pack $ symbolVal (Proxy :: Proxy field)) (elmSorter (Proxy :: Proxy type_))

mkCustom :: Text -> Sorter
mkCustom = Custom
