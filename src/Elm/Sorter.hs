{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoInstanceSigs #-}

module Elm.Sorter where

import Data.Int (Int64)
import Data.Proxy
import Data.Text (Text, pack)
import GHC.Generics
import GHC.TypeLits
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

data Sorter
    = Alphabetical
    | Increasing
    | ByField Text Sorter
    | ByNewtype Text Sorter

render :: Sorter -> Doc
render = \case
    Alphabetical -> "alphabetical"
    Increasing -> "increasing"
    ByField field sorter -> "by ." <> pretty field <> " " <> render sorter
    ByNewtype constructor sorter ->
        vsep
            [ "let unNewtype (" <> pretty constructor <> " value) = value"
            , "in by unNewtype " <> render sorter
            ]

{-
>>> render (ByNewtype "SchoolId" (ByField "name" Alphabetical))
let unNewtype (SchoolId value) = value
in by unNewtype (by .name alphabetical)
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
    genericElmSorter _ =
        ByNewtype (pack $ symbolVal (Proxy :: Proxy consName)) (elmSorter (Proxy :: Proxy interior))

instance
    (HasElmSorter interior, KnownSymbol fieldName) =>
    GenericElmSorter
        ( D1
            ('MetaData name version int 'True)
            (C1 ('MetaCons _consName _p 'True) (S1 ('MetaSel ('Just fieldName) _un _stri _lazy) (K1 _i interior)))
        )
    where
    genericElmSorter _ =
        ByField (pack $ symbolVal (Proxy :: Proxy fieldName)) (elmSorter (Proxy :: Proxy interior))

instance HasElmSorter Int where
    elmSorter _ = Increasing

instance HasElmSorter Int64 where
    elmSorter _ = Increasing

instance HasElmSorter Double where
    elmSorter _ = Increasing

instance HasElmSorter Text where
    elmSorter _ = Alphabetical

instance HasElmSorter String where
    elmSorter _ = Alphabetical

newtype A = SchoolId Int deriving (Generic, HasElmSorter)

newtype B = Test {unTest :: Text} deriving (Generic, HasElmSorter)

{-

>>> render (elmSorter (Proxy :: Proxy A))
let unNewtype (SchoolId value) = value
in by unNewtype increasing

>>> render (elmSorter (Proxy :: Proxy B))
by .unTest alphabetical

-}
