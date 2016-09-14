{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Elm.Type where

import           Data.Int     (Int16, Int32, Int64, Int8)
import           Data.Map
import           Data.Proxy
import           Data.Text
import           Data.Time
import           GHC.Generics
import           Prelude

-- TODO Without doubt, this definition can be tightened up so that
-- there are fewer (or hopefully zero) representable illegal states.
data ElmTypeExpr where
        TopLevel :: ElmTypeExpr -> ElmTypeExpr
        DataType :: Text -> ElmTypeExpr -> ElmTypeExpr
        Record :: Text -> ElmTypeExpr -> ElmTypeExpr
        Constructor :: Text -> ElmTypeExpr -> ElmTypeExpr
        Selector :: Text -> ElmTypeExpr -> ElmTypeExpr
        Field :: ElmTypeExpr -> ElmTypeExpr
        Sum :: ElmTypeExpr -> ElmTypeExpr -> ElmTypeExpr
        Dict :: ElmTypeExpr -> ElmTypeExpr -> ElmTypeExpr
        Tuple2 :: ElmTypeExpr -> ElmTypeExpr -> ElmTypeExpr
        Product :: ElmTypeExpr -> ElmTypeExpr -> ElmTypeExpr
        Unit :: ElmTypeExpr
        Primitive :: Text -> ElmTypeExpr
    deriving (Eq, Show)

class ElmType a  where
    toElmType :: a -> ElmTypeExpr
    toElmType = genericToElmType . from
    default toElmType :: (Generic a, GenericElmType (Rep a)) => a -> ElmTypeExpr

instance ElmType Bool where
    toElmType _ = Primitive "Bool"

instance ElmType Char where
    toElmType _ = Primitive "Char"

instance ElmType Text where
    toElmType _ = Primitive "String"

instance ElmType Float where
    toElmType _ = Primitive "Float"

instance ElmType UTCTime where
    toElmType _ = Primitive "Date"

instance ElmType Day where
    toElmType _ = Primitive "Date"

instance ElmType Double where
    toElmType _ = Primitive "Float"

instance ElmType Int where
    toElmType _ = Primitive "Int"

instance ElmType Integer where
    toElmType _ = Primitive "Int"

instance ElmType Int8 where
    toElmType _ = Primitive "Int"

instance ElmType Int16 where
    toElmType _ = Primitive "Int"

instance ElmType Int32 where
    toElmType _ = Primitive "Int"

instance ElmType Int64 where
    toElmType _ = Primitive "Int"

instance (ElmType a, ElmType b) =>
         ElmType (a, b) where
    toElmType _ =
        Tuple2 (toElmType (Proxy :: Proxy a)) (toElmType (Proxy :: Proxy b))

instance ElmType a =>
         ElmType [a] where
    toElmType _ = Product (Primitive "List") (toElmType (Proxy :: Proxy a))

instance ElmType a =>
         ElmType (Maybe a) where
    toElmType _ = Product (Primitive "Maybe") (toElmType (Proxy :: Proxy a))

instance (ElmType k, ElmType v) =>
         ElmType (Map k v) where
    toElmType _ =
        Dict (toElmType (Proxy :: Proxy k)) (toElmType (Proxy :: Proxy v))

instance ElmType a =>
         ElmType (Proxy a) where
    toElmType _ = toElmType (undefined :: a)

------------------------------------------------------------

class GenericElmType f  where
    genericToElmType :: f a -> ElmTypeExpr

instance (Datatype d, GenericElmType f) =>
         GenericElmType (D1 d f) where
    genericToElmType datatype =
        DataType
            (pack (datatypeName datatype))
            (genericToElmType (unM1 datatype))

instance (Constructor c, GenericElmType f) =>
         GenericElmType (C1 c f) where
    genericToElmType constructor =
        if conIsRecord constructor
            then Record name body
            else Constructor name body
      where
        name = pack $ conName constructor
        body = genericToElmType (unM1 constructor)

instance (Selector c, GenericElmType f) =>
         GenericElmType (S1 c f) where
    genericToElmType selector =
        Selector (pack (selName selector)) (genericToElmType (unM1 selector))

instance GenericElmType U1 where
    genericToElmType _ = Unit

instance (ElmType c) =>
         GenericElmType (Rec0 c) where
    genericToElmType parameter = Field (toElmType (unK1 parameter))


instance (GenericElmType f, GenericElmType g) =>
         GenericElmType (f :+: g) where
    genericToElmType _ =
        Sum
            (genericToElmType (undefined :: f p))
            (genericToElmType (undefined :: g p))

instance (GenericElmType f, GenericElmType g) =>
         GenericElmType (f :*: g) where
    genericToElmType _ =
        Product
            (genericToElmType (undefined :: f p))
            (genericToElmType (undefined :: g p))
