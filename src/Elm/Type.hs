{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Elm.Type where

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
        DataType :: String -> ElmTypeExpr -> ElmTypeExpr
        Record :: String -> ElmTypeExpr -> ElmTypeExpr
        Constructor :: String -> ElmTypeExpr -> ElmTypeExpr
        Selector :: String -> ElmTypeExpr -> ElmTypeExpr
        Field :: ElmTypeExpr -> ElmTypeExpr
        Sum :: ElmTypeExpr -> ElmTypeExpr -> ElmTypeExpr
        Dict :: ElmTypeExpr -> ElmTypeExpr -> ElmTypeExpr
        Tuple2 :: ElmTypeExpr -> ElmTypeExpr -> ElmTypeExpr
        Product :: ElmTypeExpr -> ElmTypeExpr -> ElmTypeExpr
        Unit :: ElmTypeExpr
        Primitive :: String -> ElmTypeExpr
    deriving (Eq, Show)

class ElmType a  where
  toElmType :: a -> ElmTypeExpr
  default toElmType :: (Generic a,GenericElmType (Rep a)) => a -> ElmTypeExpr
  toElmType = genericToElmType . from

instance ElmType () where
    toElmType _ = Primitive "()"

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

instance (ElmType a,ElmType b) => ElmType (a,b) where
  toElmType _ =
    Tuple2 (toElmType (Proxy :: Proxy a))
           (toElmType (Proxy :: Proxy b))

instance ElmType a => ElmType [a] where
    toElmType _ = Product (Primitive "List") (toElmType (Proxy :: Proxy a))

instance ElmType a => ElmType (Maybe a) where
  toElmType _ = Product (Primitive "Maybe") (toElmType (Proxy :: Proxy a))

instance (ElmType k,ElmType v) => ElmType (Map k v) where
  toElmType _ =
    Dict (toElmType (Proxy :: Proxy k))
         (toElmType (Proxy :: Proxy v))

instance ElmType a => ElmType (Proxy a) where
  toElmType _ = toElmType (undefined :: a)

class GenericElmType f  where
  genericToElmType :: f a -> ElmTypeExpr

instance (GenericElmType f,Datatype d) => GenericElmType (D1 d f) where
  genericToElmType d@(M1 x) =
    DataType (datatypeName d)
             (genericToElmType x)

instance (Constructor c,GenericElmType f) => GenericElmType (C1 c f) where
  genericToElmType c@(M1 x) =
    if conIsRecord c
       then Record name body
       else Constructor name body
    where name = conName c
          body = genericToElmType x

instance (Selector c,GenericElmType f) => GenericElmType (S1 c f) where
  genericToElmType s@(M1 x) =
    Selector (selName s)
             (genericToElmType x)

instance GenericElmType U1 where
  genericToElmType _  = Unit

instance (ElmType c) => GenericElmType (K1 R c) where
  genericToElmType (K1 x) = Field (toElmType x)

instance (GenericElmType f,GenericElmType g) => GenericElmType (f :+: g) where
  genericToElmType _ = Sum
           (genericToElmType (undefined :: f p))
           (genericToElmType (undefined :: g p))

instance (GenericElmType f,GenericElmType g) => GenericElmType (f :*: g) where
  genericToElmType _ =
    Product (genericToElmType (undefined :: f p))
            (genericToElmType (undefined :: g p))
