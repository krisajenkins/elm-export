{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Elm.Type where

import           Data.Proxy
import           Data.Text
import           Data.Time
import           GHC.Generics
import           Prelude

-- TODO Without doubt, this definition can be tightened up so that
-- there are fewer (or hopefully zero) representable illegal states.
data ElmType where
        TopLevel :: ElmType -> ElmType
        DataType :: String -> ElmType -> ElmType
        Record :: String -> ElmType -> ElmType
        Constructor :: String -> ElmType -> ElmType
        Selector :: String -> ElmType -> ElmType
        Field :: ElmType -> ElmType
        Sum :: ElmType -> ElmType -> ElmType
        Product :: ElmType -> ElmType -> ElmType
        Unit :: ElmType
        Primitive :: String -> ElmType
    deriving (Eq,Show)

class ToElmType a  where
  toElmType :: a -> ElmType
  default toElmType :: (Generic a,GenericToElmType (Rep a)) => a -> ElmType
  toElmType = genericToElmType . from

instance ToElmType Char where
    toElmType _ = Primitive "Char"

instance ToElmType Text where
    toElmType _ = Primitive "String"

instance ToElmType Float where
    toElmType _ = Primitive "Float"

instance ToElmType UTCTime where
    toElmType _ = Primitive "Date"

instance ToElmType Day where
    toElmType _ = Primitive "Date"

instance ToElmType Double where
    toElmType _ = Primitive "Float"

instance ToElmType Int where
    toElmType _ = Primitive "Int"

instance ToElmType Integer where
    toElmType _ = Primitive "Int"

instance ToElmType a => ToElmType [a] where
    toElmType _ = Product (Primitive "List") (toElmType (undefined :: a))

instance ToElmType a => ToElmType (Maybe a) where
  toElmType _ = Product (Primitive "Maybe") (toElmType (undefined :: a))

instance ToElmType a => ToElmType (Proxy a) where
  toElmType _ = toElmType (undefined :: a)

class GenericToElmType f  where
  genericToElmType :: f a -> ElmType

instance (GenericToElmType f,Datatype d) => GenericToElmType (D1 d f) where
  genericToElmType d@(M1 x) = DataType (datatypeName d) (genericToElmType x)

instance (Constructor c,GenericToElmType f) => GenericToElmType (C1 c f) where
  genericToElmType c@(M1 x) =
    if conIsRecord c
       then Record name body
       else Constructor name body
    where name = conName c
          body = genericToElmType x

instance (Selector c,GenericToElmType f) => GenericToElmType (S1 c f) where
  genericToElmType s@(M1 x) =
    Selector (selName s)
             (genericToElmType x)

instance GenericToElmType U1 where
  genericToElmType _  = Unit

instance (ToElmType c) => GenericToElmType (K1 R c) where
  genericToElmType (K1 x) = Field (toElmType x)

instance (GenericToElmType f,GenericToElmType g) => GenericToElmType (f :+: g) where
  genericToElmType _ = Sum
           (genericToElmType (undefined :: f p))
           (genericToElmType (undefined :: g p))

instance (GenericToElmType f,GenericToElmType g) => GenericToElmType (f :*: g) where
  genericToElmType _ =
    Product (genericToElmType (undefined :: f p))
            (genericToElmType (undefined :: g p))
