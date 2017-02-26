{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Elm.Type where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap
import Data.Map
import Data.Proxy
import Data.Text hiding (all)
import Data.Time
import GHC.Generics
import Prelude

data ElmDatatype
  = ElmDatatype Text
                ElmConstructor
  | ElmPrimitive ElmPrimitive
  deriving (Show, Eq)

data ElmPrimitive
  = EInt
  | EBool
  | EChar
  | EDate
  | EFloat
  | EString
  | EUnit
  | EList ElmDatatype
  | EMaybe ElmDatatype
  | ETuple2 ElmDatatype
            ElmDatatype
  | EDict ElmPrimitive
          ElmDatatype
  deriving (Show, Eq)

data ElmConstructor
  = NamedConstructor Text
                     ElmValue
  | RecordConstructor Text
                      ElmValue
  | MultipleConstructors [ElmConstructor]
  deriving (Show, Eq)

data ElmValue
  = ElmRef Text
  | ElmEmpty
  | ElmPrimitiveRef ElmPrimitive
  | Values ElmValue
           ElmValue
  | ElmField Text
             ElmValue
  deriving (Show, Eq)

------------------------------------------------------------
class ElmType a where
  toElmType :: a -> ElmDatatype
  toElmType = genericToElmDatatype . from
  default toElmType :: (Generic a, GenericElmDatatype (Rep a)) =>
    a -> ElmDatatype

------------------------------------------------------------
class GenericElmDatatype f where
  genericToElmDatatype :: f a -> ElmDatatype

instance (Datatype d, GenericElmConstructor f) =>
         GenericElmDatatype (D1 d f) where
  genericToElmDatatype datatype =
    ElmDatatype
      (pack (datatypeName datatype))
      (genericToElmConstructor (unM1 datatype))

-- ------------------------------------------------------------
class GenericElmConstructor f where
  genericToElmConstructor :: f a -> ElmConstructor

instance (Constructor c, GenericElmValue f) =>
         GenericElmConstructor (C1 c f) where
  genericToElmConstructor constructor =
    if conIsRecord constructor
      then RecordConstructor name (genericToElmValue (unM1 constructor))
      else NamedConstructor name (genericToElmValue (unM1 constructor))
    where
      name = pack $ conName constructor

instance (GenericElmConstructor f, GenericElmConstructor g) =>
         GenericElmConstructor (f :+: g) where
  genericToElmConstructor _ =
    MultipleConstructors
      [ genericToElmConstructor (undefined :: f p)
      , genericToElmConstructor (undefined :: g p)
      ]

------------------------------------------------------------
class GenericElmValue f where
  genericToElmValue :: f a -> ElmValue

instance (Selector s, GenericElmValue a) =>
         GenericElmValue (S1 s a) where
  genericToElmValue selector =
    case selName selector of
      "" -> genericToElmValue (undefined :: a p)
      name -> ElmField (pack name) (genericToElmValue (undefined :: a p))

instance (GenericElmValue f, GenericElmValue g) =>
         GenericElmValue (f :*: g) where
  genericToElmValue _ =
    Values
      (genericToElmValue (undefined :: f p))
      (genericToElmValue (undefined :: g p))

instance GenericElmValue U1 where
  genericToElmValue _ = ElmEmpty

instance ElmType a =>
         GenericElmValue (Rec0 a) where
  genericToElmValue _ =
    case toElmType (Proxy :: Proxy a) of
      ElmPrimitive primitive -> ElmPrimitiveRef primitive
      ElmDatatype name _ -> ElmRef name

instance ElmType a =>
         ElmType [a] where
  toElmType _ = ElmPrimitive (EList (toElmType (Proxy :: Proxy a)))

instance ElmType a =>
         ElmType (Maybe a) where
  toElmType _ = ElmPrimitive (EMaybe (toElmType (Proxy :: Proxy a)))

instance ElmType () where
  toElmType _ = ElmPrimitive EUnit

instance ElmType Text where
  toElmType _ = ElmPrimitive EString

instance ElmType Day where
  toElmType _ = ElmPrimitive EDate

instance ElmType UTCTime where
  toElmType _ = ElmPrimitive EDate

instance ElmType Float where
  toElmType _ = ElmPrimitive EFloat

instance ElmType Double where
  toElmType _ = ElmPrimitive EFloat

instance ElmType Int8 where
  toElmType _ = ElmPrimitive EInt

instance ElmType Int16 where
  toElmType _ = ElmPrimitive EInt

instance ElmType Int32 where
  toElmType _ = ElmPrimitive EInt

instance ElmType Int64 where
  toElmType _ = ElmPrimitive EInt

instance (ElmType a, ElmType b) =>
         ElmType (a, b) where
  toElmType _ =
    ElmPrimitive $
    ETuple2 (toElmType (Proxy :: Proxy a)) (toElmType (Proxy :: Proxy b))

instance (ElmType a) =>
         ElmType (Proxy a) where
  toElmType _ = toElmType (undefined :: a)

instance (HasElmComparable k, ElmType v) =>
         ElmType (Map k v) where
  toElmType _ =
    ElmPrimitive $
    EDict (toElmComparable (undefined :: k)) (toElmType (Proxy :: Proxy v))

instance (ElmType v) =>
         ElmType (IntMap v) where
  toElmType _ = ElmPrimitive $ EDict EInt (toElmType (Proxy :: Proxy v))

class HasElmComparable a where
  toElmComparable :: a -> ElmPrimitive

instance HasElmComparable String where
  toElmComparable _ = EString

instance HasElmComparable Text where
  toElmComparable _ = EString

instance ElmType Int where
  toElmType _ = ElmPrimitive EInt

instance ElmType Char where
  toElmType _ = ElmPrimitive EChar

instance ElmType Bool where
  toElmType _ = ElmPrimitive EBool

-- | Whether a set of constructors is an enumeration, i.e. whether they lack
-- values. data A = A | B | C would be simple data A = A Int | B | C would not
-- be simple.
isEnumeration :: ElmConstructor -> Bool
isEnumeration (NamedConstructor _ ElmEmpty) = True
isEnumeration (MultipleConstructors cs) = all isEnumeration cs
isEnumeration _ = False
