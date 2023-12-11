{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Elm.Type where

import qualified Data.Aeson as Aeson
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap
import Data.Map
import Data.Proxy
import Data.Set (Set)
import Data.Text hiding (all)
import Data.Time
import Elm.Sorter (HasElmSorter (..), Sorter)
import GHC.Generics
import Servant.API (Headers (getResponse))
import Prelude

data ElmDatatype
  = ElmDatatype
      Text
      ElmConstructor
  | ElmPrimitive ElmPrimitive
  | CreatedInElm ElmRefData
  deriving (Show, Eq)

data MapEncoding
  = List
  | Object
  deriving (Show, Eq)

data ElmPrimitive
  = EInt
  | EBool
  | EChar
  | ETimePosix
  | EFloat
  | EString
  | EUnit
  | EJsonValue
  | EList ElmDatatype
  | EMaybe ElmDatatype
  | ETuple2
      ElmDatatype
      ElmDatatype
  | EDict
      ElmPrimitive
      ElmDatatype
  | ESet ElmPrimitive
  | ESortDict
      Sorter
      MapEncoding
      ElmDatatype
      ElmDatatype
  | ESortSet
      Sorter
      ElmDatatype
  deriving (Show, Eq)

data ElmConstructor
  = NamedConstructor
      Text
      ElmValue
  | RecordConstructor
      Text
      ElmValue
  | MultipleConstructors [ElmConstructor]
  deriving (Show, Eq)

data ElmValue
  = ElmRef ElmRefData
  | ElmEmpty
  | ElmPrimitiveRef ElmPrimitive
  | Values
      ElmValue
      ElmValue
  | ElmField
      Text
      ElmValue
  deriving (Show, Eq)

data ElmRefData = ElmRefData
  { typeName :: Text,
    encoderFunction :: Text,
    decoderFunction :: Text
  }
  deriving (Show, Eq)

------------------------------------------------------------
class ElmType a where
  toElmType :: a -> ElmDatatype
  toElmType = genericToElmDatatype . from
  default toElmType ::
    (Generic a, GenericElmDatatype (Rep a)) =>
    a ->
    ElmDatatype

fromElm :: ElmRefData -> a -> ElmDatatype
fromElm x _ = CreatedInElm x

------------------------------------------------------------
class GenericElmDatatype f where
  genericToElmDatatype :: f a -> ElmDatatype

instance
  (Datatype d, GenericElmConstructor f) =>
  GenericElmDatatype (D1 d f)
  where
  genericToElmDatatype datatype =
    ElmDatatype
      (pack (datatypeName datatype))
      (genericToElmConstructor (unM1 datatype))

-- ------------------------------------------------------------
class GenericElmConstructor f where
  genericToElmConstructor :: f a -> ElmConstructor

instance
  (Constructor c, GenericElmValue f) =>
  GenericElmConstructor (C1 c f)
  where
  genericToElmConstructor constructor =
    if conIsRecord constructor
      then RecordConstructor name (genericToElmValue (unM1 constructor))
      else NamedConstructor name (genericToElmValue (unM1 constructor))
    where
      name = pack $ conName constructor

instance
  (GenericElmConstructor f, GenericElmConstructor g) =>
  GenericElmConstructor (f :+: g)
  where
  genericToElmConstructor _ =
    MultipleConstructors
      [ genericToElmConstructor (undefined :: f p),
        genericToElmConstructor (undefined :: g p)
      ]

------------------------------------------------------------
class GenericElmValue f where
  genericToElmValue :: f a -> ElmValue

instance
  (Selector s, GenericElmValue a) =>
  GenericElmValue (S1 s a)
  where
  genericToElmValue selector =
    case selName selector of
      "" -> genericToElmValue (undefined :: a p)
      name -> ElmField (pack name) (genericToElmValue (undefined :: a p))

instance
  (GenericElmValue f, GenericElmValue g) =>
  GenericElmValue (f :*: g)
  where
  genericToElmValue _ =
    Values
      (genericToElmValue (undefined :: f p))
      (genericToElmValue (undefined :: g p))

instance GenericElmValue U1 where
  genericToElmValue _ = ElmEmpty

instance
  (ElmType a) =>
  GenericElmValue (Rec0 a)
  where
  genericToElmValue _ =
    case toElmType (Proxy :: Proxy a) of
      ElmPrimitive primitive -> ElmPrimitiveRef primitive
      ElmDatatype name _ ->
        ElmRef
          ( ElmRefData
              { typeName = name,
                encoderFunction = "encode" <> name,
                decoderFunction = "decode" <> name
              }
          )
      CreatedInElm elmRefData ->
        ElmRef elmRefData

instance
  (ElmType a) =>
  ElmType [a]
  where
  toElmType _ = ElmPrimitive (EList (toElmType (Proxy :: Proxy a)))

instance
  (ElmType a) =>
  ElmType (Maybe a)
  where
  toElmType _ = ElmPrimitive (EMaybe (toElmType (Proxy :: Proxy a)))

instance ElmType () where
  toElmType _ = ElmPrimitive EUnit

instance ElmType Text where
  toElmType _ = ElmPrimitive EString

instance ElmType Day where
  toElmType _ = ElmPrimitive ETimePosix

instance ElmType UTCTime where
  toElmType _ = ElmPrimitive ETimePosix

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

instance ElmType Aeson.Value where
  toElmType _ = ElmPrimitive EJsonValue

instance
  (ElmType a, ElmType b) =>
  ElmType (a, b)
  where
  toElmType _ =
    ElmPrimitive $
      ETuple2 (toElmType (Proxy :: Proxy a)) (toElmType (Proxy :: Proxy b))

instance
  (ElmType a) =>
  ElmType (Proxy a)
  where
  toElmType _ = toElmType (undefined :: a)

instance
  {-# OVERLAPPABLE #-}
  (HasElmSorter a, ElmType a) =>
  ElmType (Set a)
  where
  toElmType _ =
    case setPrimitive of
      Just primitive -> ElmPrimitive $ ESet primitive
      Nothing -> ElmPrimitive $ ESortSet (elmSorter (Proxy @a)) (toElmType (undefined :: a))
    where
      elmType = toElmType (Proxy :: Proxy a)
      setPrimitive = case elmType of
        ElmPrimitive EChar -> Just EChar
        ElmPrimitive EString -> Just EString
        ElmPrimitive EInt -> Just EInt
        ElmPrimitive EFloat -> Just EFloat
        _ -> Nothing

instance ElmType (Set String) where
  toElmType _ = ElmPrimitive $ ESet EString

instance ElmType (Set Text) where
  toElmType _ = ElmPrimitive $ ESet EString

instance ElmType (Set Float) where
  toElmType _ = ElmPrimitive $ ESet EFloat

instance ElmType (Set Double) where
  toElmType _ = ElmPrimitive $ ESet EFloat

instance ElmType (Set Int) where
  toElmType _ = ElmPrimitive $ ESet EInt

instance ElmType (Set Int8) where
  toElmType _ = ElmPrimitive $ ESet EInt

instance ElmType (Set Int16) where
  toElmType _ = ElmPrimitive $ ESet EInt

instance ElmType (Set Int32) where
  toElmType _ = ElmPrimitive $ ESet EInt

instance ElmType (Set Int64) where
  toElmType _ = ElmPrimitive $ ESet EInt

instance
  (ElmType v) =>
  ElmType (IntMap v)
  where
  toElmType _ = ElmPrimitive $ EDict EInt (toElmType (Proxy :: Proxy v))

instance
  {-# OVERLAPPABLE #-}
  (HasElmSorter k, Aeson.ToJSONKey k, ElmType k, ElmType v) =>
  ElmType (Map k v)
  where
  toElmType _ =
    ElmPrimitive $
      case dictKeyPrimitive of
        Just primitive ->
          EDict primitive (toElmType (Proxy :: Proxy v))
        Nothing ->
          ESortDict (elmSorter (Proxy :: Proxy k)) encodingStrategy (toElmType (Proxy :: Proxy k)) (toElmType (Proxy :: Proxy v))
    where
      elmType = toElmType (Proxy :: Proxy k)
      dictKeyPrimitive = case elmType of
        ElmPrimitive EChar -> Just EChar
        ElmPrimitive EString -> Just EString
        ElmPrimitive EInt -> Just EInt
        ElmPrimitive EFloat -> Just EFloat
        _ -> Nothing
      encodingStrategy = case (Aeson.toJSONKey @k) of
        Aeson.ToJSONKeyText _ _ -> Object
        Aeson.ToJSONKeyValue _ _ -> List

instance
  (ElmType a) =>
  ElmType (Map String a)
  where
  toElmType _ = ElmPrimitive $ EDict EString (toElmType (Proxy :: Proxy a))

instance
  (ElmType a) =>
  ElmType (Map Text a)
  where
  toElmType _ = ElmPrimitive $ EDict EString (toElmType (Proxy :: Proxy a))

instance
  (ElmType a) =>
  ElmType (Map Float a)
  where
  toElmType _ = ElmPrimitive $ EDict EFloat (toElmType (Proxy :: Proxy a))

instance
  (ElmType a) =>
  ElmType (Map Double a)
  where
  toElmType _ = ElmPrimitive $ EDict EFloat (toElmType (Proxy :: Proxy a))

instance
  (ElmType a) =>
  ElmType (Map Int a)
  where
  toElmType _ = ElmPrimitive $ EDict EInt (toElmType (Proxy :: Proxy a))

instance
  (ElmType a) =>
  ElmType (Map Int8 a)
  where
  toElmType _ = ElmPrimitive $ EDict EInt (toElmType (Proxy :: Proxy a))

instance
  (ElmType a) =>
  ElmType (Map Int16 a)
  where
  toElmType _ = ElmPrimitive $ EDict EInt (toElmType (Proxy :: Proxy a))

instance
  (ElmType a) =>
  ElmType (Map Int32 a)
  where
  toElmType _ = ElmPrimitive $ EDict EInt (toElmType (Proxy :: Proxy a))

instance
  (ElmType a) =>
  ElmType (Map Int64 a)
  where
  toElmType _ = ElmPrimitive $ EDict EInt (toElmType (Proxy :: Proxy a))

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

-- We define this instance here because it is an orphan otherwise.

instance (ElmType a) => ElmType (Headers headers a) where
  toElmType = toElmType . getResponse
