{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ExportSpec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Algorithm.Diff as Diff
import qualified Data.Algorithm.DiffOutput as DiffOutput
import Data.Char
import Data.Int
import Data.IntMap
import Data.Map
import Data.Monoid
import Data.Proxy
import Data.Set (Set)
import Data.Text hiding (head, lines, unlines)
import Data.Time
import Elm
import GHC.Generics
import Test.HUnit (Assertion, assertBool)
import Test.Hspec hiding (Spec)
import Test.Hspec as Hspec
import Text.Printf

-- Debugging hint:
-- ghci> import GHC.Generics
-- ghci> :kind! Rep Post
-- ...
data Post = Post
  { id :: Int,
    name :: String,
    age :: Maybe Double,
    comments :: [Comment],
    promoted :: Maybe Comment,
    author :: Maybe String
  }
  deriving (Generic, ElmType)

data Comment = Comment
  { postId :: Int,
    text :: Text,
    mainCategories :: (String, String),
    published :: Bool,
    created :: UTCTime,
    tags :: Map String Int
  }
  deriving (Generic, ElmType)

data Position
  = Beginning
  | Middle
  | End
  deriving (Generic, ElmType)

data Timing
  = Start
  | Continue Double
  | Stop
  deriving (Generic, ElmType)

newtype Id = Id Int
  deriving (Generic, ElmType, HasElmSorter, Aeson.ToJSON, Aeson.ToJSONKey)

data School = School
  { schoolId :: Id,
    schoolName :: String
  }
  deriving (Generic, Aeson.ToJSON, Aeson.ToJSONKey, ElmType)

instance HasElmSorter School where
  elmSorter = mkRecordSorter @"schoolId"

data Color
  = Red
  | Green
  | Blue
  deriving (Show, Read, Eq, Ord, Generic, Aeson.ToJSON, ElmType)

instance HasElmSorter Color where
  elmSorter _ = mkCustom "colorSorter"

instance Aeson.ToJSONKey Color where
  toJSONKey = Aeson.toJSONKeyText (pack . show)

newtype AnotherId = AnotherId Int
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (ElmType, HasElmSorter, Aeson.ToJSON, Aeson.ToJSONKey)

data Monstrosity
  = NotSpecial
  | OkayIGuess Monstrosity
  | Ridiculous Int String [Monstrosity] (Set Float)
  | Dicts (Map Int64 ()) (Map Float Float)
  | SortDicts (Map Id Text) (Map School ()) (Map Color ()) (Map AnotherId Text)
  | SortSet (Set School) (Set AnotherId)
  deriving (Generic, ElmType)

newtype Useless
  = Useless ()
  deriving (Generic, ElmType)

data Unit = Unit
  deriving (Generic, ElmType)

newtype Wrapper = Wrapper Int
  deriving (Generic, ElmType)

newtype FavoritePlaces = FavoritePlaces
  { positionsByUser :: Map String [Position]
  }
  deriving (Generic, ElmType)

-- | We don't actually use this type, we just need to see that it compiles.
data LotsOfInts = LotsOfInts
  { intA :: Int8,
    intB :: Int16,
    intC :: Int32,
    intD :: Int64
  }
  deriving (Generic, ElmType)

data Shadowing = Shadowing
  { prop :: ((Int, Int), (String, String))
  }
  deriving (Generic, ElmType)

data AesonValue = AesonValue
  { aesonValue :: Aeson.Value
  }
  deriving (Generic, ElmType)

data TFromElm = A | B | C | D

instance ElmType TFromElm where
  toElmType = fromElm (ElmRefData {typeName = "FromElm", decoderFunction = "existingDecodeFromElm", encoderFunction = "existingEncodeFromElm"})

data FieldWithFromElm = FieldWithFromElm
  { fieldWithFromElm :: TFromElm
  }
  deriving (Generic, ElmType)

spec :: Hspec.Spec
spec = do
  toElmTypeSpec
  toElmDecoderSpec
  toElmEncoderSpec
  toElmSorterSpec
  moduleSpecsSpec

toElmSorterSpec :: Hspec.Spec
toElmSorterSpec =
  describe "Generate Elm sorters" $ do
    it "toElmSorterSource School" $
      shouldMatchSorterSource
        (unlines ["module SchoolSorter exposing (..)", "", "", "%s"])
        (Proxy :: Proxy School)
        "test/SchoolSorter.elm"
    it "toElmSorterSource Id" $
      shouldMatchSorterSource
        (unlines ["module IdSorter exposing (..)", "", "", "%s"])
        (Proxy :: Proxy School)
        "test/IdSorter.elm"

toElmTypeSpec :: Hspec.Spec
toElmTypeSpec =
  describe "Convert to Elm types." $ do
    it "toElmTypeSource Post" $
      shouldMatchTypeSource
        ( unlines
            [ "module PostType exposing (..)",
              "",
              "import CommentType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Post)
        "test/PostType.elm"
    it "toElmTypeSource Comment" $
      shouldMatchTypeSource
        ( unlines
            [ "module CommentType exposing (Comment)",
              "",
              "import Dict exposing (Dict)",
              "import Time",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Comment)
        "test/CommentType.elm"
    it "toElmTypeSource Position" $
      shouldMatchTypeSource
        (unlines ["module PositionType exposing (..)", "", "", "%s"])
        defaultOptions
        (Proxy :: Proxy Position)
        "test/PositionType.elm"
    it "toElmTypeSource Timing" $
      shouldMatchTypeSource
        (unlines ["module TimingType exposing (..)", "", "", "%s"])
        defaultOptions
        (Proxy :: Proxy Timing)
        "test/TimingType.elm"
    it "toElmTypeSource Monstrosity" $
      shouldMatchTypeSource
        (unlines ["module MonstrosityType exposing (..)", "", "", "%s"])
        defaultOptions
        (Proxy :: Proxy Monstrosity)
        "test/MonstrosityType.elm"
    it "toElmTypeSource Useless" $
      shouldMatchTypeSource
        (unlines ["module UselessType exposing (..)", "", "", "%s"])
        defaultOptions
        (Proxy :: Proxy Useless)
        "test/UselessType.elm"
    it "toElmTypeSource Unit" $
      shouldMatchTypeSource
        (unlines ["module UnitType exposing (..)", "", "", "%s"])
        defaultOptions
        (Proxy :: Proxy Unit)
        "test/UnitType.elm"
    it "toElmTypeSource Wrapper" $
      shouldMatchTypeSource
        (unlines ["module WrapperType exposing (..)", "", "", "%s"])
        defaultOptions
        (Proxy :: Proxy Wrapper)
        "test/WrapperType.elm"
    it "toElmTypeSource FavoritePlaces" $
      shouldMatchTypeSource
        ( unlines
            [ "module FavoritePlacesType exposing (..)",
              "",
              "import Dict exposing (..)",
              "import PositionType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy FavoritePlaces)
        "test/FavoritePlacesType.elm"
    it "toElmTypeSourceWithOptions Post" $
      shouldMatchTypeSource
        ( unlines
            [ "module PostTypeWithOptions exposing (..)",
              "",
              "import CommentType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        (defaultOptions {fieldLabelModifier = withPrefix "post"})
        (Proxy :: Proxy Post)
        "test/PostTypeWithOptions.elm"
    it "toElmTypeSourceWithOptions Comment" $
      shouldMatchTypeSource
        ( unlines
            [ "module CommentTypeWithOptions exposing (Comment)",
              "",
              "import Dict exposing (Dict)",
              "import Time",
              "",
              "",
              "%s"
            ]
        )
        (defaultOptions {fieldLabelModifier = withPrefix "comment"})
        (Proxy :: Proxy Comment)
        "test/CommentTypeWithOptions.elm"
    it "toElmTypeSource Shadowing" $
      shouldMatchTypeSource
        ( unlines
            [ "module ShadowingType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Shadowing)
        "test/ShadowingType.elm"
    it "toElmTypeSource AesonValue" $
      shouldMatchTypeSource
        ( unlines
            [ "module AesonValueType exposing (..)",
              "",
              "import Json.Decode",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy AesonValue)
        "test/AesonValueType.elm"
    it "toElmTypeSource FieldWithFromElm" $
      shouldMatchTypeSource
        ( unlines
            [ "module FieldWithFromElmType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy FieldWithFromElm)
        "test/FieldWithFromElmType.elm"
    describe "Convert to Elm type references." $ do
      it "toElmTypeRef Post" $
        toElmTypeRef (Proxy :: Proxy Post) `shouldBe` "Post"
      it "toElmTypeRef [Comment]" $
        toElmTypeRef (Proxy :: Proxy [Comment]) `shouldBe` "List (Comment)"
      it "toElmTypeRef (Comment, String)" $
        toElmTypeRef (Proxy :: Proxy (Comment, String))
          `shouldBe` "(Comment, String)"
      it "toElmTypeRef String" $
        toElmTypeRef (Proxy :: Proxy String) `shouldBe` "String"
      it "toElmTypeRef (Maybe String)" $
        toElmTypeRef (Proxy :: Proxy (Maybe String)) `shouldBe` "Maybe (String)"
      it "toElmTypeRef [Maybe String]" $
        toElmTypeRef (Proxy :: Proxy [Maybe String])
          `shouldBe` "List (Maybe (String))"
      it "toElmTypeRef (Map String (Maybe String))" $
        toElmTypeRef (Proxy :: Proxy (Map String (Maybe String)))
          `shouldBe` "Dict (String) (Maybe (String))"
      it "toElmTypeRef (IntMap (Maybe String))" $
        toElmTypeRef (Proxy :: Proxy (IntMap (Maybe String)))
          `shouldBe` "Dict (Int) (Maybe (String))"
      it "toElmTypeRef TFromElm" $
        toElmTypeRef (Proxy :: Proxy TFromElm) `shouldBe` "FromElm"

toElmDecoderSpec :: Hspec.Spec
toElmDecoderSpec =
  describe "Convert to Elm decoders." $ do
    it "toElmDecoderSource Comment" $
      shouldMatchDecoderSource
        ( unlines
            [ "module CommentDecoder exposing (..)",
              "",
              "import CommentType exposing (..)",
              "import Dict",
              "import Iso8601",
              "import Json.Decode exposing (..)",
              "import Json.Decode.Pipeline exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Comment)
        "test/CommentDecoder.elm"
    it "toElmDecoderSource Post" $
      shouldMatchDecoderSource
        ( unlines
            [ "module PostDecoder exposing (..)",
              "",
              "import CommentDecoder exposing (..)",
              "import Json.Decode exposing (..)",
              "import Json.Decode.Pipeline exposing (..)",
              "import PostType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Post)
        "test/PostDecoder.elm"
    it "toElmDecoderSourceWithOptions Post" $
      shouldMatchDecoderSource
        ( unlines
            [ "module PostDecoderWithOptions exposing (..)",
              "",
              "import CommentDecoder exposing (..)",
              "import Json.Decode exposing (..)",
              "import Json.Decode.Pipeline exposing (..)",
              "import PostType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        (defaultOptions {fieldLabelModifier = withPrefix "post"})
        (Proxy :: Proxy Post)
        "test/PostDecoderWithOptions.elm"
    it "toElmDecoderSource Position" $
      shouldMatchDecoderSource
        ( unlines
            [ "module PositionDecoder exposing (..)",
              "",
              "import Json.Decode exposing (..)",
              "import Json.Decode.Pipeline exposing (..)",
              "import PositionType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Position)
        "test/PositionDecoder.elm"
    it "toElmDecoderSource Timing" $
      shouldMatchDecoderSource
        ( unlines
            [ "module TimingDecoder exposing (..)",
              "",
              "import Json.Decode exposing (..)",
              "import Json.Decode.Pipeline exposing (..)",
              "import TimingType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Timing)
        "test/TimingDecoder.elm"
    it "toElmDecoderSource Monstrosity" $
      shouldMatchDecoderSource
        ( unlines
            [ "module MonstrosityDecoder exposing (..)",
              "",
              "import Json.Decode exposing (..)",
              "import Json.Decode.Pipeline exposing (..)",
              "import MonstrosityType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Monstrosity)
        "test/MonstrosityDecoder.elm"
    it "toElmDecoderSourceWithOptions Comment" $
      shouldMatchDecoderSource
        ( unlines
            [ "module CommentDecoderWithOptions exposing (..)",
              "",
              "import CommentType exposing (..)",
              "import Dict",
              "import Iso8601",
              "import Json.Decode exposing (..)",
              "import Json.Decode.Pipeline exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        (defaultOptions {fieldLabelModifier = withPrefix "comment"})
        (Proxy :: Proxy Comment)
        "test/CommentDecoderWithOptions.elm"
    it "toElmDecoderSource Useless" $
      shouldMatchDecoderSource
        ( unlines
            [ "module UselessDecoder exposing (..)",
              "",
              "import Json.Decode exposing (..)",
              "import Json.Decode.Pipeline exposing (..)",
              "import UselessType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Useless)
        "test/UselessDecoder.elm"
    it "toElmDecoderSource Unit" $
      shouldMatchDecoderSource
        ( unlines
            [ "module UnitDecoder exposing (..)",
              "",
              "import Json.Decode exposing (..)",
              "import Json.Decode.Pipeline exposing (..)",
              "import UnitType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Unit)
        "test/UnitDecoder.elm"
    it "toElmDecoderSource Wrapper" $
      shouldMatchDecoderSource
        ( unlines
            [ "module WrapperDecoder exposing (..)",
              "",
              "import Json.Decode exposing (..)",
              "import Json.Decode.Pipeline exposing (..)",
              "import WrapperType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Wrapper)
        "test/WrapperDecoder.elm"
    it "toElmDecoderSource Shadowing" $
      shouldMatchDecoderSource
        ( unlines
            [ "module ShadowingDecoder exposing (..)",
              "",
              "import Json.Decode exposing (..)",
              "import Json.Decode.Pipeline exposing (..)",
              "import ShadowingType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Shadowing)
        "test/ShadowingDecoder.elm"
    it "toElmDecoderSource AesonValue" $
      shouldMatchDecoderSource
        ( unlines
            [ "module AesonValueDecoder exposing (..)",
              "",
              "import Json.Decode exposing (..)",
              "import Json.Decode.Pipeline exposing (..)",
              "import AesonValueType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy AesonValue)
        "test/AesonValueDecoder.elm"
    it "toElmDecoderSource FieldWithFromElm" $
      shouldMatchDecoderSource
        ( unlines
            [ "module FieldWithFromElmDecoder exposing (..)",
              "",
              "import Json.Decode exposing (..)",
              "import Json.Decode.Pipeline exposing (..)",
              "import FieldWithFromElmType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy FieldWithFromElm)
        "test/FieldWithFromElmDecoder.elm"
    describe "Convert to Elm decoder references." $ do
      it "toElmDecoderRef Post" $
        toElmDecoderRef (Proxy :: Proxy Post) `shouldBe` "decodePost"
      it "toElmDecoderRef Position" $
        toElmDecoderRef (Proxy :: Proxy Position) `shouldBe` "decodePosition"
      it "toElmDecoderRef Timing" $
        toElmDecoderRef (Proxy :: Proxy Timing) `shouldBe` "decodeTiming"
      it "toElmDecoderRef Monstrosity" $
        toElmDecoderRef (Proxy :: Proxy Monstrosity) `shouldBe` "decodeMonstrosity"
      it "toElmDecoderRef [Comment]" $
        toElmDecoderRef (Proxy :: Proxy [Comment])
          `shouldBe` "(list decodeComment)"
      it "toElmDecoderRef String" $
        toElmDecoderRef (Proxy :: Proxy String) `shouldBe` "string"
      it "toElmDecoderRef (Maybe String)" $
        toElmDecoderRef (Proxy :: Proxy (Maybe String))
          `shouldBe` "(nullable string)"
      it "toElmDecoderRef [Maybe String]" $
        toElmDecoderRef (Proxy :: Proxy [Maybe String])
          `shouldBe` "(list (nullable string))"
      it "toElmDecoderRef (Map String (Maybe String))" $
        toElmDecoderRef (Proxy :: Proxy (Map String (Maybe String)))
          `shouldBe` "(dict (nullable string))"
      it "toElmDecoderRef (IntMap (Maybe String))" $
        toElmDecoderRef (Proxy :: Proxy (IntMap (Maybe String)))
          `shouldBe` "(Json.Decode.Extra.dict2 int (nullable string))"
      it "toElmDecoderRef TFromElm" $
        toElmDecoderRef (Proxy :: Proxy TFromElm) `shouldBe` "existingDecodeFromElm"

toElmEncoderSpec :: Hspec.Spec
toElmEncoderSpec =
  describe "Convert to Elm encoders." $ do
    it "toElmEncoderSource Comment" $
      shouldMatchEncoderSource
        ( unlines
            [ "module CommentEncoder exposing (..)",
              "",
              "import CommentType exposing (..)",
              "import Iso8601",
              "import Json.Encode",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Comment)
        "test/CommentEncoder.elm"
    it "toElmEncoderSource Post" $
      shouldMatchEncoderSource
        ( unlines
            [ "module PostEncoder exposing (..)",
              "",
              "import CommentEncoder exposing (..)",
              "import Json.Encode",
              "import PostType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Post)
        "test/PostEncoder.elm"
    it "toElmEncoderSourceWithOptions Comment" $
      shouldMatchEncoderSource
        ( unlines
            [ "module CommentEncoderWithOptions exposing (..)",
              "",
              "import CommentType exposing (..)",
              "import Iso8601",
              "import Json.Encode",
              "",
              "",
              "%s"
            ]
        )
        (defaultOptions {fieldLabelModifier = withPrefix "comment"})
        (Proxy :: Proxy Comment)
        "test/CommentEncoderWithOptions.elm"
    it "toElmEncoderSourceWithOptions Post" $
      shouldMatchEncoderSource
        ( unlines
            [ "module PostEncoderWithOptions exposing (..)",
              "",
              "import CommentEncoder exposing (..)",
              "import Json.Encode",
              "import PostType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        (defaultOptions {fieldLabelModifier = withPrefix "post"})
        (Proxy :: Proxy Post)
        "test/PostEncoderWithOptions.elm"
    it "toElmEncoderSource Position" $
      shouldMatchEncoderSource
        ( unlines
            [ "module PositionEncoder exposing (..)",
              "",
              "import Json.Encode",
              "import PositionType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Position)
        "test/PositionEncoder.elm"
    it "toElmEncoderSource Position" $
      shouldMatchEncoderSource
        ( unlines
            [ "module ShadowingEncoder exposing (..)",
              "",
              "import Json.Encode",
              "import ShadowingType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Shadowing)
        "test/ShadowingEncoder.elm"
    it "toElmEncoderSourceWithOptions Timing" $
      shouldMatchEncoderSource
        ( unlines
            [ "module TimingEncoder exposing (..)",
              "",
              "import Json.Encode",
              "import TimingType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Timing)
        "test/TimingEncoder.elm"
    it "toElmEncoderSourceWithOptions Monstrosity" $
      shouldMatchEncoderSource
        ( unlines
            [ "module MonstrosityEncoder exposing (..)",
              "",
              "import Json.Encode",
              "import MonstrosityType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Monstrosity)
        "test/MonstrosityEncoder.elm"
    it "toElmEncoderSourceWithOptions Useless" $
      shouldMatchEncoderSource
        ( unlines
            [ "module UselessEncoder exposing (..)",
              "",
              "import Json.Encode",
              "import UselessType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Useless)
        "test/UselessEncoder.elm"
    it "toElmEncoderSourceWithOptions Unit" $
      shouldMatchEncoderSource
        ( unlines
            [ "module UnitEncoder exposing (..)",
              "",
              "import Json.Encode",
              "import UnitType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Unit)
        "test/UnitEncoder.elm"
    it "toElmEncoderSourceWithOptions Wrapper" $
      shouldMatchEncoderSource
        ( unlines
            [ "module WrapperEncoder exposing (..)",
              "",
              "import Json.Encode",
              "import WrapperType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy Wrapper)
        "test/WrapperEncoder.elm"
    it "toElmEncoderSourceWithOptions AesonValue" $
      shouldMatchEncoderSource
        ( unlines
            [ "module AesonValueEncoder exposing (..)",
              "",
              "import Json.Encode",
              "import AesonValueType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy AesonValue)
        "test/AesonValueEncoder.elm"
    it "toElmEncoderSourceWithOptions FieldWithFromElm" $
      shouldMatchEncoderSource
        ( unlines
            [ "module FieldWithFromElmEncoder exposing (..)",
              "",
              "import Json.Encode",
              "import FieldWithFromElmType exposing (..)",
              "",
              "",
              "%s"
            ]
        )
        defaultOptions
        (Proxy :: Proxy FieldWithFromElm)
        "test/FieldWithFromElmEncoder.elm"
    describe "Convert to Elm encoder references." $ do
      it "toElmEncoderRef Post" $
        toElmEncoderRef (Proxy :: Proxy Post) `shouldBe` "encodePost"
      it "toElmEncoderRef [Comment]" $
        toElmEncoderRef (Proxy :: Proxy [Comment])
          `shouldBe` "(Json.Encode.list encodeComment)"
      it "toElmEncoderRef Position" $
        toElmEncoderRef (Proxy :: Proxy Position) `shouldBe` "encodePosition"
      it "toElmEncoderRef Timing" $
        toElmEncoderRef (Proxy :: Proxy Timing) `shouldBe` "encodeTiming"
      it "toElmEncoderRef Monstrosity" $
        toElmEncoderRef (Proxy :: Proxy Monstrosity) `shouldBe` "encodeMonstrosity"
      it "toElmEncoderRef String" $
        toElmEncoderRef (Proxy :: Proxy String) `shouldBe` "Json.Encode.string"
      it "toElmEncoderRef (Maybe String)" $
        toElmEncoderRef (Proxy :: Proxy (Maybe String))
          `shouldBe` "(Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string)"
      it "toElmEncoderRef [Maybe String]" $
        toElmEncoderRef (Proxy :: Proxy [Maybe String])
          `shouldBe` "(Json.Encode.list (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string))"
      it "toElmEncoderRef (Map String (Maybe String))" $
        toElmEncoderRef (Proxy :: Proxy (Map String (Maybe String)))
          `shouldBe` "(Json.Encode.dict identity (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string))"
      it "toElmEncoderRef (IntMap (Maybe String))" $
        toElmEncoderRef (Proxy :: Proxy (IntMap (Maybe String)))
          `shouldBe` "(Json.Encode.dict String.fromInt (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string))"
      it "toElmEncoderRef TFromElm" $
        toElmEncoderRef (Proxy :: Proxy TFromElm) `shouldBe` "existingEncodeFromElm"

moduleSpecsSpec :: Hspec.Spec
moduleSpecsSpec =
  describe "Generating a module Spec" $ do
    let mySpec =
          moduleSpec ["My", "Module"] $ do
            renderType (Proxy :: Proxy Post)
            renderDecoder (Proxy :: Proxy Post)
            renderType (Proxy :: Proxy Comment)
    it "sets the module namespace" $
      namespace mySpec `shouldBe` ["My", "Module"]
    it "inserts the correct imports" $
      head (declarations mySpec)
        `shouldBe` intercalate
          "\n"
          [ "import Dict",
            "import Json.Decode exposing (..)",
            "import Json.Decode.Pipeline exposing (..)",
            "import Time"
          ]

shouldMatchSorterSource ::
  (ElmType a, HasElmSorter a) =>
  String ->
  Proxy a ->
  FilePath ->
  IO ()
shouldMatchSorterSource wrapping x =
  shouldMatchFile . printf wrapping $ toElmSorterSource x

shouldMatchTypeSource ::
  (ElmType a) =>
  String ->
  Options ->
  a ->
  FilePath ->
  IO ()
shouldMatchTypeSource wrapping options x =
  shouldMatchFile . printf wrapping $ toElmTypeSourceWith options x

shouldMatchDecoderSource ::
  (ElmType a) =>
  String ->
  Options ->
  a ->
  FilePath ->
  IO ()
shouldMatchDecoderSource wrapping options x =
  shouldMatchFile . printf wrapping $ toElmDecoderSourceWith options x

shouldMatchEncoderSource ::
  (ElmType a) =>
  String ->
  Options ->
  a ->
  FilePath ->
  IO ()
shouldMatchEncoderSource wrapping options x =
  shouldMatchFile . printf wrapping $ toElmEncoderSourceWith options x

shouldMatchFile :: String -> FilePath -> IO ()
shouldMatchFile actual fileExpected = do
  source <- readFile fileExpected
  actual `shouldBeDiff` (fileExpected, source)

shouldBeDiff :: String -> (String, String) -> Assertion
shouldBeDiff a (fpath, b) =
  assertBool
    ( "< generated\n"
        <> "> "
        <> fpath
        <> "\n"
        <> DiffOutput.ppDiff (Diff.getGroupedDiff (lines a) (lines b))
    )
    (a == b)

initCap :: Text -> Text
initCap t =
  case uncons t of
    Nothing -> t
    Just (c, cs) -> cons (Data.Char.toUpper c) cs

withPrefix :: Text -> Text -> Text
withPrefix prefix s = prefix <> initCap s
