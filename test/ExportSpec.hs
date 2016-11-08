{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ExportSpec where

import           Data.Char
import           Data.Int
import           Data.Map
import           Data.Monoid
import           Data.Proxy
import           Data.Text    hiding (unlines)
import           Data.Time
import           Elm
import           GHC.Generics
import           Test.Hspec   hiding (Spec)
import           Test.Hspec   as Hspec
import           Text.Printf

-- Debugging hint:
-- ghci> import GHC.Generics
-- ghci> :kind! Rep Post
-- ...

data Post = Post
    { id       :: Int
    , name     :: String
    , age      :: Maybe Double
    , comments :: [Comment]
    , promoted :: Maybe Comment
    , author   :: Maybe String
    } deriving (Generic, ElmType)

data Comment = Comment
    { postId         :: Int
    , text           :: Text
    , mainCategories :: (String, String)
    , published      :: Bool
    , created        :: UTCTime
    , tags           :: Map String Int
    } deriving (Generic, ElmType)

data Position
  = Beginning
  | Middle
  | End
  deriving (Generic,ElmType)

data Timing
  = Start
  | Continue Double
  | Stop
  deriving (Generic,ElmType)

newtype Useless =
    Useless ()
     deriving (Generic, ElmType)

newtype FavoritePlaces =
  FavoritePlaces {positionsByUser :: Map String [Position]}
  deriving (Generic,ElmType)

-- | We don't actually use this type, we just need to see that it compiles.
data LotsOfInts = LotsOfInts
    { intA :: Int8
    , intB :: Int16
    , intC :: Int32
    , intD :: Int64
    } deriving (Generic, ElmType)

spec :: Hspec.Spec
spec =
  do toElmTypeSpec
     toElmDecoderSpec
     toElmEncoderSpec

toElmTypeSpec :: Hspec.Spec
toElmTypeSpec =
  describe "Convert to Elm types." $
  do it "toElmTypeSource Post" $
       shouldMatchTypeSource
         (unlines ["module PostType exposing (..)"
                  ,""
                  ,"import CommentType exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         defaultOptions
         (Proxy :: Proxy Post)
         "test/PostType.elm"
     it "toElmTypeSource Comment" $
       shouldMatchTypeSource
         (unlines ["module CommentType exposing (..)"
                  ,""
                  ,"import Date exposing (Date)"
                  ,"import Dict exposing (Dict)"
                  ,""
                  ,""
                  ,"%s"])
         defaultOptions
         (Proxy :: Proxy Comment)
         "test/CommentType.elm"
     it "toElmTypeSource Position" $
       shouldMatchTypeSource
         (unlines ["module PositionType exposing (..)","","","%s"])
         defaultOptions
         (Proxy :: Proxy Position)
         "test/PositionType.elm"
     it "toElmTypeSource Timing" $
       shouldMatchTypeSource
         (unlines ["module TimingType exposing (..)","","","%s"])
         defaultOptions
         (Proxy :: Proxy Timing)
         "test/TimingType.elm"
     it "toElmTypeSource Useless" $
       shouldMatchTypeSource
         (unlines ["module UselessType exposing (..)","","","%s"])
         defaultOptions
         (Proxy :: Proxy Useless)
         "test/UselessType.elm"
     it "toElmTypeSource FavoritePlaces" $
       shouldMatchTypeSource
         (unlines ["module FavoritePlacesType exposing (..)"
                  ,""
                  ,"import PositionType exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         defaultOptions
         (Proxy :: Proxy FavoritePlaces)
         "test/FavoritePlacesType.elm"
     it "toElmTypeSourceWithOptions Post" $
       shouldMatchTypeSource
         (unlines ["module PostTypeWithOptions exposing (..)"
                  ,""
                  ,"import CommentType exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         (defaultOptions {fieldLabelModifier = withPrefix "post"})
         (Proxy :: Proxy Post)
         "test/PostTypeWithOptions.elm"
     it "toElmTypeSourceWithOptions Comment" $
       shouldMatchTypeSource
         (unlines ["module CommentTypeWithOptions exposing (..)"
                  ,""
                  ,"import Date exposing (Date)"
                  ,"import Dict exposing (Dict)"
                  ,""
                  ,""
                  ,"%s"])
         (defaultOptions {fieldLabelModifier = withPrefix "comment"})
         (Proxy :: Proxy Comment)
         "test/CommentTypeWithOptions.elm"
     describe "Convert to Elm type references." $
       do it "toElmTypeRef Post" $
            toElmTypeRef (Proxy :: Proxy Post)
            `shouldBe` "Post"
          it "toElmTypeRef [Comment]" $
            toElmTypeRef (Proxy :: Proxy [Comment])
            `shouldBe` "List (Comment)"
          it "toElmTypeRef String" $
            toElmTypeRef (Proxy :: Proxy String)
            `shouldBe` "String"
          it "toElmTypeRef (Maybe String)" $
            toElmTypeRef (Proxy :: Proxy (Maybe String))
            `shouldBe` "Maybe (String)"
          it "toElmTypeRef [Maybe String]" $
            toElmTypeRef (Proxy :: Proxy [Maybe String])
            `shouldBe` "List (Maybe (String))"
          it "toElmTypeRef (Map String (Maybe String))" $
            toElmTypeRef (Proxy :: Proxy (Map String (Maybe String)))
            `shouldBe` "Dict (String) (Maybe (String))"

toElmDecoderSpec :: Hspec.Spec
toElmDecoderSpec =
  describe "Convert to Elm decoders." $
  do it "toElmDecoderSource Comment" $
       shouldMatchDecoderSource
         (unlines ["module CommentDecoder exposing (..)"
                  ,""
                  ,"import CommentType exposing (..)"
                  ,"import Date"
                  ,"import Dict"
                  ,"import Json.Decode exposing (..)"
                  ,"import Json.Decode.Pipeline exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         defaultOptions
         (Proxy :: Proxy Comment)
         "test/CommentDecoder.elm"
     it "toElmDecoderSource Post" $
       shouldMatchDecoderSource
         (unlines ["module PostDecoder exposing (..)"
                  ,""
                  ,"import CommentDecoder exposing (..)"
                  ,"import Json.Decode exposing (..)"
                  ,"import Json.Decode.Pipeline exposing (..)"
                  ,"import PostType exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         defaultOptions
         (Proxy :: Proxy Post)
         "test/PostDecoder.elm"
     it "toElmDecoderSourceWithOptions Post" $
       shouldMatchDecoderSource
         (unlines ["module PostDecoderWithOptions exposing (..)"
                  ,""
                  ,"import CommentDecoder exposing (..)"
                  ,"import Json.Decode exposing (..)"
                  ,"import Json.Decode.Pipeline exposing (..)"
                  ,"import PostType exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         (defaultOptions {fieldLabelModifier = withPrefix "post"})
         (Proxy :: Proxy Post)
         "test/PostDecoderWithOptions.elm"
     it "toElmDecoderSourceWithOptions Comment" $
       shouldMatchDecoderSource
         (unlines ["module CommentDecoderWithOptions exposing (..)"
                  ,""
                  ,"import CommentType exposing (..)"
                  ,"import Date"
                  ,"import Dict"
                  ,"import Json.Decode exposing (..)"
                  ,"import Json.Decode.Pipeline exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         (defaultOptions {fieldLabelModifier = withPrefix "comment"})
         (Proxy :: Proxy Comment)
         "test/CommentDecoderWithOptions.elm"
     describe "Convert to Elm decoder references." $
       do it "toElmDecoderRef Post" $
            toElmDecoderRef (Proxy :: Proxy Post)
            `shouldBe` "decodePost"
          it "toElmDecoderRef [Comment]" $
            toElmDecoderRef (Proxy :: Proxy [Comment])
            `shouldBe` "(list decodeComment)"
          it "toElmDecoderRef String" $
            toElmDecoderRef (Proxy :: Proxy String)
            `shouldBe` "string"
          it "toElmDecoderRef (Maybe String)" $
            toElmDecoderRef (Proxy :: Proxy (Maybe String))
            `shouldBe` "(maybe string)"
          it "toElmDecoderRef [Maybe String]" $
            toElmDecoderRef (Proxy :: Proxy [Maybe String])
            `shouldBe` "(list (maybe string))"
          it "toElmDecoderRef (Map String (Maybe String))" $
            toElmDecoderRef (Proxy :: Proxy (Map String (Maybe String)))
            `shouldBe` "(map Dict.fromList (list (tuple2 (,) string (maybe string))))"

toElmEncoderSpec :: Hspec.Spec
toElmEncoderSpec =
  describe "Convert to Elm encoders." $
  do it "toElmEncoderSource Comment" $
       shouldMatchEncoderSource
         (unlines ["module CommentEncoder exposing (..)"
                  ,""
                  ,"import CommentType exposing (..)"
                  ,"import Exts.Date exposing (..)"
                  ,"import Exts.Json.Encode exposing (..)"
                  ,"import Json.Encode"
                  ,""
                  ,""
                  ,"%s"])
         defaultOptions
         (Proxy :: Proxy Comment)
         "test/CommentEncoder.elm"
     it "toElmEncoderSource Post" $
       shouldMatchEncoderSource
         (unlines ["module PostEncoder exposing (..)"
                  ,""
                  ,"import CommentEncoder exposing (..)"
                  ,"import Json.Encode"
                  ,"import PostType exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         defaultOptions
         (Proxy :: Proxy Post)
         "test/PostEncoder.elm"
     it "toElmEncoderSourceWithOptions Comment" $
       shouldMatchEncoderSource
         (unlines ["module CommentEncoderWithOptions exposing (..)"
                  ,""
                  ,"import CommentType exposing (..)"
                  ,"import Exts.Date exposing (..)"
                  ,"import Exts.Json.Encode exposing (..)"
                  ,"import Json.Encode"
                  ,""
                  ,""
                  ,"%s"])
         (defaultOptions {fieldLabelModifier = withPrefix "comment"})
         (Proxy :: Proxy Comment)
         "test/CommentEncoderWithOptions.elm"
     it "toElmEncoderSourceWithOptions Post" $
       shouldMatchEncoderSource
         (unlines ["module PostEncoderWithOptions exposing (..)"
                  ,""
                  ,"import CommentEncoder exposing (..)"
                  ,"import Json.Encode"
                  ,"import PostType exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         (defaultOptions {fieldLabelModifier = withPrefix "post"})
         (Proxy :: Proxy Post)
         "test/PostEncoderWithOptions.elm"
     describe "Convert to Elm encoder references." $
       do it "toElmEncoderRef Post" $
            toElmEncoderRef (Proxy :: Proxy Post)
            `shouldBe` "encodePost"
          it "toElmEncoderRef [Comment]" $
            toElmEncoderRef (Proxy :: Proxy [Comment])
            `shouldBe` "(Json.Encode.list << List.map encodeComment)"
          it "toElmEncoderRef String" $
            toElmEncoderRef (Proxy :: Proxy String)
            `shouldBe` "Json.Encode.string"
          it "toElmEncoderRef (Maybe String)" $
            toElmEncoderRef (Proxy :: Proxy (Maybe String))
            `shouldBe` "(Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string)"
          it "toElmEncoderRef [Maybe String]" $
            toElmEncoderRef (Proxy :: Proxy [Maybe String])
            `shouldBe` "(Json.Encode.list << List.map (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string))"
          it "toElmEncoderRef (Map String (Maybe String))" $
            toElmEncoderRef (Proxy :: Proxy (Map String (Maybe String)))
            `shouldBe` "(dict Json.Encode.string (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string))"

shouldMatchTypeSource
  :: ElmType a
  => String -> Options -> a -> FilePath -> IO ()
shouldMatchTypeSource wrapping options x =
  shouldMatchFile . printf wrapping $ toElmTypeSourceWith options x

shouldMatchDecoderSource
  :: ElmType a
  => String -> Options -> a -> FilePath -> IO ()
shouldMatchDecoderSource wrapping options x =
  shouldMatchFile . printf wrapping $ toElmDecoderSourceWith options x

shouldMatchEncoderSource
  :: ElmType a
  => String -> Options -> a -> FilePath -> IO ()
shouldMatchEncoderSource wrapping options x =
  shouldMatchFile . printf wrapping $ toElmEncoderSourceWith options x

shouldMatchFile :: String -> FilePath -> IO ()
shouldMatchFile actual fileExpected =
  do source <- readFile fileExpected
     actual `shouldBe` source

initCap :: Text -> Text
initCap t =
    case uncons t of
        Nothing -> t
        Just (c, cs) -> cons (Data.Char.toUpper c) cs

withPrefix :: Text -> Text -> Text
withPrefix prefix s = prefix <> ( initCap  s)
