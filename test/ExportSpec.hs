{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ExportSpec where

import           Data.Char
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

data Post =
  Post {id       :: Int
       ,name     :: String
       ,age      :: Maybe Double
       ,comments :: [Comment]
       ,promoted :: Maybe Comment
       ,author   :: Maybe String}
  deriving (Generic,ElmType)

data Comment =
  Comment {postId         :: Int
          ,text           :: Text
          ,mainCategories :: (String,String)
          ,published      :: Bool
          ,created        :: UTCTime
          ,tags           :: Map String Int}
  deriving (Generic,ElmType)

data Position
  = Beginning
  | Middle
  | End
  deriving (Generic,ElmType)

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
                  ,"import Json.Encode exposing (..)"
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
                  ,"import Json.Encode exposing (..)"
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
                  ,"import Json.Encode exposing (..)"
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
                  ,"import Json.Encode exposing (..)"
                  ,"import PostType exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         (defaultOptions {fieldLabelModifier = withPrefix "post"})
         (Proxy :: Proxy Post)
         "test/PostEncoderWithOptions.elm"

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
