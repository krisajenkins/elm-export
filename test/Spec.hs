{-# LANGUAGE DeriveGeneric #-}

import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           Elm
import           GHC.Generics
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

data Post =
  Post {id       :: Int
       ,name     :: String
       ,age      :: Maybe Double
       ,comments :: [Comment]
       ,promoted :: Maybe Comment
       ,author   :: Maybe String}
  deriving Generic

data Comment =
  Comment {postId    :: Int
          ,text      :: Text
          ,published :: Bool}
  deriving (Generic)

instance ToElmType Post
instance ToElmType Comment

main :: IO ()
main =
  defaultMainWithOpts
    [testCase "toElmTypeSource" testToElmTypeSource
    ,testCase "toElmDecoderSource" testToElmDecoderSource
    ,testCase "toElmEncoderSource" testToElmEncoderSource]
    mempty

testToElmTypeSource :: Assertion
testToElmTypeSource =
  do postSource <- readFile "test/PostType.elm"
     assertEqual "Encoding a Post type" postSource $
       toElmTypeSource (Proxy :: Proxy Post) ++ "\n"
     commentSource <- readFile "test/CommentType.elm"
     assertEqual "Encoding a Comment type" commentSource $
       toElmTypeSource (Proxy :: Proxy Comment) ++ "\n"

testToElmDecoderSource :: Assertion
testToElmDecoderSource =
  do postSource <- readFile "test/PostDecoder.elm"
     assertEqual "Encoding a Post decoder" postSource $
       toElmDecoderSource (Proxy :: Proxy Post) ++ "\n"
     commentSource <- readFile "test/CommentDecoder.elm"
     assertEqual "Encoding a Comment decoder" commentSource $
       toElmDecoderSource (Proxy :: Proxy Comment) ++ "\n"

testToElmEncoderSource :: Assertion
testToElmEncoderSource =
  do postSource <- readFile "test/PostEncoder.elm"
     assertEqual "Encoding a Post encoder" postSource $
       toElmEncoderSource (Proxy :: Proxy Post) ++ "\n"
     commentSource <- readFile "test/CommentEncoder.elm"
     assertEqual "Encoding a Comment encoder" commentSource $
       toElmEncoderSource (Proxy :: Proxy Comment) ++ "\n"
