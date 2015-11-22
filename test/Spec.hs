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
    ,testCase "toElmDecoderSource" testToElmDecoderSource]
    mempty

testToElmTypeSource :: Assertion
testToElmTypeSource =
  do postSource <- readFile "test/PostType.txt"
     assertEqual "Encoding a Post type" postSource $
       (toElmTypeSource (Proxy :: Proxy Post)) ++ "\n"
     commentSource <- readFile "test/CommentType.txt"
     assertEqual "Encoding a Comment type" commentSource $
       (toElmTypeSource (Proxy :: Proxy Comment)) ++ "\n"

testToElmDecoderSource :: Assertion
testToElmDecoderSource =
  do postSource <- readFile "test/PostDecoder.txt"
     assertEqual "Encoding a Post decoder" postSource $
       (toElmDecoderSource (Proxy :: Proxy Post)) ++ "\n"
     commentSource <- readFile "test/CommentDecoder.txt"
     assertEqual "Encoding a Comment decoder" commentSource $
       (toElmDecoderSource (Proxy :: Proxy Comment)) ++ "\n"
