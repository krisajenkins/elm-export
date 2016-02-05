{-# LANGUAGE DeriveGeneric #-}


import           Data.Proxy
import           Data.Text
import           Elm
import           GHC.Generics
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import           Text.Printf

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
    [testCase "toElmTypeSource Post"
              (testToElmTypeSource (Proxy :: Proxy Post)
                                   "test/PostType.elm")
    ,testCase "toElmTypeSource Comment"
              (testToElmTypeSource (Proxy :: Proxy Comment)
                                   "test/CommentType.elm")
    ,testCase "toElmDecoderSource Post"
              (testToElmDecoderSource (Proxy :: Proxy Post)
                                      "test/PostDecoder.elm")
    ,testCase "toElmDecoderSource Comment"
              (testToElmDecoderSource (Proxy :: Proxy Comment)
                                      "test/CommentDecoder.elm")
    ,testCase "toElmEncoderSource Post"
              (testToElmEncoderSource (Proxy :: Proxy Post)
                                      "test/PostEncoder.elm")
    ,testCase "toElmEncoderSource Comment"
              (testToElmEncoderSource (Proxy :: Proxy Comment)
                                      "test/CommentEncoder.elm")]
    mempty

testToElmTypeSource :: ToElmType a => a -> FilePath -> IO ()
testToElmTypeSource proxy sourceFile =
  do source <- readFile sourceFile
     assertEqual "Encoding a type" source $
       printf "module Main (..) where\n\n\n%s\n" (toElmTypeSource proxy)

testToElmDecoderSource :: ToElmType a => a -> FilePath -> IO ()
testToElmDecoderSource proxy sourceFile =
  do source <- readFile sourceFile
     assertEqual "Encoding a decoder" source $
       printf "module Main (..) where\n\n\n%s\n" (toElmDecoderSource proxy)

testToElmEncoderSource :: ToElmType a => a -> FilePath -> IO ()
testToElmEncoderSource proxy sourceFile =
  do source <- readFile sourceFile
     assertEqual "Encoding a encoder" source $
       printf "module Main (..) where\n\n\n%s\n" (toElmEncoderSource proxy)
