{-# LANGUAGE DeriveGeneric #-}

module ExportSpec (spec, Post(..), Comment(..)) where

import           Data.Char
import           Data.Proxy
import           Data.Text
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
  deriving Generic

data Comment =
  Comment {postId    :: Int
          ,text      :: Text
          ,published :: Bool}
  deriving (Generic)

instance ElmType Post
instance ElmType Comment

spec :: Hspec.Spec
spec =
  do toElmTypeSpec
     toElmDecoderSpec
     toElmEncoderSpec

toElmTypeSpec :: Hspec.Spec
toElmTypeSpec =
  describe "Convert to Elm types." $
  do it "toElmTypeSource Post" $
       shouldMatchTypeSource (Proxy :: Proxy Post)
                             "test/PostType.elm"
     it "toElmTypeSource Comment" $
       shouldMatchTypeSource (Proxy :: Proxy Comment)
                             "test/CommentType.elm"

toElmDecoderSpec :: Hspec.Spec
toElmDecoderSpec =
  describe "Convert to Elm decoders." $
  do it "toElmDecoderSource Post" $
       shouldMatchDecoderSource (Proxy :: Proxy Post)
                                "test/PostDecoder.elm"
     it "toElmDecoderSource Comment" $
       shouldMatchDecoderSource (Proxy :: Proxy Comment)
                                "test/CommentDecoder.elm"

toElmEncoderSpec :: Hspec.Spec
toElmEncoderSpec =
  describe "Convert to Elm encoders." $
  do it "toElmEncoderSource Post" $
       shouldMatchEncoderSource (Proxy :: Proxy Post)
                                "test/PostEncoder.elm"
     it "toElmEncoderSource Comment" $
       shouldMatchEncoderSource (Proxy :: Proxy Comment)
                                "test/CommentEncoder.elm"

shouldMatchTypeSource :: ElmType a => a -> FilePath -> IO ()
shouldMatchTypeSource =
  shouldMatchFile . printf outputWrapping . toElmTypeSource

shouldMatchDecoderSource :: ElmType a => a -> FilePath -> IO ()
shouldMatchDecoderSource =
  shouldMatchFile . printf outputWrapping . toElmDecoderSource

shouldMatchEncoderSource :: ElmType a => a -> FilePath -> IO ()
shouldMatchEncoderSource =
  shouldMatchFile . printf outputWrapping . toElmEncoderSource

outputWrapping :: String
outputWrapping = "module Main (..) where\n\n\n%s\n"

shouldMatchFile :: String -> FilePath -> IO ()
shouldMatchFile actual fileExpected =
  do source <- readFile fileExpected
     actual `shouldBe` source

initCap :: String -> String
initCap [] = []
initCap (c:cs) = Data.Char.toUpper c : cs
