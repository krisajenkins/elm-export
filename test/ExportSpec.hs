{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ExportSpec where

import           Data.Char
import           Data.Map
import           Data.Proxy
import           Data.Text
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
       ,author   :: Maybe String
       ,nothing  :: ()}
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
       shouldMatchTypeSource defaultOptions
                             (Proxy :: Proxy Post)
                             "test/PostType.elm"
     it "toElmTypeSource Comment" $
       shouldMatchTypeSource defaultOptions
                             (Proxy :: Proxy Comment)
                             "test/CommentType.elm"
     it "toElmTypeSource Position" $
       shouldMatchTypeSource defaultOptions
                             (Proxy :: Proxy Position)
                             "test/PositionType.elm"
     it "toElmTypeSourceWithOptions Post" $
       shouldMatchTypeSource
         (defaultOptions {fieldLabelModifier = withPrefix "post"})
         (Proxy :: Proxy Post)
         "test/PostTypeWithOptions.elm"
     it "toElmTypeSourceWithOptions Comment" $
       shouldMatchTypeSource
         (defaultOptions {fieldLabelModifier = withPrefix "comment"})
         (Proxy :: Proxy Comment)
         "test/CommentTypeWithOptions.elm"

toElmDecoderSpec :: Hspec.Spec
toElmDecoderSpec =
  describe "Convert to Elm decoders." $
  do it "toElmDecoderSource Post" $
       shouldMatchDecoderSource defaultOptions
                                (Proxy :: Proxy Post)
                                "test/PostDecoder.elm"
     it "toElmDecoderSource Comment" $
       shouldMatchDecoderSource defaultOptions
                                (Proxy :: Proxy Comment)
                                "test/CommentDecoder.elm"
     it "toElmDecoderSourceWithOptions Post" $
       shouldMatchDecoderSource
         (defaultOptions {fieldLabelModifier = withPrefix "post"})
         (Proxy :: Proxy Post)
         "test/PostDecoderWithOptions.elm"
     it "toElmDecoderSourceWithOptions Comment" $
       shouldMatchDecoderSource
         (defaultOptions {fieldLabelModifier = withPrefix "comment"})
         (Proxy :: Proxy Comment)
         "test/CommentDecoderWithOptions.elm"

toElmEncoderSpec :: Hspec.Spec
toElmEncoderSpec =
  describe "Convert to Elm encoders." $
  do it "toElmEncoderSource Post" $
       shouldMatchEncoderSource defaultOptions
                                (Proxy :: Proxy Post)
                                "test/PostEncoder.elm"
     it "toElmEncoderSource Comment" $
       shouldMatchEncoderSource defaultOptions
                                (Proxy :: Proxy Comment)
                                "test/CommentEncoder.elm"
     it "toElmEncoderSourceWithOptions Post" $
       shouldMatchEncoderSource
         (defaultOptions {fieldLabelModifier = withPrefix "post"})
         (Proxy :: Proxy Post)
         "test/PostEncoderWithOptions.elm"
     it "toElmEncoderSourceWithOptions Comment" $
       shouldMatchEncoderSource
         (defaultOptions {fieldLabelModifier = withPrefix "comment"})
         (Proxy :: Proxy Comment)
         "test/CommentEncoderWithOptions.elm"

shouldMatchTypeSource
  :: ElmType a
  => Options -> a -> FilePath -> IO ()
shouldMatchTypeSource options x =
  shouldMatchFile . printf outputWrapping $ toElmTypeSourceWith options x

shouldMatchDecoderSource
  :: ElmType a
  => Options -> a -> FilePath -> IO ()
shouldMatchDecoderSource options x =
  shouldMatchFile . printf outputWrapping $ toElmDecoderSourceWith options x

shouldMatchEncoderSource
  :: ElmType a
  => Options -> a -> FilePath -> IO ()
shouldMatchEncoderSource options x =
  shouldMatchFile . printf outputWrapping $ toElmEncoderSourceWith options x

outputWrapping :: String
outputWrapping = "module Main (..) where\n\n\n%s\n"

shouldMatchFile :: String -> FilePath -> IO ()
shouldMatchFile actual fileExpected =
  do source <- readFile fileExpected
     actual `shouldBe` source

initCap :: String -> String
initCap [] = []
initCap (c:cs) = Data.Char.toUpper c : cs

withPrefix :: String -> String -> String
withPrefix prefix s = prefix ++ initCap s
