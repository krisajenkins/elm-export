{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ExportSpec where

import           Control.Monad (zipWithM_)
import           Data.Char
import           Data.Map
import           Data.Proxy
import           Data.Text
import           Data.Time
import           Elm
import           GHC.Generics
import           Test.Hspec    hiding (Spec)
import           Test.Hspec    as Hspec
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

     toElmTypeSourceDefsSpec
     toElmDecoderSourceDefsSpec
     toElmEncoderSourceDefsSpec

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

toElmTypeSourceDefsSpec :: Hspec.Spec
toElmTypeSourceDefsSpec =
  describe "Convert to Elm types with supporting definitions." $
  do it "toElmTypeSourceDefs [Post]" $
       shouldMatchTypeSourceDefs
         defaultOptions
         (Proxy :: Proxy [Post])
         "List (Post)"
         ["test/PostType.elm"
         ,"test/CommentType.elm"]

     it "toElmTypeSourceDefsWithOptions [Post]" $
       shouldMatchTypeSourceDefs
         (defaultOptions {fieldLabelModifier = withPrefix "post"})
         (Proxy :: Proxy [Post])
         "List (Post)"
         ["test/PostTypeWithOptions.elm"
          -- FIXME: the same prefix is applied to all generated types.
         ,"test/CommentTypeWithOptionsPost.elm"]

toElmDecoderSourceDefsSpec :: Hspec.Spec
toElmDecoderSourceDefsSpec =
  describe "Convert to Elm decoders with supporting definitions." $
  do it "toElmDecoderSourceDefs [Post]" $
       shouldMatchDecoderSourceDefs
         defaultOptions
         (Proxy :: Proxy [Post])
         "(Json.Decode.list decodePost)"
         ["test/PostDecoder.elm"
         ,"test/CommentDecoder.elm"]

     it "toElmDecoderSourceDefsWithOptions [Post]" $
       shouldMatchDecoderSourceDefs
         (defaultOptions {fieldLabelModifier = withPrefix "post"})
         (Proxy :: Proxy [Post])
         "(Json.Decode.list decodePost)"
         ["test/PostDecoderWithOptions.elm"
          -- FIXME: the same prefix is applied to all generated types.
         ,"test/CommentDecoderWithOptionsPost.elm"]

toElmEncoderSourceDefsSpec :: Hspec.Spec
toElmEncoderSourceDefsSpec =
  describe "Convert to Elm encoders with supporting definitions." $
  do it "toElmEncoderSource [Post]" $
       shouldMatchEncoderSourceDefs
         defaultOptions
         (Proxy :: Proxy [Post])
         "(JS.list << List.map encodePost)"
         ["test/PostEncoder.elm"
         ,"test/CommentEncoder.elm"]

     it "toElmEncoderSourceWithOptions [Post]" $
       shouldMatchEncoderSourceDefs
         (defaultOptions {fieldLabelModifier = withPrefix "post"})
         (Proxy :: Proxy [Post])
         "(JS.list << List.map encodePost)"
         ["test/PostEncoderWithOptions.elm"
          -- FIXME: the same prefix is applied to all generated types.
         ,"test/CommentEncoderWithOptionsPost.elm"]

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

shouldMatchTypeSourceDefs
  :: ElmType a
  => Options -> a -> String -> [FilePath] -> IO ()
shouldMatchTypeSourceDefs =
  shouldMatchSourceDefs toElmTypeSourceDefsWith

shouldMatchDecoderSourceDefs
  :: ElmType a
  => Options -> a -> String -> [FilePath] -> IO ()
shouldMatchDecoderSourceDefs =
  shouldMatchSourceDefs toElmDecoderSourceDefsWith

shouldMatchEncoderSourceDefs
  :: ElmType a
  => Options -> a -> String -> [FilePath] -> IO ()
shouldMatchEncoderSourceDefs =
  shouldMatchSourceDefs toElmEncoderSourceDefsWith

shouldMatchSourceDefs
  :: ElmType a
  => (Options -> a -> (String, [String]))
  -> Options -> a -> String -> [FilePath] -> IO ()
shouldMatchSourceDefs toSourceDefsWith options x expected expectedDefFiles = do
    let (actual, actualDefs) =
          toSourceDefsWith options x
    actual `shouldBe` expected
    zipWithM_ shouldMatchFile
      (printf outputWrapping <$> actualDefs)
      expectedDefFiles

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
