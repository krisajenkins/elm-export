{-# LANGUAGE DeriveGeneric #-}

import           Data.Monoid
import           Data.Proxy
import           Elm
import           GHC.Generics
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

data Post =
  Post {id   :: Int
       ,name :: String
       ,age  :: Maybe Double}
  deriving Generic

instance ToElmType Post

main :: IO ()
main =
  defaultMainWithOpts
    [testCase "toElmTypeSource" testToElmTypeSource
    ,testCase "toElmDecoderSource" testToElmDecoderSource]
    mempty

testToElmTypeSource :: Assertion
testToElmTypeSource =
  do source <- readFile "test/PostType.txt"
     assertEqual "Encoding a Post type" source $
       (toElmTypeSource (Proxy :: Proxy Post)) ++ "\n"

testToElmDecoderSource :: Assertion
testToElmDecoderSource =
  do source <- readFile "test/PostDecoder.txt"
     assertEqual "Encoding a Post decoder" source $
       (toElmDecoderSource (Proxy :: Proxy Post)) ++ "\n"
