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
  defaultMainWithOpts [testCase "toElmTypeSource" testToElmTypeSource]
                      mempty

testToElmTypeSource :: Assertion
testToElmTypeSource =
  do source <- readFile "test/Post.txt"
     assertEqual "Encoding a Post" source $
       (toElmTypeSource (Proxy :: Proxy Post)) ++ "\n"
