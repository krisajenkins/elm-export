{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module TypesSpec where

import           Elm
import           GHC.Generics
import           Test.Hspec   as Hspec

-- All the types in this file should be Elm-encodable.
data Person = Person
    { personName :: String
    } deriving (Generic, ElmType)

spec :: Hspec.Spec
spec = return ()
