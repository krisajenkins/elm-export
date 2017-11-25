{-# LANGUAGE OverloadedStrings #-}

module Elm.File
  ( Spec(..)
  , specsToDir
  , moduleSpec
  , moduleSpecWith
  ) where

import Control.Monad.RWS
import Data.List
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Elm.Common
import Formatting as F
import System.Directory

makePath :: [Text] -> Text
makePath = T.intercalate "/"

data Spec = Spec
  { namespace :: [Text]
  , declarations :: [Text]
  } deriving (Eq, Show)

pathForSpec :: FilePath -> Spec -> [Text]
pathForSpec rootDir spec = T.pack rootDir : namespace spec

ensureDirectory :: FilePath -> Spec -> IO ()
ensureDirectory rootDir spec =
  let dir = makePath . Data.List.init $ pathForSpec rootDir spec
  in createDirectoryIfMissing True (T.unpack dir)

specToFile :: FilePath -> Spec -> IO ()
specToFile rootDir spec =
  let path = pathForSpec rootDir spec
      file = makePath path <> ".elm"
      namespaceText = T.intercalate "." (namespace spec)
      body =
        T.intercalate
          "\n\n"
          (sformat ("module " % F.stext % " exposing (..)") namespaceText :
           declarations spec)
  in do fprint ("Writing: " % F.stext % "\n") file
        T.writeFile (T.unpack file) body

specsToDir :: [Spec] -> FilePath -> IO ()
specsToDir specs rootDir = mapM_ processSpec specs
  where
    processSpec = ensureDirectory rootDir >> specToFile rootDir

moduleSpecWith :: Options -> [Text] -> RenderM () -> Spec
moduleSpecWith options ns m =
  let ((), (imports, defns)) = execRWS m options ()
  in Spec
     { namespace = ns
     , declarations =
         (T.intercalate "\n" . fmap ("import " <>) . S.toAscList $ imports) :
         defns
     }

moduleSpec :: [Text] -> RenderM () -> Spec
moduleSpec = moduleSpecWith defaultOptions
