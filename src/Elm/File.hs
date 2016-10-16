{-# LANGUAGE OverloadedStrings #-}

module Elm.File
  ( Spec(..)
  , specsToDir
  ) where

import           Data.List
import           Data.Monoid
import           Formatting       as F
import           System.Directory

pathString :: [String] -> String
pathString = intercalate "/"

data Spec = Spec
    { namespace    :: [String]
    , declarations :: [String]
    }

pathForSpec :: FilePath -> Spec -> [String]
pathForSpec rootDir spec = rootDir : namespace spec

ensureDirectory :: FilePath -> Spec -> IO ()
ensureDirectory rootDir spec =
    let dir = pathString . init $ pathForSpec rootDir spec
    in createDirectoryIfMissing True dir

specToFile :: FilePath -> Spec -> IO ()
specToFile rootDir spec =
    let path = pathForSpec rootDir spec
        file = pathString path <> ".elm"
        namespaceString = intercalate "." (namespace spec)
        body =
            intercalate
                "\n\n"
                (formatToString ("module " % F.string % " exposing (..)") namespaceString :
                 declarations spec)
    in do fprint ("Writing: " % F.string % "\n") file
          writeFile file body

specsToDir :: [Spec] -> FilePath -> IO ()
specsToDir specs rootDir = mapM_ processSpec specs
  where
    processSpec = ensureDirectory rootDir >> specToFile rootDir
