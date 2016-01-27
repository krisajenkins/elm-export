module Elm.File (Spec(..),specsToDir) where

import           Data.List
import           System.Directory
import           Text.Printf

pathString :: [String] -> String
pathString = intercalate "/"

data Spec =
  Spec {namespace    :: [String]
       ,declarations :: [String]}

pathForSpec :: FilePath -> Spec -> [String]
pathForSpec rootDir spec = rootDir : namespace spec

ensureDirectory :: FilePath -> Spec -> IO ()
ensureDirectory rootDir spec =
  let dir = pathString . init $ pathForSpec rootDir spec
  in createDirectoryIfMissing True dir

specToFile :: FilePath -> Spec -> IO ()
specToFile rootDir spec =
  let path = pathForSpec rootDir spec
      file = pathString path ++ ".elm"
      namespaceString =
        intercalate "."
                    (namespace spec)
      body =
        intercalate
          "\n\n"
          (printf "module %s where" namespaceString : declarations spec)
  in do printf "Writing: %s\n" file
        writeFile file body

specsToDir :: FilePath -> [Spec] -> IO ()
specsToDir rootDir = mapM_ processSpec
  where processSpec = ensureDirectory rootDir >> specToFile rootDir
