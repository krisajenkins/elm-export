module Elm.File (Spec(..),specsToDir) where

import           Data.List
import           System.Directory
import           Text.Printf

namespaceString :: [String] -> String
namespaceString = intercalate "."

pathString :: [String] -> String
pathString = intercalate "/"

data Spec =
  Spec {namespace    :: [String]
       ,declarations :: [String]}

specToFile :: FilePath -> Spec -> IO ()
specToFile rootDir spec =
  let path = rootDir : namespace spec
      dir = pathString $ init path
      file = pathString path ++ ".elm"
      body =
        intercalate
          "\n\n"
          (printf "module %s where" (namespaceString (namespace spec)) :
           declarations spec)
  in do printf "Writing: %s\n" file
        createDirectoryIfMissing True dir
        writeFile file body

specsToDir :: FilePath -> [Spec] -> IO ()
specsToDir rootDir = mapM_ (specToFile rootDir)
