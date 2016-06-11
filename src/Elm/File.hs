module Elm.File (Spec(..),specsToDir,specsToDirWith) where

import           Data.List
import           System.Directory
import           Text.Printf
import           Elm.Common

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

specToFileWith :: Options -> FilePath -> Spec -> IO ()
specToFileWith options rootDir spec =
  let path = pathForSpec rootDir spec
      file = pathString path ++ ".elm"
      namespaceString =
        intercalate "."
                    (namespace spec)
      moduleHeader =
        case elmVersion options of
          V_0_16 ->
            printf "module %s where" namespaceString
          V_0_17 ->
            printf "module %s exposing (..)" namespaceString
      body =
        intercalate
          "\n\n"
          (moduleHeader : declarations spec)
  in do printf "Writing: %s\n" file
        writeFile file body

specsToDirWith :: Options -> [Spec] -> FilePath -> IO ()
specsToDirWith options specs rootDir = mapM_ processSpec specs
  where processSpec = ensureDirectory rootDir >> specToFileWith options rootDir

specsToDir :: [Spec] -> FilePath -> IO ()
specsToDir = specsToDirWith defaultOptions
