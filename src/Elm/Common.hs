{-# LANGUAGE OverloadedStrings #-}
module Elm.Common  where

import           Data.Monoid
import           Data.Text              (Text)
import           Data.Text.Lazy         (count)
import           Data.Text.Lazy.Builder
import           Formatting

data Options =
  Options {fieldLabelModifier :: Text -> Text}

defaultOptions :: Options
defaultOptions = Options {fieldLabelModifier = id}

cr :: Format r r
cr = now "\n"

parenthesize :: Format r (Builder -> r)
parenthesize =
  later (\t ->
           if count " " (toLazyText t) > 0
              then singleton '(' <> t <> singleton ')'
              else t)
