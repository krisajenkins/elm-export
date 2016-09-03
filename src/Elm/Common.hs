{-# LANGUAGE OverloadedStrings #-}
module Elm.Common  where

import           Data.Monoid ((<>))
import           Data.Text
import           Elm.Type

isTopLevel :: ElmTypeExpr -> Bool
isTopLevel (Primitive _) = True
isTopLevel (DataType _ _) = True
isTopLevel (Product (Primitive "List") (Primitive "Char")) = True
isTopLevel _ = False

-- Put parentheses around the string if the Elm type requires it (i.e. it's not a
-- Primitive Elm type nor a named DataType).
parenthesize :: ElmTypeExpr -> Text -> Text
parenthesize t s =
  if isTopLevel t then s else ("(" <> s <> ")")

data Options =
  Options {fieldLabelModifier :: Text -> Text}

defaultOptions :: Options
defaultOptions = Options {fieldLabelModifier = id}
