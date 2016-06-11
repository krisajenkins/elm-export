module Elm.Common  where

import           Elm.Type

isTopLevel :: ElmTypeExpr -> Bool
isTopLevel (Primitive _) = True
isTopLevel (DataType _ _) = True
isTopLevel (Product (Primitive "List") (Primitive "Char")) = True
isTopLevel _ = False

-- Put parentheses around the string if the Elm type requires it (i.e. it's not a
-- Primitive Elm type nor a named DataType).
parenthesize :: ElmTypeExpr -> String -> String
parenthesize t s =
  if isTopLevel t then s else "(" ++ s ++ ")"

data ElmVersion
  = V_0_16
  | V_0_17
  deriving (Show, Eq, Ord)

data Options =
  Options {elmVersion :: ElmVersion
          ,fieldLabelModifier :: String -> String}

defaultOptions :: Options
defaultOptions = Options {elmVersion = V_0_16
                         ,fieldLabelModifier = id}
