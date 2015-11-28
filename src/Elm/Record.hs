module Elm.Record (toElmTypeSource) where

import           Elm.Common
import           Elm.Type
import           Text.Printf

render :: ElmType -> String
render (TopLevel (DataType d r@(Record _ _))) = printf "type alias %s =\n  {%s}" d (render r)
render (TopLevel (DataType d s@(Sum _ _))) = printf "type %s\n  = %s" d (render s)
render (DataType d _) = d
render (Primitive s) = s
render (Sum x y) = printf "%s\n  | %s" (render x) (render y)
render (Field t) = render t
render (Selector s t) = printf "%s : %s" s (render t)
render (Constructor c t) = printf "%s %s" c (render t)
render (Product (Primitive "List") (Primitive "Char")) = "String"
render (Product x y) = printf "%s %s" (render x) (parenthesize y (render y))
render (Record n (Product x y)) = printf "%s\n  ,%s" (render (Record n x)) (render (Record n y))
render (Record _ s@(Selector _ _)) = render s
render Unit = ""

toElmTypeSource :: ToElmType a => a -> String
toElmTypeSource = render . TopLevel . toElmType
