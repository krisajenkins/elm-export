module Elm.Encoder (toElmEncoderSource) where

import           Elm.Type
import           Text.Printf

render :: ElmTypeExpr -> String
render elmType =
  case elmType of
    (TopLevel (DataType d t)) ->
      let fnName = "encode" ++ d
      in printf "%s : %s -> Json.Encode.Value\n%s x =%s" fnName d fnName (render t)
    (DataType d _) -> "encode" ++ d
    (Record _ t) -> printf "\n  Json.Encode.object\n    [ %s\n    ]" (render t)
    (Product (Primitive "List") (Primitive "Char")) -> render (Primitive "String")
    (Product (Primitive "List") t) ->
      printf "(Json.Encode.list << List.map %s)" (render t)
    (Product (Primitive "Maybe") t) ->
      printf "(Maybe.withDefault Json.Encode.null << Maybe.map %s)" (render t)
    (Product x y) ->
      printf "%s\n    , %s"
             (render x)
             (render y)
    (Selector n t) -> printf "( \"%s\", %s x.%s )" n (render t) n
    (Primitive "String") -> "Json.Encode.string"
    (Primitive "Int") -> "Json.Encode.int"
    (Primitive "Double") -> "Json.Encode.float"
    (Primitive "Float") -> "Json.Encode.float"
    (Primitive "Date") -> "Json.Encode.date"
    (Primitive "Bool") -> "Json.Encode.bool"
    (Field t) -> render t
    x -> printf "<%s>" (show x)

toElmEncoderSource :: ElmType a => a -> String
toElmEncoderSource = render . TopLevel . toElmType
