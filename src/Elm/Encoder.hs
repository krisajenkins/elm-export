module Elm.Encoder (toElmEncoderSource) where

import           Data.List (intercalate)
import           Elm.Type
import           Text.Printf

render :: ElmType -> String
render elmType = case elmType of
  (TopLevel (DataType d t)) ->
    let fnName = "encode" ++ d
    in printf "%s : %s -> JS.Value\n%s x =%s" fnName d fnName (render t)
  (DataType d _) -> "encode" ++ d
  (Record _ t) -> printf "\n  JS.object\n    [%s]" (render t)
  (Product x y) -> printf "%s\n    ,%s" (render x) (render y)
  (Selector n t) -> printf "(\"%s\", %s x.%s)" n (render t) n
  (Primitive "String") -> "JS.string"
  (Primitive "Int") -> "JS.int"
  (Primitive "Double") -> "JS.float"
  (Primitive "Float") -> "JS.float"
  (Primitive "Date") -> "JS.date"
  (Primitive "Bool") -> "JS.bool"
  (Field (Product (Primitive "Maybe") (Product (Primitive "List") (Primitive "Char")))) -> renderMaybeWith "JS.string"
  (Field (Product (Primitive "Maybe") t)) -> renderMaybeWith (render t)
  (Field (Product (Primitive "List") (Primitive "Char"))) -> "JS.string"
  (Field (Product (Primitive "List") t )) -> printf "(JS.list << List.map %s)" (render t)
  (Field t) -> render t
  x -> printf "<%s>" (show x)

  where renderMaybeWith :: String -> String
        renderMaybeWith renderedType =
          printf (intercalate "\n"
                    [""
                    ,"      (\\y ->"
                    ,"        case y of"
                    ,"          Just val -> %s val"
                    ,"          Nothing -> JS.null)"])
                 renderedType

toElmEncoderSource :: ToElmType a => a -> String
toElmEncoderSource = render . TopLevel . toElmType
