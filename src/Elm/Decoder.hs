{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Elm.Decoder (toElmDecoderSource) where

import           Elm.Type
import           Text.Printf

render :: ElmType -> String
render (TopLevel (DataType d t)) =
  printf "%s : Json.Decode.Decoder %s\n%s =\n%s" fnName d fnName (render t)
  where fnName = "decode" ++ d
render (DataType d _) = "decode" ++ d
render (Record n t) = printf "  Json.Decode.succeed %s\n%s" n (render t)
render (Product (Primitive "List") (Primitive "Char")) = render (Primitive "String")
render (Product (Primitive "List") t) = printf "Json.Decode.list %s" (render t)
render (Product (Primitive "Maybe") t) = printf "Json.Decode.maybe %s" (render t)
render (Product x y) = printf "%s\n%s" (render x) (render y)
render (Selector n t) = printf "    |: (\"%s\" := %s)" n (render t)
render (Primitive "String") = "Json.Decode.string"
render (Primitive "Int") = "Json.Decode.int"
render (Primitive "Double") = "Json.Decode.float"
render (Primitive "Float") = "Json.Decode.float"
render (Primitive "Date") = "Json.Decode.date"
render (Primitive "Bool") = "Json.Decode.bool"
render (Field t) = render t
render x = printf "<%s>" (show x)

toElmDecoderSource :: ToElmType a => a -> String
toElmDecoderSource = render . TopLevel . toElmType
