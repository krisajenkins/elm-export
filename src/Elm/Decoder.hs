{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Elm.Decoder (toElmDecoderSource) where

import           Elm.Type
import           Text.Printf

render :: ElmType -> String
render (TopLevel (DataType d t)) =
  printf "%s : Decoder %s\n%s =\n%s" fnName d fnName (render t)
  where fnName = "decode" ++ d
render (DataType d _) = "decode" ++ d
render (Record n t) = printf "  succeed %s\n%s" n (render t)
render (Product (Primitive "List") (Primitive "Char")) = "string"
render (Product (Primitive "List") t) = printf "list %s" (render t)
render (Product (Primitive "Maybe") t) = printf "maybe %s" (render t)
render (Product x y) = printf "%s\n%s" (render x) (render y)
render (Selector n t) = printf "    |: (\"%s\" := %s)" n (render t)
render (Primitive "String") = "string"
render (Primitive "Int") = "int"
render (Primitive "Double") = "float"
render (Primitive "Float") = "float"
render (Primitive "Date") = "date"
render (Primitive "Bool") = "bool"
render (Field t) = render t
render x = printf "<%s>" (show x)

toElmDecoderSource :: ToElmType a => a -> String
toElmDecoderSource = render . TopLevel . toElmType
