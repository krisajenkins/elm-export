{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Elm.Decoder (toElmDecoderSource, toElmDecoderSourceWith)
       where

import           Control.Monad.Reader
import           Elm.Common
import           Elm.Type
import           Text.Printf

render :: ElmTypeExpr -> Reader Options String

render (TopLevel (DataType d t)) =
  printf "%s : Decoder %s\n%s =\n%s" fnName d fnName <$> render t
  where fnName = "decode" ++ d

render (DataType d _) = return $ "decode" ++ d

render (Record n t) = printf "    decode %s\n%s" n <$> render t

render (Product (Primitive "List") (Primitive "Char")) = render (Primitive "String")

render (Product (Primitive "List") t) = printf "(list %s)" <$> render t

render (Product (Primitive "Maybe") t) = printf "(maybe %s)" <$> render t

render (Product x y) =
  do bodyX <- render x
     bodyY <- render y
     return $ printf "%s\n%s" bodyX bodyY

render (Selector n t) =
  do fieldModifier <- asks fieldLabelModifier
     printf "        |> required \"%s\" %s" (fieldModifier n) <$> render t

render (Tuple2 x y) =
    do bodyX <- render x
       bodyY <- render y
       return $ printf "(tuple2 (,) %s %s)" bodyX bodyY

render (Dict x y) =
  printf "(map Dict.fromList (list %s))" <$> render (Tuple2 x y)

render (Primitive "String") = return "string"

render (Primitive "Int") = return "int"

render (Primitive "Double") = return "float"

render (Primitive "Float") = return "float"

render (Primitive "Date") = return "(customDecoder string Date.fromString)"

render (Primitive "Bool") = return "bool"

render (Field t) = render t

render x = return $ printf "<%s>" (show x)

toElmDecoderSourceWith :: ElmType a => Options -> a -> String
toElmDecoderSourceWith options x =
  runReader (render . TopLevel $ toElmType x) options

toElmDecoderSource :: ElmType a => a -> String
toElmDecoderSource = toElmDecoderSourceWith defaultOptions
