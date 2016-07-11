{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Elm.Decoder (toElmDecoderSource, toElmDecoderSourceWith)
       where

import           Control.Monad.Reader
import           Data.List            (intercalate)
import           Elm.Common
import           Elm.Type
import           Text.Printf

render :: ElmTypeExpr -> Reader Options String

render (TopLevel (DataType d t)) =
  printf "%s : Json.Decode.Decoder %s\n%s =\n%s" fnName d fnName <$> render t
  where fnName = "decode" ++ d

render (DataType d _) = return $ "decode" ++ d

render (Record n t) = printf "  Json.Decode.succeed %s\n%s" n <$> render t

render (Product (Primitive "List") (Primitive "Char")) = render (Primitive "String")

render (Product (Primitive "List") t) = printf "Json.Decode.list %s" <$> render t

render (Product (Primitive "Maybe") t) = printf "Json.Decode.maybe %s" <$> render t

render (Product x y) =
  do bodyX <- render x
     bodyY <- render y
     return $ printf "%s\n%s" bodyX bodyY

render (Selector n t) =
  do fieldModifier <- asks fieldLabelModifier
     printf "    |: (\"%s\" := %s)" (fieldModifier n) <$> render t

render (Tuple2 x y) =
    do bodyX <- render x
       bodyY <- render y
       return $ printf "Json.Decode.tuple2 (,) %s %s" bodyX bodyY

render (Dict x y) =
  printf "Json.Decode.map Dict.fromList (Json.Decode.list (%s))" <$> render (Tuple2 x y)

render (Primitive "String") = return "Json.Decode.string"

render (Primitive "Int") = return "Json.Decode.int"

render (Primitive "Double") = return "Json.Decode.float"

render (Primitive "Float") = return "Json.Decode.float"

render (Primitive "Date") = return "Json.Decode.Extra.date"

render (Primitive "Bool") = return "Json.Decode.bool"

render (Primitive "()") =
  return $ intercalate "\n"
    ["Json.Decode.customDecoder (Json.Decode.list Json.Decode.value)"
    ,"          (\\jsonList ->"
    ,"             case jsonList of"
    ,"               [] -> Result.Ok ()"
    ,"               _  -> Result.Err \"expecting a zero-length array\")"
    ]

render (Field t) = render t

render x = return $ printf "<%s>" (show x)

toElmDecoderSourceWith :: ElmType a => Options -> a -> String
toElmDecoderSourceWith options x =
  runReader (render . TopLevel $ toElmType x) options

toElmDecoderSource :: ElmType a => a -> String
toElmDecoderSource = toElmDecoderSourceWith defaultOptions
