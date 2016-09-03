{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Elm.Decoder
  ( toElmDecoderSource
  , toElmDecoderSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Text
import           Elm.Common
import           Elm.Type
import           Formatting

cr :: Format r r
cr = now "\n"

render :: ElmTypeExpr -> Reader Options Text

render (TopLevel (DataType d t)) =
    sformat
        (stext % " : Decoder " % stext % cr % stext % " =" % cr % stext)
        fnName
        d
        fnName <$>
    render t
  where
    fnName = sformat ("decode" % stext) d

render (DataType d _) = pure $ sformat ("decode" % stext) d

render (Record n t) =
    sformat ("    decode " % stext % cr % stext) n <$> render t

render (Product (Primitive "List") (Primitive "Char")) =
    render (Primitive "String")

render (Product (Primitive "List") t) =
    sformat ("(list " % stext % ")") <$> render t

render (Product (Primitive "Maybe") t) =
    sformat ("(maybe " % stext % ")") <$> render t

render (Product x y) =
    sformat (stext % cr % stext) <$> render x <*> render y

render (Selector n t) = do
    fieldModifier <- asks fieldLabelModifier
    sformat
        ("        |> required \"" % stext % "\" " % stext)
        (fieldModifier n) <$>
        render t

render (Tuple2 x y) =
    sformat ("(tuple2 (,) " % stext % " " % stext % ")") <$> render x <*>
    render y

render (Dict x y) =
    sformat ("(map Dict.fromList (list " % stext % "))") <$>
    render (Tuple2 x y)

render (Primitive "String") = pure "string"
render (Primitive "Int") = pure "int"
render (Primitive "Double") = pure "float"
render (Primitive "Float") = pure "float"
render (Primitive "Date") = pure "(customDecoder string Date.fromString)"
render (Primitive "Bool") = pure "bool"
render (Field t) = render t
render x = pure $ sformat ("<" % shown % ">") x


toElmDecoderSourceWith
    :: ElmType a
    => Options -> a -> Text
toElmDecoderSourceWith options x = runReader (render . TopLevel $ toElmType x) options

toElmDecoderSource
    :: ElmType a
    => a -> Text
toElmDecoderSource = toElmDecoderSourceWith defaultOptions
