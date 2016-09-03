{-# LANGUAGE OverloadedStrings #-}
module Elm.Encoder (toElmEncoderSource, toElmEncoderSourceWith)
       where

import           Control.Monad.Reader
import           Data.Text
import           Elm.Common
import           Elm.Type
import           Formatting

render :: ElmTypeExpr -> Reader Options Text

render (TopLevel (DataType d t)) =
    sformat
        (stext % " : " % stext % " -> Value\n" % stext % " x =" % stext)
        fnName
        d
        fnName <$>
    render t
  where
    fnName = sformat ("encode" % stext) d

render (DataType d _) = return $ sformat ("encode" % stext) d

render (Record _ t) =
  sformat ("\n    object\n        [ " % stext % "\n        ]") <$> render t

render (Product (Primitive "List") (Primitive "Char")) =
  render (Primitive "String")

render (Product (Primitive "List") t) =
  sformat ("(list << List.map " % stext % ")") <$> render t

render (Product (Primitive "Maybe") t) =
  sformat ("(Maybe.withDefault null << Maybe.map " % stext % ")") <$> render t

render (Tuple2 x y) =
  do bodyX <- render x
     bodyY <- render y
     return $ sformat ("tuple2 " % stext % " " % stext) bodyX bodyY

render (Dict x y) =
  do bodyX <- render x
     bodyY <- render y
     return $ sformat ("dict " % stext % " " % stext) bodyX bodyY

render (Product x y) =
  do bodyX <- render x
     bodyY <- render y
     return $ sformat (stext % "\n        , " % stext) bodyX bodyY

render (Selector n t) =
  do fieldModifier <- asks fieldLabelModifier
     typeBody <- render t
     return $ sformat ("( \"" % stext % "\", " % stext % " x." % stext % " )") (fieldModifier n) typeBody n

render  (Primitive "String") = return "string"
render  (Primitive "Int") = return "int"
render  (Primitive "Double") = return "float"
render  (Primitive "Float") = return "float"
render  (Primitive "Date") = return "(string << toISOString)"
render  (Primitive "Bool") = return "bool"
render  (Field t) = render t

toElmEncoderSourceWith :: ElmType a => Options -> a -> Text
toElmEncoderSourceWith options x = runReader (render . TopLevel $ toElmType x) options

toElmEncoderSource :: ElmType a => a -> Text
toElmEncoderSource = toElmEncoderSourceWith defaultOptions
