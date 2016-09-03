{-# LANGUAGE OverloadedStrings #-}
module Elm.Record (toElmTypeSource,toElmTypeSourceWith) where

import           Control.Monad.Reader
import           Elm.Common
import           Elm.Type
import           Formatting
import Data.Text

render :: ElmTypeExpr -> Reader Options Text

render (TopLevel (DataType dataTypeName record@(Record _ _))) =
    sformat ("type alias " % stext % " =\n    { " % stext % "\n    }") dataTypeName <$>
    render record

render (TopLevel (DataType d s@(Sum _ _))) =
    sformat ("type " % stext % "\n    = " % stext) d <$> render s


render (DataType d _) = return d
render (Primitive s) = return s

render (Sum x y) =
    sformat (stext % "\n    | " % stext) <$> render x <*> render y


render (Field t) = render t

render (Selector s t) = do
    fieldModifier <- asks fieldLabelModifier
    sformat (stext % " : " % stext) (fieldModifier s) <$> render t

render (Constructor c Unit) = pure c

render (Constructor c t) = sformat (stext % " " % stext) c <$> render t

render (Tuple2 x y) =
    sformat ("( " % stext % ", " % stext % " )") <$> render x <*> render y

render (Dict x y) =
    sformat ("Dict " % stext % " " % stext) <$> render x <*> render y


render (Product (Primitive "List") (Primitive "Char")) = return "String"

render (Product (Primitive "List") p@(Product _ _)) =
  sformat ("List (" % stext % ")") <$> render p

render (Product (Primitive "List") t) = sformat ("List " % stext) <$> render t

render (Product (Primitive "Dict") (Product k v)) =
    sformat ("Dict " % stext % " " % stext) <$> render k <*> render v


render (Product x y) =
  do bodyX <- render x
     bodyY <- render y
     return $ sformat (stext % " " % stext) bodyX (parenthesize y  bodyY)

render (Record n (Product x y)) =
  do bodyX <- render (Record n x)
     bodyY <- render (Record n y)
     return $ sformat (stext % "\n    , " % stext) bodyX bodyY

render (Record _ s@(Selector _ _)) = render s

render Unit = return ""

toElmTypeSourceWith :: ElmType a => Options -> a -> Text
toElmTypeSourceWith options x = runReader (render . TopLevel $ toElmType x) options

toElmTypeSource :: ElmType a => a -> Text
toElmTypeSource = toElmTypeSourceWith defaultOptions
