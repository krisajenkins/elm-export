{-# LANGUAGE OverloadedStrings #-}
module Elm.Record (toElmTypeSource,toElmTypeSourceWith) where

import           Control.Monad.Reader
import           Elm.Common
import           Elm.Type
import           Formatting
import Data.Text

class HasType a where
  render :: a -> Reader Options Text

instance HasType ElmDatatype where
    render (ElmDatatype typeName (RecordConstructor _ value)) =
        sformat ("type alias " % stext % " =\n    { " % stext % "\n    }") typeName <$>
        render value
    render (ElmDatatype typeName (NamedConstructor constructorName value)) =
        sformat ("type " % stext % "\n    = " % stext % " " % stext) typeName constructorName <$>
        render value
    render (ElmPrimitive primitive) = render primitive

instance HasType ElmValue where
    render (ElmRef name) = pure name
    render (Values x y) =
        sformat (stext % cr % "    , " % stext) <$> render x <*> render y
    render (ElmPrimitiveRef primitive) = render primitive
    render (ElmField name value) = do
        fieldModifier <- asks fieldLabelModifier
        sformat (stext % " : " % stext) (fieldModifier name) <$> render value


instance HasType ElmPrimitive where
    render (EList (ElmPrimitive EChar)) = render EString
    render (EList (ElmPrimitive value)) = sformat ("List " % stext) <$> render value
    render (EList (ElmDatatype name _)) = pure $ sformat ("List " % stext) name
    render (ETuple2 x y) =
        sformat ("( " % stext % ", " % stext % " )") <$> render x <*> render y
    render (EMaybe (ElmDatatype name _)) = pure $ sformat ("Maybe " % stext) name
    render (EMaybe (ElmPrimitive value)) =
        sformat ("Maybe " % stext) <$> render value
    render (EDict k v) =
        sformat ("Dict " % stext % " " % stext) <$> render k <*> render v
    render EInt = pure "Int"
    render EDate = pure "Date"
    render EBool = pure "Bool"
    render EChar = pure "Char"
    render EString = pure "String"
    render EUnit = pure "()"
    render EFloat = pure "Float"

toElmTypeSourceWith :: ElmType a => Options -> a -> Text
toElmTypeSourceWith options x = runReader (render (toElmType x)) options

toElmTypeSource :: ElmType a => a -> Text
toElmTypeSource = toElmTypeSourceWith defaultOptions
