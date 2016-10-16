{-# LANGUAGE OverloadedStrings #-}
module Elm.Record
  ( toElmTypeRef
  , toElmTypeRefWith
  , toElmTypeSource
  , toElmTypeSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Text
import           Elm.Common
import           Elm.Type
import           Formatting

class HasType a where
  render :: a -> Reader Options Text

class HasTypeRef a where
  renderRef :: a -> Reader Options Text

instance HasType ElmDatatype where
    render d@(ElmDatatype _ constructor@(RecordConstructor _ _)) =
        sformat ("type alias " % stext % " =" % cr % stext) <$> renderRef d <*> render constructor
    render d@(ElmDatatype _ constructor@(MultipleConstructors _)) =
        sformat ("type " % stext % cr % "    = " % stext) <$> renderRef d <*> render constructor
    render d@(ElmDatatype _ constructor@(NamedConstructor _ _)) =
        sformat ("type " % stext % cr % "    = " % stext) <$> renderRef d <*> render constructor
    render (ElmPrimitive primitive) = render primitive

instance HasTypeRef ElmDatatype where
    renderRef (ElmDatatype typeName _) =
        pure typeName

    renderRef (ElmPrimitive primitive) =
        renderRef primitive


instance HasType ElmConstructor where
    render (RecordConstructor _ value) =
        sformat ("    { " % stext % cr % "    }") <$> render value
    render (NamedConstructor constructorName value) =
        sformat (stext % stext) constructorName <$> render value
    render (MultipleConstructors constructors) =
        fmap (Data.Text.intercalate "\n    | ") . sequence $ render <$> constructors


instance HasType ElmValue where
    render (ElmRef name) = pure name
    render ElmEmpty = pure ""
    render (Values x y) =
        sformat (stext % cr % "    , " % stext) <$> render x <*> render y
    render (ElmPrimitiveRef primitive) = sformat (" " % stext) <$> render primitive
    render (ElmField name value) = do
        fieldModifier <- asks fieldLabelModifier
        sformat (stext % " :" % stext) (fieldModifier name) <$> render value


instance HasType ElmPrimitive where
    render (EList (ElmPrimitive EChar)) = render EString
    render (EList datatype) = sformat ("List " % (parenthesize %. stext)) <$> renderRef datatype
    render (ETuple2 x y) =
        sformat ("( " % stext % ", " % stext % " )") <$> render x <*> render y
    render (EMaybe datatype) =
        sformat ("Maybe " % (parenthesize %. stext)) <$> renderRef datatype
    render (EDict k v) =
        sformat ("Dict " % (parenthesize %. stext) % " " % (parenthesize %. stext)) <$> render k <*> render v
    render EInt = pure "Int"
    render EDate = pure "Date"
    render EBool = pure "Bool"
    render EChar = pure "Char"
    render EString = pure "String"
    render EUnit = pure "()"
    render EFloat = pure "Float"

instance HasTypeRef ElmPrimitive where
    renderRef = render

toElmTypeRefWith :: ElmType a => Options -> a -> Text
toElmTypeRefWith options x = runReader (renderRef (toElmType x)) options

toElmTypeRef :: ElmType a => a -> Text
toElmTypeRef = toElmTypeRefWith defaultOptions

toElmTypeSourceWith :: ElmType a => Options -> a -> Text
toElmTypeSourceWith options x = runReader (render (toElmType x)) options

toElmTypeSource :: ElmType a => a -> Text
toElmTypeSource = toElmTypeSourceWith defaultOptions
