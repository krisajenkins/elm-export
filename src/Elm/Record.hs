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

class HasType a  where
  render :: a -> Reader Options Text

class HasTypeRef a  where
  renderRef :: a -> Reader Options Text

instance HasType ElmDatatype where
  render d@(ElmDatatype _ constructor@(RecordConstructor _ _)) =
    sformat ("type alias " % stext % " =" % cr % stext) <$> renderRef d <*>
    render constructor
  render d@(ElmDatatype _ constructor@(MultipleConstructors _)) =
    sformat ("type " % stext % cr % "    = " % stext) <$> renderRef d <*>
    render constructor
  render d@(ElmDatatype _ constructor@(NamedConstructor _ _)) =
    sformat ("type " % stext % cr % "    = " % stext) <$> renderRef d <*>
    render constructor
  render (ElmPrimitive primitive) = renderRef primitive

instance HasTypeRef ElmDatatype where
  renderRef (ElmDatatype typeName _) = pure typeName
  renderRef (ElmPrimitive primitive) = renderRef primitive

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
  render (ElmPrimitiveRef primitive) = sformat (" " % stext) <$> renderRef primitive
  render (ElmField name value) = do
    fieldModifier <- asks fieldLabelModifier
    sformat (stext % " :" % stext) (fieldModifier name) <$> render value

instance HasTypeRef ElmPrimitive where
  renderRef (EList (ElmPrimitive EChar)) = renderRef EString
  renderRef (EList datatype) = sformat ("List (" % stext % ")") <$> renderRef datatype
  renderRef (ETuple2 x y) =
    sformat ("( " % stext % ", " % stext % " )") <$> renderRef x <*> renderRef y
  renderRef (EMaybe datatype) =
    sformat ("Maybe (" % stext % ")") <$> renderRef datatype
  renderRef (EDict k v) =
    sformat ("Dict (" % stext % ") (" % stext % ")") <$> renderRef k <*>
    renderRef v
  renderRef EInt = pure "Int"
  renderRef EDate = pure "Date"
  renderRef EBool = pure "Bool"
  renderRef EChar = pure "Char"
  renderRef EString = pure "String"
  renderRef EUnit = pure "()"
  renderRef EFloat = pure "Float"

toElmTypeRefWith
  :: ElmType a
  => Options -> a -> Text
toElmTypeRefWith options x = runReader (renderRef (toElmType x)) options

toElmTypeRef
  :: ElmType a
  => a -> Text
toElmTypeRef = toElmTypeRefWith defaultOptions

toElmTypeSourceWith
  :: ElmType a
  => Options -> a -> Text
toElmTypeSourceWith options x = runReader (render (toElmType x)) options

toElmTypeSource
  :: ElmType a
  => a -> Text
toElmTypeSource = toElmTypeSourceWith defaultOptions
