{-# LANGUAGE OverloadedStrings #-}
module Elm.Encoder
  ( toElmEncoderRef
  , toElmEncoderRefWith
  , toElmEncoderSource
  , toElmEncoderSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Text
import           Elm.Common
import           Elm.Type
import           Formatting

class HasEncoder a where
  render :: a -> Reader Options Text

class HasEncoderRef a where
  renderRef :: a -> Reader Options Text

instance HasEncoder ElmDatatype where
    render d@(ElmDatatype name constructor) = do
        fnName <- renderRef d
        sformat
            (stext % " : " % stext % " -> Value" % cr % stext % " x =" % stext)
            fnName
            name
            fnName <$>
            render constructor
    render (ElmPrimitive primitive) = renderRef primitive

instance HasEncoderRef ElmDatatype where
    renderRef (ElmDatatype name _) =
        pure $ sformat ("encode" % stext) name

    renderRef (ElmPrimitive primitive) =
        renderRef primitive

instance HasEncoder ElmConstructor where
    render (RecordConstructor _ value) =
      sformat (cr % "    object" % cr % "        [ " % stext % cr % "        ]") <$> render value

instance HasEncoder ElmValue where
    render (ElmField name value) = do
        fieldModifier <- asks fieldLabelModifier
        valueBody <- render value
        pure $
            sformat
                ("( \"" % stext % "\", " % stext % " x." % stext % " )")
                (fieldModifier name)
                valueBody
                name
    render (ElmPrimitiveRef primitive) = renderRef primitive
    render (ElmRef name) = pure $ sformat ("encode" % stext) name
    render (Values x y) = sformat (stext % cr % "        , " % stext) <$> render x <*> render y

instance HasEncoderRef ElmPrimitive where
    renderRef EDate = pure "(string << toISOString)"
    renderRef EUnit = pure "null"
    renderRef EInt = pure "int"
    renderRef EChar = pure "char"
    renderRef EBool = pure "bool"
    renderRef EFloat = pure "float"
    renderRef EString = pure "string"
    renderRef (EList (ElmPrimitive EChar)) = pure "string"
    renderRef (EList datatype) = sformat ("(list << List.map " % stext % ")") <$> renderRef datatype
    renderRef (EMaybe datatype) =
        sformat ("(Maybe.withDefault null << Maybe.map " % stext % ")") <$>
        renderRef datatype
    renderRef (ETuple2 x y) =
        sformat ("(tuple2 " % stext % " " % stext % ")") <$> renderRef x <*>
        renderRef y
    renderRef (EDict k datatype) =
        sformat ("(dict " % stext % " " % stext % ")") <$> renderRef k <*> renderRef datatype

toElmEncoderRefWith :: ElmType a => Options -> a -> Text
toElmEncoderRefWith options x = runReader (renderRef (toElmType x)) options

toElmEncoderRef :: ElmType a => a -> Text
toElmEncoderRef = toElmEncoderRefWith defaultOptions

toElmEncoderSourceWith :: ElmType a => Options -> a -> Text
toElmEncoderSourceWith options x = runReader (render (toElmType x)) options

toElmEncoderSource :: ElmType a => a -> Text
toElmEncoderSource = toElmEncoderSourceWith defaultOptions
