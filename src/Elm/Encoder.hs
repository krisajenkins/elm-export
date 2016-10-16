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
    render (ElmDatatype name constructor) =
        sformat
            (stext % " : " % stext % " -> Value" % cr % stext % " x =" % stext)
            fnName
            name
            fnName <$>
        render constructor
      where
        fnName = sformat ("encode" % stext) name
    render (ElmPrimitive primitive) = render primitive

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
    render (ElmPrimitiveRef primitive) = render primitive
    render (ElmRef name) = pure $ sformat ("encode" % stext) name
    render (Values x y) = sformat (stext % cr % "        , " % stext) <$> render x <*> render y

instance HasEncoder ElmPrimitive where
    render EDate = pure "(string << toISOString)"
    render EUnit = pure "null"
    render EInt = pure "int"
    render EChar = pure "char"
    render EBool = pure "bool"
    render EFloat = pure "float"
    render EString = pure "string"
    render (EList (ElmDatatype name _)) = sformat ("(list << List.map " % stext % ")") <$> render (ElmRef name)
    render (EList (ElmPrimitive EChar)) = pure "string"
    render (EList (ElmPrimitive primitive)) =
        sformat ("(list << List.map " % stext % ")") <$> render primitive
    render (EMaybe (ElmDatatype name _)) =
        sformat ("(Maybe.withDefault null << Maybe.map " % stext % ")") <$>
        render (ElmRef name)
    render (EMaybe (ElmPrimitive primitive)) =
        sformat ("(Maybe.withDefault null << Maybe.map " % stext % ")") <$>
        render primitive
    render (ETuple2 x y) =
        sformat ("(tuple2 " % stext % " " % stext % ")") <$> render x <*>
        render y
    render (EDict k (ElmDatatype name _)) =
        sformat ("(dict " % stext % " " % stext % ")") <$> render k <*> render (ElmRef name)
    render (EDict k (ElmPrimitive primitive)) =
        sformat ("(dict " % stext % " " % stext % ")") <$> render k <*> render primitive

instance HasEncoderRef ElmPrimitive where
    renderRef = render

toElmEncoderRefWith :: ElmType a => Options -> a -> Text
toElmEncoderRefWith options x = runReader (renderRef (toElmType x)) options

toElmEncoderRef :: ElmType a => a -> Text
toElmEncoderRef = toElmEncoderRefWith defaultOptions

toElmEncoderSourceWith :: ElmType a => Options -> a -> Text
toElmEncoderSourceWith options x = runReader (render (toElmType x)) options

toElmEncoderSource :: ElmType a => a -> Text
toElmEncoderSource = toElmEncoderSourceWith defaultOptions
