{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Elm.Decoder
  ( toElmDecoderRef
  , toElmDecoderRefWith
  , toElmDecoderSource
  , toElmDecoderSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Text
import           Elm.Common
import           Elm.Type
import           Formatting

class HasDecoder a where
  render :: a -> Reader Options Text

class HasDecoderRef a where
  renderRef :: a -> Reader Options Text

instance HasDecoder ElmDatatype where
    render d@(ElmDatatype name constructor) = do
        fnName <- renderRef d
        sformat
            (stext % " : Decoder " % stext % cr % stext % " =" % cr % stext)
            fnName
            name
            fnName <$>
            render constructor
    render (ElmPrimitive primitive) = renderRef primitive

instance HasDecoderRef ElmDatatype where
    renderRef (ElmDatatype name _) =
        pure $ sformat ("decode" % stext) name

    renderRef (ElmPrimitive primitive) =
        renderRef primitive


instance HasDecoder ElmConstructor where
    render (NamedConstructor name value) =
        sformat ("    decode " % stext % cr % stext) name <$> render value
    render (RecordConstructor name value) =
        sformat ("    decode " % stext % cr % stext) name <$> render value


instance HasDecoder ElmValue where
    render (ElmRef name) = pure (sformat ("decode" % stext) name)
    render (ElmPrimitiveRef primitive) = renderRef primitive
    render (Values x y) = sformat (stext % cr % stext) <$> render x <*> render y
    render (ElmField name value) = do
        fieldModifier <- asks fieldLabelModifier
        sformat
            ("        |> required \"" % stext % "\" " % stext)
            (fieldModifier name) <$>
            render value


instance HasDecoderRef ElmPrimitive where
    renderRef (EList (ElmPrimitive EChar)) = pure "string"
    renderRef (EList datatype) =
        sformat ("(list " % stext % ")") <$> renderRef datatype
    renderRef (EDict key value) =
        sformat ("(map Dict.fromList " % stext % ")") <$>
        renderRef (EList (ElmPrimitive (ETuple2 (ElmPrimitive key) value)))
    renderRef (EMaybe datatype) =
        sformat ("(maybe " % stext % ")") <$> renderRef datatype
    renderRef (ETuple2 x y) =
        sformat ("(tuple2 (,) " % stext % " " % stext % ")") <$> renderRef x <*>
        renderRef y
    renderRef EUnit = pure "(succeed ())"
    renderRef EDate = pure "(customDecoder string Date.fromString)"
    renderRef EInt = pure "int"
    renderRef EBool = pure "bool"
    renderRef EChar = pure "char"
    renderRef EFloat = pure "float"
    renderRef EString = pure "string"


toElmDecoderRefWith :: ElmType a => Options -> a -> Text
toElmDecoderRefWith options x = runReader (renderRef (toElmType x)) options


toElmDecoderRef :: ElmType a => a -> Text
toElmDecoderRef = toElmDecoderRefWith defaultOptions


toElmDecoderSourceWith :: ElmType a => Options -> a -> Text
toElmDecoderSourceWith options x = runReader (render (toElmType x)) options


toElmDecoderSource :: ElmType a => a -> Text
toElmDecoderSource = toElmDecoderSourceWith defaultOptions
