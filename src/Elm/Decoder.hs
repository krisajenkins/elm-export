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
    render (ElmPrimitive primitive) = render primitive

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
    render (ElmPrimitiveRef primitive) = render primitive
    render (Values x y) = sformat (stext % cr % stext) <$> render x <*> render y
    render (ElmField name value) = do
        fieldModifier <- asks fieldLabelModifier
        sformat
            ("        |> required \"" % stext % "\" " % stext)
            (fieldModifier name) <$>
            render value


instance HasDecoder ElmPrimitive where
    render (EList (ElmPrimitive EChar)) = pure "string"
    render (EList datatype) =
        sformat ("(list " % stext % ")") <$> renderRef datatype
    render (EDict key value) =
        sformat ("(map Dict.fromList " % stext % ")") <$>
        render (EList (ElmPrimitive (ETuple2 (ElmPrimitive key) value)))
    render (EMaybe datatype) =
        sformat ("(maybe " % stext % ")") <$> renderRef datatype
    render (ETuple2 x y) =
        sformat ("(tuple2 (,) " % stext % " " % stext % ")") <$> render x <*>
        render y
    render EUnit = pure "(succeed ())"
    render EDate = pure "(customDecoder string Date.fromString)"
    render EInt = pure "int"
    render EBool = pure "bool"
    render EChar = pure "char"
    render EFloat = pure "float"
    render EString = pure "string"

instance HasDecoderRef ElmPrimitive where
    renderRef = render


toElmDecoderRefWith :: ElmType a => Options -> a -> Text
toElmDecoderRefWith options x = runReader (renderRef (toElmType x)) options


toElmDecoderRef :: ElmType a => a -> Text
toElmDecoderRef = toElmDecoderRefWith defaultOptions


toElmDecoderSourceWith :: ElmType a => Options -> a -> Text
toElmDecoderSourceWith options x = runReader (render (toElmType x)) options


toElmDecoderSource :: ElmType a => a -> Text
toElmDecoderSource = toElmDecoderSourceWith defaultOptions
