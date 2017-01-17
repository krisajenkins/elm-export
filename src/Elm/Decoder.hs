{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Elm.Decoder
  ( toElmDecoderRef
  , toElmDecoderRefWith
  , toElmDecoderSource
  , toElmDecoderSourceWith
  ) where

import Control.Monad.Reader
import Data.Monoid
import qualified Data.Text as T
import Elm.Common
import Elm.Type
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

class HasDecoder a where
  render :: a -> Reader Options Doc

class HasDecoderRef a where
  renderRef :: a -> Reader Options Doc

instance HasDecoder ElmDatatype where
  render d@(ElmDatatype name constructor) = do
    fnName <- renderRef d
    ctor <- render constructor
    return $
      (fnName <+> ": Decoder" <+> stext name) <$$>
      (fnName <+> "=" <$$> indent 4 ctor)
  render (ElmPrimitive primitive) = renderRef primitive

instance HasDecoderRef ElmDatatype where
  renderRef (ElmDatatype name _) = pure $ "decode" <> stext name
  renderRef (ElmPrimitive primitive) = renderRef primitive

instance HasDecoder ElmConstructor where
  render (NamedConstructor name value) = do
    dv <- render value
    return $ "decode" <+> stext name <$$> indent 4 dv
  render (RecordConstructor name value) = do
    dv <- render value
    return $ "decode" <+> stext name <$$> indent 4 dv

instance HasDecoder ElmValue where
  render (ElmRef name) = pure $ "decode" <> stext name
  render (ElmPrimitiveRef primitive) = renderRef primitive
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> dy
  render (ElmField name value) = do
    fieldModifier <- asks fieldLabelModifier
    dv <- render value
    return $ "|> required" <+> dquotes (stext (fieldModifier name)) <+> dv

instance HasDecoderRef ElmPrimitive where
  renderRef (EList (ElmPrimitive EChar)) = pure "string"
  renderRef (EList datatype) = do
    dt <- renderRef datatype
    return . parens $ "list" <+> dt
  renderRef (EDict key value) = do
    d <- renderRef (EList (ElmPrimitive (ETuple2 (ElmPrimitive key) value)))
    return . parens $ "map Dict.fromList" <+> d
  renderRef (EMaybe datatype) = do
    dt <- renderRef datatype
    return . parens $ "maybe" <+> dt
  renderRef (ETuple2 x y) = do
    dx <- renderRef x
    dy <- renderRef y
    return . parens $ "tuple2 (,)" <+> dx <+> dy
  renderRef EUnit = pure $ parens "succeed ()"
  renderRef EDate = pure $ parens "customDecoder string Date.fromString"
  renderRef EInt = pure "int"
  renderRef EBool = pure "bool"
  renderRef EChar = pure "char"
  renderRef EFloat = pure "float"
  renderRef EString = pure "string"

toElmDecoderRefWith
  :: ElmType a
  => Options -> a -> T.Text
toElmDecoderRefWith options x = pprinter $ runReader (renderRef (toElmType x)) options

toElmDecoderRef
  :: ElmType a
  => a -> T.Text
toElmDecoderRef = toElmDecoderRefWith defaultOptions

toElmDecoderSourceWith
  :: ElmType a
  => Options -> a -> T.Text
toElmDecoderSourceWith options x = pprinter $ runReader (render (toElmType x)) options

toElmDecoderSource
  :: ElmType a
  => a -> T.Text
toElmDecoderSource = toElmDecoderSourceWith defaultOptions
