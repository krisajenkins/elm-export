{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Elm.Decoder
  ( toElmDecoderRef
  , toElmDecoderRefWith
  , toElmDecoderSource
  , toElmDecoderSourceWith
  , renderDecoder
  ) where

import Control.Monad.RWS
import qualified Data.Text as T
import Elm.Common
import Elm.Type
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

class HasDecoder a where
  render :: a -> RenderM Doc

class HasDecoderRef a where
  renderRef :: a -> RenderM Doc

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
  renderRef (EDict EString value) = do
    require "Dict"
    d <- renderRef value
    return . parens $ "dict" <+> d
  renderRef (EDict EInt value) = do
    require "Dict"
    d <- renderRef value
    return . parens $ "dict" <+> d <+> " |> map (Dict.toList >> List.filterMap (\\( k, v ) -> String.toInt k |> Result.toMaybe |> Maybe.map (\\i -> ( i, v ))) >> Dict.fromList)"
  renderRef (EDict key value) = do
    require "Dict"
    d <- renderRef (EList (ElmPrimitive (ETuple2 (ElmPrimitive key) value)))
    return . parens $ "map Dict.fromList" <+> d
  renderRef (EMaybe datatype) = do
    dt <- renderRef datatype
    return . parens $ "nullable" <+> dt
  renderRef (ETuple2 x y) = do
    dx <- renderRef x
    dy <- renderRef y
    return . parens $
      "map2 (,)" <+> parens ("index 0" <+> dx) <+> parens ("index 1" <+> dy)
  renderRef EUnit = pure $ parens "succeed ()"
  renderRef EDate = pure "decodeDate"
  renderRef EInt = pure "int"
  renderRef EBool = pure "bool"
  renderRef EChar = pure "char"
  renderRef EFloat = pure "float"
  renderRef EString = pure "string"

toElmDecoderRefWith
  :: ElmType a
  => Options -> a -> T.Text
toElmDecoderRefWith options x =
  pprinter . fst $ evalRWS (renderRef (toElmType x)) options ()

toElmDecoderRef
  :: ElmType a
  => a -> T.Text
toElmDecoderRef = toElmDecoderRefWith defaultOptions

toElmDecoderSourceWith
  :: ElmType a
  => Options -> a -> T.Text
toElmDecoderSourceWith options x =
  pprinter . fst $ evalRWS (render (toElmType x)) options ()

toElmDecoderSource
  :: ElmType a
  => a -> T.Text
toElmDecoderSource = toElmDecoderSourceWith defaultOptions

renderDecoder
  :: ElmType a
  => a -> RenderM ()
renderDecoder x = do
  require "Json.Decode exposing (..)"
  require "Json.Decode.Pipeline exposing (..)"
  collectDeclaration . render . toElmType $ x
