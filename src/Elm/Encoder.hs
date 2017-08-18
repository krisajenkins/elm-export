{-# LANGUAGE OverloadedStrings #-}

module Elm.Encoder
  ( toElmEncoderRef
  , toElmEncoderRefWith
  , toElmEncoderSource
  , toElmEncoderSourceWith
  , renderEncoder
  ) where

import Control.Monad.RWS
import qualified Data.Text as T
import Elm.Common
import Elm.Type
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

class HasEncoder a where
  render :: a -> RenderM Doc

class HasEncoderRef a where
  renderRef :: a -> RenderM Doc

instance HasEncoder ElmDatatype where
  render d@(ElmDatatype name constructor) = do
    fnName <- renderRef d
    ctor <- render constructor
    return $
      (fnName <+> ":" <+> stext name <+> "->" <+> "Json.Encode.Value") <$$>
      (fnName <+> "x =" <$$> indent 4 ctor)
  render (ElmPrimitive primitive) = renderRef primitive

instance HasEncoderRef ElmDatatype where
  renderRef (ElmDatatype name _) = pure $ "encode" <> stext name
  renderRef (ElmPrimitive primitive) = renderRef primitive

instance HasEncoder ElmConstructor where
  render (RecordConstructor _ value) = do
    dv <- render value
    return . nest 4 $ "Json.Encode.object" <$$> "[" <+> dv <$$> "]"

instance HasEncoder ElmValue where
  render (ElmField name value) = do
    fieldModifier <- asks fieldLabelModifier
    valueBody <- render value
    return . spaceparens $
      dquotes (stext (fieldModifier name)) <> comma <+>
      (valueBody <+> "x." <> stext name)
  render (ElmPrimitiveRef primitive) = renderRef primitive
  render (ElmRef name) = pure $ "encode" <> stext name
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> comma <+> dy

instance HasEncoderRef ElmPrimitive where
  renderRef EDate = pure $ parens "Json.Encode.string << toString"
  renderRef EUnit = pure "Json.Encode.null"
  renderRef EInt = pure "Json.Encode.int"
  renderRef EChar = pure "Json.Encode.char"
  renderRef EBool = pure "Json.Encode.bool"
  renderRef EFloat = pure "Json.Encode.float"
  renderRef EString = pure "Json.Encode.string"
  renderRef (EList (ElmPrimitive EChar)) = pure "Json.Encode.string"
  renderRef (EList datatype) = do
    dd <- renderRef datatype
    return . parens $ "Json.Encode.list << List.map" <+> dd
  renderRef (EMaybe datatype) = do
    dd <- renderRef datatype
    return . parens $ "Maybe.withDefault Json.Encode.null << Maybe.map" <+> dd
  renderRef (ETuple2 x y) = do
    dx <- renderRef x
    dy <- renderRef y
    require "Exts.Json.Encode"
    return . parens $ "Exts.Json.Encode.tuple2" <+> dx <+> dy
  renderRef (EMap _ v) = do
    dv <- renderRef v
    require "Exts.Json.Encode"
    return . parens $ "Dict.toList >> (\\ (k,v) -> (toString k, (" <+> dv <+> ") v)) >> Json.encode.object"
  renderRef (EDict k v) = do
    dk <- renderRef k
    dv <- renderRef v
    require "Exts.Json.Encode"
    return . parens $ "Exts.Json.Encode.dict" <+> dk <+> dv

toElmEncoderRefWith
  :: ElmType a
  => Options -> a -> T.Text
toElmEncoderRefWith options x =
  pprinter . fst $ evalRWS (renderRef (toElmType x)) options ()

toElmEncoderRef
  :: ElmType a
  => a -> T.Text
toElmEncoderRef = toElmEncoderRefWith defaultOptions

toElmEncoderSourceWith
  :: ElmType a
  => Options -> a -> T.Text
toElmEncoderSourceWith options x =
  pprinter . fst $ evalRWS (render (toElmType x)) options ()

toElmEncoderSource
  :: ElmType a
  => a -> T.Text
toElmEncoderSource = toElmEncoderSourceWith defaultOptions

renderEncoder
  :: ElmType a
  => a -> RenderM ()
renderEncoder x = do
  require "Json.Encode"
  collectDeclaration . render . toElmType $ x
