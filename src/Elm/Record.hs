{-# LANGUAGE OverloadedStrings #-}

module Elm.Record
  ( toElmTypeRef
  , toElmTypeRefWith
  , toElmTypeSource
  , toElmTypeSourceWith
  ) where

import Control.Monad.Reader
import Data.Monoid
import qualified Data.Text as T
import Elm.Common
import Elm.Type
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

class HasType a where
  render :: a -> Reader Options Doc

class HasTypeRef a where
  renderRef :: a -> Reader Options Doc

instance HasType ElmDatatype where
  render d@(ElmDatatype _ constructor@(RecordConstructor _ _)) = do
    name <- renderRef d
    ctor <- render constructor
    return . nest 4 $ "type alias" <+> name <+> "=" <$$> ctor
  render d@(ElmDatatype _ constructor@(MultipleConstructors _)) = do
    name <- renderRef d
    ctor <- render constructor
    return . nest 4 $ "type" <+> name <$$> "=" <+> ctor
  render d@(ElmDatatype _ constructor@(NamedConstructor _ _)) = do
    name <- renderRef d
    ctor <- render constructor
    return . nest 4 $ "type" <+> name <$$> "=" <+> ctor
  render (ElmPrimitive primitive) = renderRef primitive

instance HasTypeRef ElmDatatype where
  renderRef (ElmDatatype typeName _) = pure (stext typeName)
  renderRef (ElmPrimitive primitive) = renderRef primitive

instance HasType ElmConstructor where
  render (RecordConstructor _ value) = do
    dv <- render value
    return $ "{" <+> dv <$$> "}"
  render (NamedConstructor constructorName value) = do
    dv <- render value
    return $ stext constructorName <+> dv
  render (MultipleConstructors constructors) =
    mintercalate (line <> "|" <> space) <$> sequence (render <$> constructors)

instance HasType ElmValue where
  render (ElmRef name) = pure (stext name)
  render ElmEmpty = pure (text "")
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> comma <+> dy
  render (ElmPrimitiveRef primitive) = renderRef primitive
  render (ElmField name value) = do
    fieldModifier <- asks fieldLabelModifier
    dv <- render value
    return $ stext (fieldModifier name) <+> ":" <+> dv

instance HasTypeRef ElmPrimitive where
  renderRef (EList (ElmPrimitive EChar)) = renderRef EString
  renderRef (EList datatype) = do
    dt <- renderRef datatype
    return $ "List" <+> parens dt
  renderRef (ETuple2 x y) = do
    dx <- render x
    dy <- render y
    return . spaceparens $ dx <> comma <+> dy
  renderRef (EMaybe datatype) = do
    dt <- renderRef datatype
    return $ "Maybe" <+> parens dt
  renderRef (EDict k v) = do
    dk <- renderRef k
    dv <- renderRef v
    return $ "Dict" <+> parens dk <+> parens dv
  renderRef EInt = pure "Int"
  renderRef EDate = pure "Date"
  renderRef EBool = pure "Bool"
  renderRef EChar = pure "Char"
  renderRef EString = pure "String"
  renderRef EUnit = pure "()"
  renderRef EFloat = pure "Float"

toElmTypeRefWith
  :: ElmType a
  => Options -> a -> T.Text
toElmTypeRefWith options x =
  pprinter $ runReader (renderRef (toElmType x)) options

toElmTypeRef
  :: ElmType a
  => a -> T.Text
toElmTypeRef = toElmTypeRefWith defaultOptions

toElmTypeSourceWith
  :: ElmType a
  => Options -> a -> T.Text
toElmTypeSourceWith options x =
  pprinter $ runReader (render (toElmType x)) options

toElmTypeSource
  :: ElmType a
  => a -> T.Text
toElmTypeSource = toElmTypeSourceWith defaultOptions
