{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elm.Record
  ( toElmSorterSource,
    toElmTypeRef,
    toElmTypeRefWith,
    toElmTypeSource,
    toElmTypeSourceWith,
    renderType,
  )
where

import Control.Monad.RWS
import Data.Proxy
import qualified Data.Text as T
import Elm.Common
import Elm.Sorter (HasElmSorter (..))
import qualified Elm.Sorter as Sorter
import Elm.Type
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

class HasType a where
  render :: a -> RenderM Doc

class HasRecordType a where
  renderRecord :: a -> RenderM Doc

class HasTypeRef a where
  renderRef :: a -> RenderM Doc

instance HasType ElmDatatype where
  render d@(ElmDatatype _ constructor@(RecordConstructor _ _)) = do
    name <- renderRef d
    ctor <- render constructor
    return . nest 4 $ "type alias" <+> name <+> "=" <$$> ctor
  render d@(ElmDatatype _ constructor) = do
    name <- renderRef d
    ctor <- render constructor
    return . nest 4 $ "type" <+> name <$$> "=" <+> ctor
  render (ElmPrimitive primitive) = renderRef primitive
  render (CreatedInElm _) = pure $ stext ""

instance HasTypeRef ElmDatatype where
  renderRef (ElmDatatype typeName _) = pure (stext typeName)
  renderRef (ElmPrimitive primitive) = renderRef primitive
  renderRef (CreatedInElm (FromElm typeName _ _)) = pure (stext typeName)

instance HasType ElmConstructor where
  render (RecordConstructor _ value) = do
    dv <- renderRecord value
    return $ "{" <+> dv <$$> "}"
  render (NamedConstructor constructorName value) = do
    dv <- render value
    return $ stext constructorName <+> dv
  render (MultipleConstructors constructors) =
    mintercalate (line <> "|" <> space) <$> sequence (render <$> constructors)

instance HasType ElmValue where
  render (ElmRef name) = pure (stext name)
  render (ElmPrimitiveRef primitive) = elmRefParens primitive <$> renderRef primitive
  render ElmEmpty = pure (text "")
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <+> dy
  render (ElmField name value) = do
    fieldModifier <- asks fieldLabelModifier
    dv <- renderRecord value
    return $ stext (fieldModifier name) <+> ":" <+> dv

instance HasRecordType ElmValue where
  renderRecord (ElmPrimitiveRef primitive) = renderRef primitive
  renderRecord (Values x y) = do
    dx <- renderRecord x
    dy <- renderRecord y
    return $ dx <$$> comma <+> dy
  renderRecord value = render value

instance HasTypeRef ElmPrimitive where
  renderRef (EList (ElmPrimitive EChar)) = renderRef EString
  renderRef (EList datatype) = do
    dt <- renderRef datatype
    return $ "List" <+> parens dt
  renderRef (ETuple2 x y) = do
    dx <- renderRef x
    dy <- renderRef y
    return . parens $ dx <> comma <+> dy
  renderRef (EMaybe datatype) = do
    dt <- renderRef datatype
    return $ "Maybe" <+> parens dt
  renderRef (EDict k v) = do
    require "Dict"
    dk <- renderRef k
    dv <- renderRef v
    return $ "Dict" <+> parens dk <+> parens dv
  renderRef (ESet datatype) = do
    require "Set"
    dt <- renderRef datatype
    return $ "Set" <+> parens dt
  renderRef EInt = pure "Int"
  renderRef ETimePosix = do
    require "Time"
    pure "Time.Posix"
  renderRef EBool = pure "Bool"
  renderRef EChar = pure "Char"
  renderRef EString = pure "String"
  renderRef EUnit = pure "()"
  renderRef EFloat = pure "Float"
  renderRef EJsonValue = do
    require "Json.Decode"
    pure "Json.Decode.Value"
  renderRef (ESortDict _ _ k v) = do
    require "Sort.Dict"
    keyType <- renderRef k
    valueType <- renderRef v
    pure $ "Sort.Dict.Dict" <+> parens keyType <+> parens valueType
  renderRef (ESortSet _ v) = do
    require "Sort.Set"
    valueType <- renderRef v
    pure $ "Sort.Set.Set" <+> parens valueType

-- | Puts parentheses around the doc of an elm ref if it contains spaces.
elmRefParens :: ElmPrimitive -> Doc -> Doc
elmRefParens (EList (ElmPrimitive EChar)) = id
elmRefParens (EList _) = parens
elmRefParens (ESet _) = parens
elmRefParens (EMaybe _) = parens
elmRefParens (EDict _ _) = parens
elmRefParens (ESortDict _ _ _ _) = parens
elmRefParens (ESortSet _ _) = parens
elmRefParens _ = id

toElmTypeRefWith ::
  (ElmType a) =>
  Options ->
  a ->
  T.Text
toElmTypeRefWith options x =
  pprinter . fst $ evalRWS (renderRef (toElmType x)) options ()

toElmTypeRef ::
  (ElmType a) =>
  a ->
  T.Text
toElmTypeRef = toElmTypeRefWith defaultOptions

toElmTypeSourceWith ::
  (ElmType a) =>
  Options ->
  a ->
  T.Text
toElmTypeSourceWith options x =
  pprinter . fst $ evalRWS (render (toElmType x)) options ()

toElmSorterSource :: (ElmType a, HasElmSorter a) => Proxy a -> T.Text
toElmSorterSource p =
  pprinter $
    vsep
      [ hcat ["sorter", typeName, " : Sorter ", typeName],
        hcat ["sorter", typeName, " = ", sorter]
      ]
  where
    typeName = pretty $ toElmTypeRef p
    sorter = pretty $ Sorter.render (elmSorter p)

toElmTypeSource ::
  (ElmType a) =>
  a ->
  T.Text
toElmTypeSource = toElmTypeSourceWith defaultOptions

renderType ::
  (ElmType a) =>
  a ->
  RenderM ()
renderType = collectDeclaration . render . toElmType
