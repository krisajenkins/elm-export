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
  render d@(ElmDatatype _ constructor@(RecordConstructor _ _)) =
    template <$> renderRef d <*> render constructor
    where
      template name ctor = nest 4 $ "type alias" <+> name <+> "=" <$$> ctor
  render d@(ElmDatatype _ constructor@(MultipleConstructors _)) =
    template <$> renderRef d <*> render constructor
    where
      template name ctor = nest 4 $ "type" <+> name <$$> "=" <+> ctor
  render d@(ElmDatatype _ constructor@(NamedConstructor _ _)) =
    template <$> renderRef d <*> render constructor
    where
      template name ctor = nest 4 $ "type" <+> name <$$> "=" <+> ctor
  render (ElmPrimitive primitive) = renderRef primitive

instance HasTypeRef ElmDatatype where
  renderRef (ElmDatatype typeName _) = pure (stext typeName)
  renderRef (ElmPrimitive primitive) = renderRef primitive

instance HasType ElmConstructor where
  render (RecordConstructor _ value) = template <$> render value
    where
      template v = "{" <+> v <$$> "}"
  render (NamedConstructor constructorName value) = template <$> render value
    where
      template v = stext constructorName <+> v
  render (MultipleConstructors constructors) =
    mintercalate (line <> "|" <> space) <$> sequence (render <$> constructors)

instance HasType ElmValue where
  render (ElmRef name) = pure (stext name)
  render ElmEmpty = pure (text "")
  render (Values x y) = template <$> render x <*> render y
    where
      template dx dy = dx <$$> comma <+> dy
  render (ElmPrimitiveRef primitive) = renderRef primitive
  render (ElmField name value) = do
    fieldModifier <- asks fieldLabelModifier
    let template v = stext (fieldModifier name) <+> ":" <+> v
    template <$> render value

instance HasTypeRef ElmPrimitive where
  renderRef (EList (ElmPrimitive EChar)) = renderRef EString
  renderRef (EList datatype) = template <$> renderRef datatype
    where
      template dv = "List" <+> parens dv
  renderRef (ETuple2 x y) = template <$> renderRef x <*> renderRef y
    where
      template dx dy = "(" <+> dx <> comma <+> dy <+> ")"
  renderRef (EMaybe datatype) = template <$> renderRef datatype
    where
      template dv = "Maybe" <+> parens dv
  renderRef (EDict k v) = template <$> renderRef k <*> renderRef v
    where
      template dk dv = "Dict" <+> parens dk <+> parens dv
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
