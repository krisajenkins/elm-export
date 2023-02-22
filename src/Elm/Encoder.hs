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
  renderRef :: Int -> a -> RenderM Doc

instance HasEncoder ElmDatatype where
  render d@(ElmDatatype name constructor) = do
    fnName <- renderRef 0 d
    ctor <- render constructor
    return $
      (fnName <+> ":" <+> stext name <+> "->" <+> "Json.Encode.Value") <$$>
      (fnName <+> "x =" <$$> indent 4 ctor)
  render (ElmPrimitive primitive) = renderRef 0 primitive

instance HasEncoderRef ElmDatatype where
  renderRef _ (ElmDatatype name _) = pure $ "encode" <> stext name
  renderRef level (ElmPrimitive primitive) = renderRef level primitive

instance HasEncoder ElmConstructor where
  -- Single constructor, no values: empty array
  render (NamedConstructor _name ElmEmpty) =
    return $ "Json.Encode.list identity []"

  render (NamedConstructor _name (ElmPrimitiveRef EUnit)) =
    return $ "Json.Encode.list identity []"

  -- Single constructor, multiple values: create array with values
  render (NamedConstructor name value@(Values _ _)) = do
    let ps = constructorParameters 0 value

    (dv, _) <- renderVariable ps value

    let cs = stext name <+> foldl1 (<+>) ps <+> "->"
    return . nest 4 $ "case x of" <$$>
      (nest 4 $ cs <$$> nest 4 ("Json.Encode.list identity" <$$> "[" <+> dv <$$> "]"))

  -- Single constructor, one value: skip constructor and r just the value
  render (NamedConstructor name value) = do
    dv <- render value

    let cs = stext name <+> "y0 ->"
    return . nest 4 $ "case x of" <$$>
      nest 4 (cs <$$> nest 4 dv <+> "y0")


  render (RecordConstructor _ value) = do
    dv <- render value
    return . nest 4 $ "Json.Encode.object" <$$> "[" <+> dv <$$> "]"

  render mc@(MultipleConstructors constrs) = do
    let rndr = if isEnumeration mc then renderEnumeration else renderSum
    dc <- mapM rndr constrs
    return . nest 4 $ "case x of" <$$> foldl1 (<$+$>) dc

jsonEncodeObject :: Doc -> Doc -> Doc -> Doc
jsonEncodeObject constructor tag contents =
  nest 4 $ constructor <$$>
    nest 4 ("Json.Encode.object" <$$> "[" <+> tag <$$>
      contents <$$>
    "]")

renderSum :: ElmConstructor -> RenderM Doc
renderSum c@(NamedConstructor name ElmEmpty) = do
  dc <- render c
  let cs = stext name <+> "->"
  let tag = pair (dquotes "tag") ("Json.Encode.string" <+> dquotes (stext name))
  let ct = comma <+> pair (dquotes "contents") dc

  return $ jsonEncodeObject cs tag ct

renderSum (NamedConstructor name value) = do
  let ps = constructorParameters 0 value

  (dc, _) <- renderVariable ps value
  let dc' = if length ps > 1 then "Json.Encode.list identity" <+> squarebracks dc else dc
  let cs = stext name <+> foldl1 (<+>) ps <+> "->"
  let tag = pair (dquotes "tag") ("Json.Encode.string" <+> dquotes (stext name))
  let ct = comma <+> pair (dquotes "contents") dc'

  return $ jsonEncodeObject cs tag ct

renderSum (RecordConstructor name value) = do
  dv <- render value
  let cs = stext name <+> "->"
  let tag = pair (dquotes "tag") (dquotes $ stext name)
  let ct = comma <+> dv
  return $ jsonEncodeObject cs tag ct

renderSum (MultipleConstructors constrs) = do
  dc <- mapM renderSum constrs
  return $ foldl1 (<$+$>) dc


renderEnumeration :: ElmConstructor -> RenderM Doc
renderEnumeration (NamedConstructor name _) =
  return . nest 4 $ stext name <+> "->" <$$>
      "Json.Encode.string" <+> dquotes (stext name)
renderEnumeration (MultipleConstructors constrs) = do
  dc <- mapM renderEnumeration constrs
  return $ foldl1 (<$+$>) dc
renderEnumeration c = render c


instance HasEncoder ElmValue where
  render (ElmField name value) = do
    fieldModifier <- asks fieldLabelModifier
    valueBody <- render value
    return . spaceparens $
      dquotes (stext (fieldModifier name)) <> comma <+>
      (valueBody <+> "x." <> stext (fieldModifier name))
  render (ElmPrimitiveRef primitive) = renderRef 0 primitive
  render (ElmRef name) = pure $ "encode" <> stext name
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> comma <+> dy
  render _ = error "HasEncoderRef ElmValue: should not happen"

instance HasEncoderRef ElmPrimitive where
  renderRef _ ETimePosix = pure $ parens "Iso8601.encode"
  renderRef _ EUnit = pure "Json.Encode.null"
  renderRef _ EInt = pure "Json.Encode.int"
  renderRef _ EChar = pure "Json.Encode.char"
  renderRef _ EBool = pure "Json.Encode.bool"
  renderRef _ EFloat = pure "Json.Encode.float"
  renderRef _ EString = pure "Json.Encode.string"
  renderRef _ EJsonValue = pure "identity"
  renderRef _ (EList (ElmPrimitive EChar)) = pure "Json.Encode.string"
  renderRef level (EList datatype) = do
    dd <- renderRef level datatype
    return . parens $ "Json.Encode.list" <+> dd
  renderRef level (EMaybe datatype) = do
    dd <- renderRef level datatype
    return . parens $ "Maybe.withDefault Json.Encode.null << Maybe.map" <+> dd
  renderRef level (ETuple2 x y) = do
    dx <- renderRef (level + 1) x
    dy <- renderRef (level + 1) y
    let firstName = "m" <> int level
    let secondName = "n" <> int level
    return . parens $ "\\("<> firstName <> "," <+> secondName <> ") -> Json.Encode.list identity [" <+> dx <+> firstName <> "," <+> dy <+> secondName <+> "]"
  renderRef level (EDict k v) = do
    dk <- renderRef level k
    dv <- renderRef level v
    return . parens $ "Json.Encode.dict" <+> dk <+> dv

toElmEncoderRefWith
  :: ElmType a
  => Options -> a -> T.Text
toElmEncoderRefWith options x =
  pprinter . fst $ evalRWS (renderRef 0 (toElmType x)) options ()

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

-- | Variable names for the members of constructors
-- Used in pattern matches
constructorParameters :: Int -> ElmValue -> [Doc]
constructorParameters _ ElmEmpty = [ empty ]
constructorParameters i (Values l r) =
    left ++ right
  where
    left = constructorParameters i l
    right = constructorParameters (length left + i) r
constructorParameters i _ = [ "y" <> int i ]


-- | Encode variables following the recipe of an ElmValue
renderVariable :: [Doc] -> ElmValue -> RenderM (Doc, [Doc])
renderVariable (d : ds) v@(ElmRef {}) = do
  v' <- render v
  return (v' <+> d, ds)
renderVariable ds ElmEmpty = return (empty, ds)
renderVariable (_ : ds) (ElmPrimitiveRef EUnit) =
  return ("Json.Encode.null", ds)
renderVariable (d : ds) (ElmPrimitiveRef ref) = do
  r <- renderRef 0 ref
  return (r <+> d, ds)
renderVariable ds (Values l r) = do
  (left, dsl) <- renderVariable ds l
  (right, dsr) <- renderVariable dsl r
  return (left <> comma <+> right, dsr)
renderVariable ds f@(ElmField _ _) = do
  f' <- render f
  return (f', ds)
renderVariable [] _ = error "Amount of variables does not match variables"
