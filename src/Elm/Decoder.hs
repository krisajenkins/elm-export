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
    return $ "Json.Decode.succeed" <+> stext name <$$> indent 4 dv
  render (RecordConstructor name value) = do
    dv <- render value
    return $ "Json.Decode.succeed" <+> stext name <$$> indent 4 dv

  render mc@(MultipleConstructors constrs) = do
      cstrs <- mapM renderSum constrs
      pure $ constructorName <$$> indent 4
        ("|> andThen" <$$>
          indent 4 (newlineparens ("\\x ->" <$$>
            (indent 4 $ "case x of" <$$>
              (indent 4 $ foldl1 (<$+$>) cstrs <$+$>
               "_ ->" <$$> indent 4 "fail \"Constructor not matched\""
              )
            )
          ))
        )
    where
      constructorName :: Doc
      constructorName =
        if isEnumeration mc then "string" else "field \"tag\" string"

-- | required "contents"
requiredContents :: Doc
requiredContents = "required" <+> dquotes "contents"

-- | "<name>" -> Json.Decode.succeed <name>
renderSumCondition :: T.Text -> Doc -> RenderM Doc
renderSumCondition name contents =
  pure $ dquotes (stext name) <+> "->" <$$>
    indent 4
      ("Json.Decode.succeed" <+> stext name <$$> indent 4 contents)

-- | Render a sum type constructor in context of a data type with multiple
-- constructors.
renderSum :: ElmConstructor -> RenderM Doc
renderSum (NamedConstructor name ElmEmpty) = renderSumCondition name mempty
renderSum (NamedConstructor name v@(Values _ _)) = do
  (_, val) <- renderConstructorArgs 0 v
  renderSumCondition name val
renderSum (NamedConstructor name value) = do
  val <- render value
  renderSumCondition name $ "|>" <+> requiredContents <+> val
renderSum (RecordConstructor name value) = do
  val <- render value
  renderSumCondition name val
renderSum (MultipleConstructors constrs) =
  foldl1 (<$+$>) <$> mapM renderSum constrs

-- | Render the decoding of a constructor's arguments. Note the constructor must
-- be from a data type with multiple constructors and that it has multiple
-- constructors itself.
renderConstructorArgs :: Int -> ElmValue -> RenderM (Int, Doc)
renderConstructorArgs i (Values l r) = do
  (iL, rndrL) <- renderConstructorArgs i l
  (iR, rndrR) <- renderConstructorArgs (iL + 1) r
  pure (iR, rndrL <$$> rndrR)
renderConstructorArgs i val = do
  rndrVal <- render val
  let index = parens $ "index" <+> int i <+> rndrVal
  pure (i, "|>" <+> requiredContents <+> index)

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
  render ElmEmpty = pure (stext "")

instance HasDecoderRef ElmPrimitive where
  renderRef (EList (ElmPrimitive EChar)) = pure "string"
  renderRef (EList datatype) = do
    dt <- renderRef datatype
    return . parens $ "list" <+> dt
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
  renderRef EDate = do
    require "Iso8601"
    pure "Iso8601.decoder"
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
