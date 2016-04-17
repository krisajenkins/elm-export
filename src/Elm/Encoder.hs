module Elm.Encoder (toElmEncoderSource, toElmEncoderSourceWith
                   ,toElmEncoderSourceDefs, toElmEncoderSourceDefsWith)
       where

import           Control.Monad.Reader
import           Data.List   (nub)
import           Elm.Common
import           Elm.Type
import           Text.Printf

render :: ElmTypeExpr -> Reader Options String

render (TopLevel (DataType d t)) =
  printf "%s : %s -> Json.Encode.Value\n%s x =%s" fnName d fnName <$> render t
  where fnName = "encode" ++ d

render (DataType d _) = return $ "encode" ++ d

render (Record _ t) =
  printf "\n  Json.Encode.object\n    [ %s\n    ]" <$> render t

render (Product (Primitive "List") (Primitive "Char")) =
  render (Primitive "String")

render (Product (Primitive "List") t) =
  printf "(Json.Encode.list << List.map %s)" <$> render t

render (Product (Primitive "Maybe") t) =
  printf "(Maybe.withDefault Json.Encode.null << Maybe.map %s)" <$> render t

render (Tuple2 x y) =
  do bodyX <- render x
     bodyY <- render y
     return $ printf "Exts.Json.Encode.tuple2 %s %s" bodyX bodyY

render (Dict x y) =
  do bodyX <- render x
     bodyY <- render y
     return $ printf "Exts.Json.Encode.dict %s %s" bodyX bodyY

render (Product x y) =
  do bodyX <- render x
     bodyY <- render y
     return $ printf "%s\n    , %s" bodyX bodyY

render (Selector n t) =
  do fieldModifier <- asks fieldLabelModifier
     typeBody <- render t
     return $ printf "( \"%s\", %s x.%s )" (fieldModifier n) typeBody n

render  (Primitive "String") = return "Json.Encode.string"
render  (Primitive "Int") = return "Json.Encode.int"
render  (Primitive "Double") = return "Json.Encode.float"
render  (Primitive "Float") = return "Json.Encode.float"
render  (Primitive "Date") = return "(Json.Encode.string << Exts.Date.toISOString)"
render  (Primitive "Bool") = return "Json.Encode.bool"
render  (Field t) = render t

toElmEncoderSourceWith :: ElmType a => Options -> a -> String
toElmEncoderSourceWith options x = runReader (render . TopLevel $ toElmType x) options

toElmEncoderSource :: ElmType a => a -> String
toElmEncoderSource = toElmEncoderSourceWith defaultOptions

---------------------------------

renderWithDefs :: ElmTypeExpr -> Reader Options (String, [String])

renderWithDefs t@(DataType _ s) =
  do
    tEncoder   <- render t
    tDef       <- render (TopLevel t)
    (_, sDefs) <- renderWithDefs s
    return ( tEncoder
           , tDef : sDefs)

renderWithDefs (Product (Primitive "Maybe") t) =
  do
    (tEncoder, tDefs) <- renderWithDefs t
    return ( printf "(\\y -> case y of Nothing -> JS.null; Just val -> %s val)" (parenthesize t tEncoder)
           , tDefs)

renderWithDefs t@(Product (Primitive "List") (Primitive "Char")) =
  do
    tEncoder <- render (Field t)
    return (tEncoder, [])

renderWithDefs (Product (Primitive "List") t) =
  do
    (tEncoder, tDefs) <- renderWithDefs t
    return ( printf "(JS.list << List.map %s)" (parenthesize t tEncoder)
           , tDefs)

renderWithDefs (Product x y) =
  do
    (_, xDefs) <- renderWithDefs x
    (_, yDefs) <- renderWithDefs y
    return ( undefined -- this case is used only for generating definitions (called from DataType match above)
           , xDefs ++ yDefs )

renderWithDefs Unit = return ("JS.null", [])

renderWithDefs t@(Primitive _) =
  do
    tEncoder <- render t
    return (tEncoder, [])

renderWithDefs (Record _ t) = renderWithDefs t

renderWithDefs (Selector _ t) = renderWithDefs t

renderWithDefs (Field t) = renderWithDefs t

renderWithDefs x = return (printf "<%s>" (show x), [])

toElmEncoderSourceDefsWith :: ElmType a => Options -> a -> (String, [String])
toElmEncoderSourceDefsWith options x =
  nub <$> runReader (renderWithDefs (toElmType x)) options

toElmEncoderSourceDefs :: ElmType a => a -> (String, [String])
toElmEncoderSourceDefs = toElmEncoderSourceDefsWith defaultOptions
