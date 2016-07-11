module Elm.Encoder (toElmEncoderSource, toElmEncoderSourceWith)
       where

import           Control.Monad.Reader
import           Elm.Common
import           Elm.Type
import           Text.Printf

render :: ElmTypeExpr -> Reader Options String

render (TopLevel (DataType d t)) =
  printf "%s : %s -> Value\n%s x =%s" fnName d fnName <$> render t
  where fnName = "encode" ++ d

render (DataType d _) = return $ "encode" ++ d

render (Record _ t) =
  printf "\n    object\n        [ %s\n        ]" <$> render t

render (Product (Primitive "List") (Primitive "Char")) =
  render (Primitive "String")

render (Product (Primitive "List") t) =
  printf "(list << List.map %s)" <$> render t

render (Product (Primitive "Maybe") t) =
  printf "(Maybe.withDefault null << Maybe.map %s)" <$> render t

render (Tuple2 x y) =
  do bodyX <- render x
     bodyY <- render y
     return $ printf "tuple2 %s %s" bodyX bodyY

render (Dict x y) =
  do bodyX <- render x
     bodyY <- render y
     return $ printf "dict %s %s" bodyX bodyY

render (Product x y) =
  do bodyX <- render x
     bodyY <- render y
     return $ printf "%s\n        , %s" bodyX bodyY

render (Selector n t) =
  do fieldModifier <- asks fieldLabelModifier
     typeBody <- render t
     return $ printf "( \"%s\", %s x.%s )" (fieldModifier n) typeBody n

render  (Primitive "String") = return "string"
render  (Primitive "Int") = return "int"
render  (Primitive "Double") = return "float"
render  (Primitive "Float") = return "float"
render  (Primitive "Date") = return "(string << toISOString)"
render  (Primitive "Bool") = return "bool"
render  (Field t) = render t

toElmEncoderSourceWith :: ElmType a => Options -> a -> String
toElmEncoderSourceWith options x = runReader (render . TopLevel $ toElmType x) options

toElmEncoderSource :: ElmType a => a -> String
toElmEncoderSource = toElmEncoderSourceWith defaultOptions
