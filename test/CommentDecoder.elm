module CommentDecoder exposing (..)

import CommentType exposing (..)
import Date
import Dict
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeComment : Decoder Comment
decodeComment =
    decode Comment
        |> required "postId" int
        |> required "text" string
        |> required "mainCategories" (tuple2 (,) string string)
        |> required "published" bool
        |> required "created" (customDecoder string Date.fromString)
        |> required "tags" (map Dict.fromList (list (tuple2 (,) string int)))
