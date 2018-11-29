module CommentDecoder exposing (..)

import CommentType exposing (..)
import Dict
import Iso8601
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeComment : Decoder Comment
decodeComment =
    succeed Comment
        |> required "postId" int
        |> required "text" string
        |> required "mainCategories" (map2 Tuple.pair (index 0 string) (index 1 string))
        |> required "published" bool
        |> required "created" Iso8601.decoder
        |> required "tags" (dict int)
