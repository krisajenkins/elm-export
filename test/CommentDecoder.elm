module CommentDecoder exposing (..)

import CommentType exposing (..)
import Dict
import Exts.Json.Decode exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeComment : Decoder Comment
decodeComment =
    decode Comment
        |> required "postId" int
        |> required "text" string
        |> required "mainCategories" (map2 (,) (index 0 string) (index 1 string))
        |> required "published" bool
        |> required "created" decodeDate
        |> required "tags" (dict int)
