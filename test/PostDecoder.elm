module PostDecoder exposing (..)

import CommentDecoder exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import PostType exposing (..)


decodePost : Decoder Post
decodePost =
    decode Post
        |> required "id" int
        |> required "name" string
        |> required "age" (maybe float)
        |> required "comments" (list decodeComment)
        |> required "promoted" (maybe decodeComment)
        |> required "author" (maybe string)
